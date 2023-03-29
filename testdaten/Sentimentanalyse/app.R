# Laden Sie die benötigten Pakete
if (!require("shiny")) install.packages("shiny")
if (!require("tidytext")) install.packages("tidytext")
if (!require("dplyr")) install.packages("dplyr")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("textdata")) install.packages("textdata")

library(shiny)
library(tidytext)
library(dplyr)
library(ggplot2)
library(textdata)

# Laden Sie den Datensatz
textdata <- base::readRDS(url("https://slcladal.github.io/data/sotu_paragraphs.rda", "rb"))

# UI-Definition
ui <- fluidPage(
  titlePanel("Sentimentanalyse von Reden amerikanischer Präsidenten"),
  sidebarLayout(
    sidebarPanel(
      selectInput("president", "Wählen Sie einen Präsidenten:",
                  choices = unique(textdata$president),
                  selected = "George Washington")
    ),
    mainPanel(
      plotOutput("sentiment_plot")
    )
  )
)

# Server-Definition
server <- function(input, output) {
  
  output$sentiment_plot <- renderPlot({
    
    # Filtern Sie den Datensatz basierend auf der ausgewählten Präsidenten
    selected_president_data <- textdata %>%
      filter(president == input$president) %>%
      mutate(year = as.numeric(substr(date, 1, 4))) # Extrahieren Sie die `year`-Spalte aus dem Datensatz
    
    # Führen Sie die Sentimentanalyse durch
    sentiment_data <- selected_president_data %>%
      unnest_tokens(word, text) %>%
      inner_join(get_sentiments("afinn")) %>%
      group_by(president, year) %>%
      summarize(sentiment = sum(value)) %>%
      ungroup()
    
    # Erstellen Sie einen ggplot
    sentiment_plot <- ggplot(sentiment_data, aes(x = year, y = sentiment)) +
      geom_line() +
      geom_point(aes(color = sentiment)) + # Fügen Sie die Farbe basierend auf dem Sentimentwert hinzu
      #geom_bar(stat="identity",aes(fill=sentiment))+
      scale_color_gradient2(low = "red", mid = "yellow", high = "green", midpoint = 0, name = "Sentimentwert") + # Anpassen der Farbskala
      labs(title = paste("Sentimentanalyse von Reden von", input$president),
           x = "Jahr",
           y = "Sentimentwert") +
      theme(legend.position = "bottom") # Ändern Sie die Position der Legende
    
    # Zeigen Sie den Plot an
    print(sentiment_plot)
    
  })
  
  
}

# Starten Sie die Shiny-App
shinyApp(ui = ui, server = server)
