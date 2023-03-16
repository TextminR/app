library(shiny)
library(wordcloud2)
library(tidytext)
library(tm)
library(SnowballC)
library(dplyr)
library(tibble)


# Lade Textdaten
textdata <- base::readRDS(url("https://slcladal.github.io/data/sotu_paragraphs.rda", "rb"))

# Verarbeite Textdaten mit tidytext
# %>% ist eine Pipe; statt jedes mal "words <-" zu verwenden, kann man es so machen und zwischenspeichern
words <- textdata %>%
  # Wie Wörter in der Spalte text werden extrahiert und in eine eigene Spalte word gespeichert
  unnest_tokens(word, text) %>%
  # Entferne stopwords
  anti_join(stop_words)

# Erstelle Shiny-App
ui <- fluidPage(
  # Titel
  titlePanel("Wordcloud mit Textdaten"),
  
  # Wordcloud-Bereich
  mainPanel(
    sliderInput("num_words", "Maximale Anzahl an Wörtern:",
                min = 1, max = 500, value = 100, step = 10),
    wordcloud2Output("wordcloud", width = "100%", height = "600px")
  )
)

server <- function(input, output) {
  # Erzeuge Wordcloud
  output$wordcloud <- renderWordcloud2({
    # Extrahiere die am häufigsten vorkommenden Wörter und ihre Häufigkeit
    top_words <- words %>%
      # Gruppiert alle wörter in "word" und zählt wie oft sie vorkommen + sortiert sie
      count(word, sort = TRUE) %>%
      # User input zur Größen bestimmung  
      head(input$num_words)
    
    # Erzeuge Wordcloud
    wordcloud2(data.frame(name = top_words$word, size = top_words$n),
               size = 1, backgroundColor = "white", fontFamily = "sans-serif")
  })
}

# Starte die App
shinyApp(ui, server)
