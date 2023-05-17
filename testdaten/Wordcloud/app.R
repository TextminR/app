library(shiny)
library(wordcloud2)
library(tidytext)
library(SnowballC)
library(dplyr)
library(tibble)

# Lade Textdaten
textdata <- base::readRDS(url("https://slcladal.github.io/data/sotu_paragraphs.rda", "rb"))

# Entferne "states"
v <- stop_words
v <- v %>% filter(word != "states")


ui <- fluidPage(
  # Titel
  titlePanel("Amerikanische Reden von Präsidenten"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("num_words", "Maximale Anzahl an Wörtern:",
                  min = 1, max = 500, value = 100, step = 10),
      sliderInput("years", "Von 1790 bis ...:",
                  min = 1790, max = 1903, value = 1790, step = 1)
    ),
    # Wordcloud-Bereich
    mainPanel(
      wordcloud2Output("wordcloud", width = "100%", height = "600px")
    )
  )
)

server <- function(input, output) {
  # Erzeuge Wordcloud
  output$wordcloud <- renderWordcloud2({
    # Filtere die Textdaten nach dem ausgewählten Jahr
    # %>% ist eine Pipe; statt jedes mal "words <-" zu verwenden, kann man es so machen und zwischenspeichern
    filtered_textdata <- textdata %>%
      mutate(year = year(date)) %>% # Extra Spalte year erstellen, um das Jahr zu vergleichen
      filter(year <= input$years) # Alle Zeilen entfernen, die <= user input sind
    
    # Verarbeite die gefilterten Textdaten mit tidytext
    words <- filtered_textdata %>%
      unnest_tokens(word, text, to_lower = TRUE) %>% # Die Wörter in der Spalte text werden extrahiert und in eine eigene Spalte word gespeichert 
      anti_join(v) # stopwords entfernen
    
    # Extrahiere die am häufigsten vorkommenden Wörter und ihre Häufigkeit
    top_words <- words %>%
      count(word, sort = TRUE) %>%
      head(input$num_words)
    
    # Erzeuge Wordcloud
    wordcloud2(data.frame(name = top_words$word, size = top_words$n),
               size = 1, backgroundColor = "white", fontFamily = "sans-serif")
  })
}

# Starte die App
shinyApp(ui, server)