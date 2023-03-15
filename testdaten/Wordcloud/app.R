# Lade benötigte Pakete
library(shiny)
library(wordcloud2)

# Lade Textdaten
textdata <- base::readRDS(url("https://slcladal.github.io/data/sotu_paragraphs.rda", "rb"))
corpus <- Corpus(VectorSource(textdata))

# Verarbeite Textdaten
processedCorpus <- tm_map(corpus, content_transformer(tolower))
processedCorpus <- tm_map(processedCorpus, removeWords, stopwords())
processedCorpus <- tm_map(processedCorpus, removePunctuation, preserve_intra_word_dashes = TRUE)
processedCorpus <- tm_map(processedCorpus, removeNumbers)
processedCorpus <- tm_map(processedCorpus, stemDocument, language = "en")
processedCorpus <- tm_map(processedCorpus, stripWhitespace)

# Erstelle Shiny-App
ui <- fluidPage(
  # Titel
  titlePanel("Wordcloud mit Textdaten"),
  
  # Wordcloud-Bereich
  mainPanel(
    wordcloud2Output("wordcloud", width = "100%", height = "500px")
  )
)

server <- function(input, output) {
  # Erzeuge Wordcloud
  output$wordcloud <- renderWordcloud2({
    # Extrahiere Wörter aus den Textdaten und berechne die Häufigkeit
    words <- unlist(strsplit(tolower(paste(processedCorpus, collapse = " ")), "\\W+"))
    freq <- sort(table(words), decreasing = TRUE)
    top_words <- head(freq, 100)
    
    # Erzeuge Wordcloud
    wordcloud2(data.frame(name=names(top_words), size=as.numeric(top_words)), 
               size = 1, backgroundColor = "white", fontFamily = "sans-serif")
  })
}

# Starte die App
shinyApp(ui, server)