library(shiny)
ui <- fluidPage(
  sidebarLayout(
    textInput("words", "Wörter eingeben:")
  ),
  mainPanel(
    plotOutput("plot")
  )
)
server <- function(input, output, session) {
  
  
  textdata <- base::readRDS(url("https://slcladal.github.io/data/sotu_paragraphs.rda", "rb"))
  df <- as.data.frame(textdata)
  
  data <- df %>%
    group_by(speech_doc_id, date, president) %>%
    summarise(text = paste(text, collapse = ""))
  
  plooten <- function(word) {
    result <- data
    result <- result %>%
      group_by(speech_doc_id, date, president) %>%
      summarise(date = as.Date(date), "count" = str_count(text, word))
    return(result)
  }
  
  merge_fun <- function(x, y) {
    merge(x, y, by = c("speech_doc_id", "date", "president"), all = TRUE)
  }
  
  words <- input$words
  dfs <- list()
  
  for (word in words){
    append(dfs, plooten(word))
  }
  
  output$plot <- renderPlot(
    
  )
}
shinyApp(ui, server)