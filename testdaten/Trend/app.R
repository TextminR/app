library(shiny)
library(dplyr)
library(stringr)
library(ggplot2)
library(plotly)
library(reshape2)

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





ui <- fluidPage(
    textInput("words", "Wörter eingeben:"), 
    plotlyOutput("plot")
)




server <- function(input, output, session) {
  observeEvent(input$words,{
    output$plot <- renderPlotly({
      words <- strsplit(input$words, ",")[[1]]
      
      dfList <- list()
      i=1
      for (word in words){
        if(!(str_replace_all(word, "[^[:alnum:]]", ""))==""){
          word <- str_replace_all(word, "[^[:alnum:]]", "")
          dfList[[i]] = plooten(word)
          i=i+1
        }
        if(i > 3){
          break
        }
      }
      
      merged <- Reduce(merge_fun, dfList)
      
      merged2 <- melt(merged, id.vars = c("speech_doc_id", "date", "president"))
      
      p <- ggplot() +
        geom_smooth(merged2, mapping = aes(date, value, color = variable), se = FALSE)
      
      p <- ggplotly(p)
      p
    })
  })
}
shinyApp(ui, server)