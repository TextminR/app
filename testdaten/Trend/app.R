library(shiny)
library(dplyr)
library(stringr)
library(ggplot2)
library(plotly)
library(reshape2)


ui <- fluidPage(
    textInput("words", "Wörter eingeben:"), 
    plotlyOutput("plot")
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
  
  
  
  
  # words <- input$words
  # dfs <- list()
  # 
  # for (word in words){
  #   append(dfs, plooten(word))
  # }
  
  df1 <- plooten("war")
  df2 <- plooten("peace")
  df3 <- plooten("one")
  
  merged <- Reduce(merge_fun, list(df1, df2, df3))
  
  merged2 <- melt(merged, id.vars = c("speech_doc_id", "date", "president"))
  
  p <- ggplot() +
    geom_smooth(merged2, mapping = aes(date, value, color = variable))
  
  p <- ggplotly(p)
  
  output$plot <- renderPlotly(
    p
  )
}
shinyApp(ui, server)