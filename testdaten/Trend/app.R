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
    
    #changing font size of all text
    tags$head(
      tags$style("label{font-size: 12pt;}"),
      tags$style("[type = 'text'] {font-size:12pt;height:20pt;}")
    ),
    #title of shiny page
    titlePanel(
      "Linecharts"
    ),
    #main panel for I/O
    sidebarLayout(
      sidebarPanel(
        textInput("words","Type in words you want to see displayed.")
      ),
      mainPanel(
        plotlyOutput("plot", height = "500px")
      )
    )
)




server <- function(input, output, session) {
  tryCatch({
    observeEvent(input$words,{
      words <- strsplit(input$words, ",")[[1]]
      output$plot <- renderPlotly({
        
        dfList <- list()
        i=1
        for (word in words){
          if(!(str_replace_all(word, "[^[:alnum:]]", ""))==""){
            word <- str_replace_all(word, "[^[:alnum:]]", "")
            dfList[[i]] = plooten(word)
            i=i+1
          }
          # if(i > 3){
          #   break
          # }
        }
        
        
        tryCatch({
          merged <- Reduce(merge_fun, dfList)
          colnames(merged)[4:ncol(merged)] <- words
          merged2 <- melt(merged, id.vars = c("speech_doc_id", "date", "president"))
          p <- ggplot() +
            geom_smooth(merged2, mapping = aes(date, value, color = variable), se = FALSE)
          
          p <- ggplotly(p)
          p
        },error = function(e)
          ggplot() +                      
            annotate("text",
                   x = 1,
                   y = 1,
                   size = 12,
                   label = "Please enter a word") + 
            theme_void()
        )
      
        

      })
    })
  },error=function(e)
    ggplotly() +                      
    annotate("text",
             x = 1,
             y = 1,
             size = 12,
             label = "Please enter a word") + 
    theme_void()
  )
}
shinyApp(ui, server)