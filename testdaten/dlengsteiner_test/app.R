library(shiny)
library(tidyverse)
library(stringr)
library(tidytext)

#getting and formatting data
textdata <- base::readRDS(url("https://slcladal.github.io/data/sotu_paragraphs.rda", "rb"))
speech <- textdata %>% group_by(speech_doc_id,president,date,) %>% summarise(text = paste0(text, collapse = " ")) 



ui <- fluidPage(
  #changing font size of all text
  tags$head(
    tags$style("label{font-size: 15pt;}"),
    tags$style("#words_input {font-size:15pt;height:20pt;color:green}"),
  ),
  #title of shiny page
  titlePanel(
    "Barcharts"
  ),
  #main panel for I/O
  sidebarLayout(
    sidebarPanel(
      textInput("words_input","Type in words you want to see displayed, seperated by a comma (,). WARNING: words are case-sensitive!")
    ),
    mainPanel(
      plotOutput("barchart", height = "500px")
    )
  )
)



server <- function(input, output, session) {
  
  print("Server starting...")
  
  #printing the barchart
  output$barchart <- renderPlot({
    #split the input by  comma (,) to extract words
    words <- strsplit(input$words_input,",")[[1]]
    
    
    tryCatch({
      #creating vector for the word count of each word
      wordcount<- c()
      for(i in 1:length(words)){
        #Removing all non-alpha-numeric characters and numbers from the extracted words,
        if(!(str_replace_all(words[i], "[^[:alnum:]]", ""))==""){
          words[i] <- str_replace_all(words[i], "[^[:alnum:]]", "")
          #saving the wordcount of the current word to the new vector
          wordcount[i] <- sum(str_count(speech$text, words[i]))
        }
      }
      #dataframe which combines words and wordcount
      dataframe <- data.frame(word=words,count=wordcount)
      #plotting the dataframe
      ggplot(data=dataframe, aes(x=word,y=count)) + 
        geom_bar(stat="identity", aes(fill=count)) +
        #scale_fill_gradientn(colours = c("darkblue","blue","blue","purple","magenta","red","red")) + 
        ggtitle("Frequency of words used \n in presidential speeches from 1790 - 1903") +
        theme(text=element_text(size=25),plot.title=element_text(hjust=0.5))
      
    }, error = function(e)
      # Draw ggplot2 plot with text only
      ggplot() +                      
      annotate("text",
               x = 1,
               y = 1,
               size = 12,
               label = "Please enter a word") + 
      theme_void()
    )
    #barplot(wordcount,names.arg=words,xlab="Word",ylab="Frequency",col="Red",main="Frequency of words used in presidential speeches from 1790 - 1903",border="black")
  })
}

#starting the shiny-app
shinyApp(ui, server)