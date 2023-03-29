#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)

# Define UI for application that draws a histogram
ui <- fluidPage(
    useShinyjs(),

    # Application title
    titlePanel("Visualisierungen"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          checkboxInput("trendgraph", "Trendgraph", FALSE),
          tags$div(id = "td-div",
            textInput("words","Type in words you want to see displayed."),
          ),
          
          checkboxInput("barchart", "Bar chart", FALSE),
          tags$div(id = "bc-div",
             textInput("words2","Type in words you want to see displayed."),
          ),
          
          checkboxInput("wordcloud", "Wordcloud", FALSE),
          tags$div(id = "wc-div",
            sliderInput("num_words", "Maximale Anzahl an Wörtern:",
                       min = 1, max = 500, value = 100, step = 10),
            sliderInput("years", "Von 1790 bis ...:",
                       min = 1790, max = 1903, value = 1790, step = 1)
          ),
        ),

        # Show a plot of the generated distribution
        mainPanel(
           
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  observeEvent(input$trendgraph, toggle("td-div"))
  observeEvent(input$barchart, toggle("bc-div"))
  observeEvent(input$wordcloud, toggle("wc-div"))
}

# Run the application 
shinyApp(ui = ui, server = server)
