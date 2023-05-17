fetch_data <- function() {
  return(base::readRDS(url("https://slcladal.github.io/data/sotu_paragraphs.rda", "rb")))
}

prepare_data <- function(textdata) {
  return(textdata %>%
           group_by(speech_doc_id, date, president) %>%
           summarise(text = paste(text, collapse = "")))
}


#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  theme_set(theme(
    legend.position = "bottom",
    text = element_text(size = 14)
  ))
  
  textdata <- fetch_data()
  data <- prepare_data(textdata)
  
  plottg <- reactive({
    if (!input$checktg) {
      return(NULL)
    }
    
    renderTrendgraph(input$glwords, input$glpres, input$glyears[1], input$glyears[2])
  })
  
  plotbc <- reactive({
    if (!input$checkbc) {
      return(NULL)
    }
    
    renderBarchart(input$glwords, input$glpres, input$glyears[1], input$glyears[2])
  })
  
  plotst <- reactive({
    if (!input$checkst) {
      return(NULL)
    }
    
    renderSentiment(input$glpres, input$glyears[1], input$glyears[2])
  })
  
  output$plot <- renderPlot({
    ptlist <- list(plottg(), plotbc(), plotst())
    to_delete <- !sapply(ptlist, is.null)
    ptlist <- ptlist[to_delete]
    
    len <- length(ptlist)
    if (!len) {
      return(NULL)
    }
    
    gridExtra::grid.arrange(
      grobs = ptlist,
      ncol = ifelse(len >= 2, 2, len),
      nrow = ifelse(len >= 2, len / 2 + len %% 2, 1),
      padding = 2
    )
  })
  
  output$wcplot <- renderWordcloud2({
    renderWordcloud(input$glyears[1], input$glyears[2], input$wcnum)
  })
}
