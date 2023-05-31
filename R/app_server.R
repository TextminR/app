
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
  
  # Calls 'fct_fetch_data.R', to obtain the data (right now textdata only consists of speeches of American presidents).
  data <- fetch_data()
  
  # plot for Trendgraph
  plottg <- reactive({
    if (!input$checktg) {
      return(NULL)
    }
    
    # Calls the function 'renderTrendgraph()' from the file 'fct_render_plots.R'.
    renderTrendgraph(data, input$glwords, input$glpres, input$glyears[1], input$glyears[2])
  })
  
  # plot for BarChart
  plotbc <- reactive({
    if (!input$checkbc) {
      return(NULL)
    }
    
    # Calls the function 'renderBarchart()' from the file 'fct_render_plots.R'.
    renderBarchart(data, input$glwords, input$glpres, input$glyears[1], input$glyears[2])
  })
  
  # plot for Sentiment-analysis
  plotst <- reactive({
    if (!input$checkst) {
      return(NULL)
    }
    
    # Calls the function 'renderSentiment()' from the file 'fct_render_plots.R'.
    renderSentiment(data, input$glpres, input$glyears[1], input$glyears[2])
  })
  
  # Plots all graphs
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
  
  # plot for WordCloud
  output$wcplot <- renderWordcloud2({
    
    # Calls the function 'renderWordcloud()' from the file 'fct_render_plots.R'.
    renderWordcloud(data, input$glyears[1], input$glyears[2], input$wcnum)
  })
}
