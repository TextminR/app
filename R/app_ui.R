fetch_data <- function() {
  return(base::readRDS(url("https://slcladal.github.io/data/sotu_paragraphs.rda", "rb")))
}

prepare_data <- function(textdata) {
  return(textdata %>%
           group_by(speech_doc_id, date, president) %>%
           summarise(text = paste(text, collapse = "")))
}

#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

library(plotly)
library(shinythemes)
app_ui <- function(request) {
  textdata <- fetch_data()
  golem_add_external_resources()
  tagList(
    # Leave this function for adding external resources
    # Your application UI logic
    
    navbarPage(theme= shinytheme("cerulean") ,"Prototyp",
      shinyjs::useShinyjs(),
               
      tabPanel(
        "Basisdiag.",
        
        sidebarLayout(
          sidebarPanel(
            h4("Basisdiagramme"),
            
            textInput("glwords", "Anzuzeigende Wörter (mit Beistrich trennen):"),
            sliderInput("glyears",
                        "Zeitspanne:",
                        min = 1790, max = 1903, value = c(1800, 1850), sep = "", step = 1
            ),
            selectInput("glpres",
                        "Wählen Sie einen Präsidenten:",
                        choices = c("Alle", paste(unique(textdata$president))),
                        selected = "Alle"
            ),
            checkboxInput("checktg", "Trendgraph", FALSE),
            checkboxInput("checkbc", "Bar chart", FALSE),
            
            hr(),
            
            h4("Weitere Diagramme"),
            checkboxInput("checkst", "Sentimentanalyse", FALSE),
          ),
          mainPanel(plotlyOutput(outputId = "plot", height = "90vh"))
        )
      ),
      tabPanel(
        "Wordcloud",
        
        sidebarLayout(
          sidebarPanel(
            h4("Wordcloud"),
            
            sliderInput("wcnum",
                        "Maximale Anzahl an Wörtern:",
                        min = 1, max = 500, value = 100, step = 10
            ),
            sliderInput("wcyears",
                        "Zeitspanne:",
                        min = 1790, max = 1903, value = c(1800, 1850), sep = "", step = 1
            )
          ),
          mainPanel(
            wordcloud2Output("wcplot", width = "100%", height = "600px")
          )
        )
      )
    )
  )
}



#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )
  
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "textminRApp"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}