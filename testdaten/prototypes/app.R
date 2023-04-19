library(shiny)
library(shinyjs)
library(gridExtra)
library(ggplot2)
library(wordcloud2)
library(dplyr)
library(stringr)
library(reshape2)
library(tm)
library(SnowballC)
library(tibble)
library(tidytext)
library(textdata)
library(lubridate)

textdata <-
  base::readRDS(url("https://slcladal.github.io/data/sotu_paragraphs.rda", "rb"))

data <- textdata %>%
  group_by(speech_doc_id, date, president) %>%
  summarise(text = paste(text, collapse = ""))

v <- stop_words
v <- v %>% filter(word != "states")

plooten <- function(data, word) {
  result <- data
  result <- result %>%
    group_by(speech_doc_id, date, president) %>%
    summarise(date = as.Date(date), "count" = str_count(text, word))
  return(result)
}

merge_fun <- function(x, y) {
  merge(x, y, by = c("speech_doc_id", "date", "president"), all = TRUE)
}

filter_year <- function(data, year_start, year_end) {
  return(data %>% filter(date >= year_start & date <= year_end))
}

filter_pres <- function(data, pres){
  if (pres == "Alle") {
    return(data)
  }
  
  return (data %>% filter(president == pres))
}

theme_set(theme(
  legend.position = "bottom",
  text = element_text(size = 14)
))

ui <- navbarPage("Prototyp",
  useShinyjs(),
  
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
      mainPanel(plotOutput(outputId = "plot", height = "90vh"))
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

server <- function(input, output) {
  plottg <- reactive({
    if (!input$checktg) {
      return(NULL)
    }

    words <- strsplit(input$glwords, ",")[[1]]
    if (!length(words)) {
      return(NULL)
    }
    
    filtered_data <- data
    filtered_data <- filter_pres(filtered_data, input$glpres)
    filtered_data <- filter_year(filtered_data, input$glyears[1], input$glyears[2])

    dfList <- list()
    i <- 1
    for (word in words) {
      if (!(str_replace_all(word, "[^[:alnum:]]", "")) == "") {
        word <- str_replace_all(word, "[^[:alnum:]]", "")
        dfList[[i]] <- plooten(filtered_data, word)
        i <- i + 1
      }
    }

    merged <- Reduce(merge_fun, dfList)
    colnames(merged)[4:ncol(merged)] <- words
    merged2 <-
      melt(merged, id.vars = c("speech_doc_id", "date", "president"))

    ggplot() +
      geom_smooth(merged2, mapping = aes(date, value, color = variable), se = FALSE) +
      labs(title = "Wortfrequenzen (über alle Jahre)", x = "Zeit", y = "Frequenz")
  })

  plotbc <- reactive({
    if (!input$checkbc) {
      return(NULL)
    }

    words <- strsplit(input$glwords, ",")[[1]]
    if (!length(words)) {
      return(NULL)
    }
    
    filtered_data <- data
    filtered_data <- filter_pres(filtered_data, input$glpres)
    filtered_data <- filter_year(filtered_data, input$glyears[1], input$glyears[2])

    wordcount <- c()
    for (i in 1:length(words)) {
      if (!(str_replace_all(words[i], "[^[:alnum:]]", "")) == "") {
        words[i] <- str_replace_all(words[i], "[^[:alnum:]]", "")
        wordcount[i] <- sum(str_count(filtered_data$text, words[i]))
      }
    }

    dataframe <- data.frame(word = words, count = wordcount)

    ggplot(data = dataframe, aes(x = word, y = count)) +
      geom_bar(stat = "identity", aes(fill = count)) +
      labs(title = "Wortfrequenzen (insgesamt)", x = "Wörter", y = "Frequenz")
  })

  plotst <- reactive({
    if (!input$checkst) {
      return(NULL)
    }
    
    selected_president_data <- textdata
    selected_president_data <- filter_pres(selected_president_data, input$glpres)
    selected_president_data <- filter_year(selected_president_data, input$glyears[1], input$glyears[2])
    
    selected_president_data <- selected_president_data %>%
      mutate(year = as.numeric(substr(date, 1, 4)))

    sentiment_data <- selected_president_data %>%
      unnest_tokens(word, text) %>%
      inner_join(get_sentiments("afinn")) %>%
      group_by(president, year) %>%
      summarize(sentiment = sum(value)) %>%
      ungroup()

    ggplot(sentiment_data, aes(x = year, y = sentiment)) +
      geom_line() +
      geom_point(aes(color = sentiment)) +
      scale_color_gradient2(low = "red", mid = "yellow", high = "green", midpoint = 0, name = "Sentimentwert") +
      labs(
        title = paste("Sentimentanalyse der Reden von:", input$glpres),
        x = "Jahr",
        y = "Sentimentwert"
      )
  })

  output$plot <- renderPlot({
    ptlist <- list(plottg(), plotbc(), plotst())
    to_delete <- !sapply(ptlist, is.null)
    ptlist <- ptlist[to_delete]

    len <- length(ptlist)
    if (!len) {
      return(NULL)
    }

    grid.arrange(
      grobs = ptlist,
      ncol = ifelse(len >= 2, 2, len),
      nrow = ifelse(len >= 2, len / 2 + len %% 2, 1),
      padding = 2
    )
  })

  output$wcplot <- renderWordcloud2({
    filtered_textdata <- filter_year(textdata, input$wcyears[1], input$wcyears[2])

    words <- filtered_textdata %>%
      unnest_tokens(word, text, to_lower = TRUE) %>%
      anti_join(v)

    top_words <- words %>%
      count(word, sort = TRUE) %>%
      head(input$wcnum)

    wordcloud2(data.frame(name = top_words$word, size = top_words$n),
      size = 1, backgroundColor = "white", fontFamily = "sans-serif"
    )
  })
}

shinyApp(ui = ui, server = server)
