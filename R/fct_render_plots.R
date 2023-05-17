library(ggplot2)
library(reshape2)
library(stringr)
library(tidytext)
library(wordcloud2)
library(dplyr)

#' Title
#'
#' @param data 
#' @param word 
#'
#' @return
#' @export
#'
#' @examples
plooten <- function(data, word) {
  result <- data
  result <- result %>%
    group_by(speech_doc_id, date, president) %>%
    summarise(date = as.Date(date), "count" = str_count(text, word))
  return(result)
}


#' Title
#'
#' @param x 
#' @param y 
#'
#' @return
#' @export
#'
#' @examples
merge_fun <- function(x, y) {
  merge(x, y, by = c("speech_doc_id", "date", "president"), all = TRUE)
}

#' Title
#'
#' @param data 
#' @param year_start 
#' @param year_end 
#'
#' @return
#' @export
#'
#' @examples
filter_year <- function(data, year_start, year_end) {
  return(filter(data, date >= year_start & date <= year_end))
}

#' Title
#'
#' @param data 
#' @param pres 
#'
#' @return
#' @export
#'
#' @examples
filter_pres <- function(data, pres){
  if (pres == "Alle") {
    return(data)
  }
  
  return (data %>% filter(president == pres))
}


#' Title
#'
#' @param words 
#' @param pres 
#' @param years_start 
#' @param years_end 
#'
#' @return
#' @export
#'
#' @examples
renderTrendgraph <- function(data, words, pres, years_start, years_end) {
  words <- strsplit(words, ",")[[1]]
  if (!length(words)) {
    return(NULL)
  }
  
  filtered_data <- data
  filtered_data <- filter_pres(filtered_data, pres)
  filtered_data <- filter_year(filtered_data, years_start, years_end)
  
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
  
  return (ggplot() +
    geom_smooth(merged2, mapping = aes(date, value, color = variable), se = FALSE) +
    labs(title = "Wortfrequenzen (über alle Jahre)", x = "Zeit", y = "Frequenz"))
}


#' Takes a set of strings, looks up their occurrence 
#' and returns a ggplot visualizing the words with their occurrence as a bar-chart.
#'
#' @param words a string of words seperated by commas (,)
#' @param pres name of an amercian president
#' @param years_start first year of the textdata that influences the plot
#' @param years_end last year of the textdata that influences the plot
#'
#' @return bar-chart as ggplot
renderBarchart <- function(data, words, pres, years_start, years_end) {
  words <- strsplit(words, ",")[[1]]
  if (!length(words)) {
    return(NULL)
  }
  
  filtered_data <- data
  filtered_data <- filter_pres(filtered_data, pres)
  filtered_data <- filter_year(filtered_data, years_start, years_end)
  
  wordcount <- c()
  for (i in 1:length(words)) {
    if (!(str_replace_all(words[i], "[^[:alnum:]]", "")) == "") {
      words[i] <- str_replace_all(words[i], "[^[:alnum:]]", "")
      wordcount[i] <- sum(str_count(filtered_data$text, words[i]))
    }
  }
  
  dataframe <- data.frame(word = words, count = wordcount)
  
  return(ggplot(data = dataframe, aes(x = word, y = count)) +
    geom_bar(stat = "identity", aes(fill = count)) +
    labs(title = "Wortfrequenzen (insgesamt)", x = "Wörter", y = "Frequenz"))
}


#' Title
#'
#' @param pres 
#' @param years_start 
#' @param years_end 
#'
#' @return
#' @export
#'
#' @examples
renderSentiment <- function(data, pres, years_start, years_end) {
  selected_president_data <- data
  selected_president_data <- filter_pres(selected_president_data, pres)
  selected_president_data <- filter_year(selected_president_data, years_start, years_end)
  
  selected_president_data <- selected_president_data %>%
    mutate(year = as.numeric(substr(date, 1, 4)))
  
  sentiment_data <- selected_president_data %>%
    unnest_tokens(word, text) %>%
    inner_join(get_sentiments("afinn")) %>%
    group_by(president, year) %>%
    summarize(sentiment = sum(value)) %>%
    ungroup()
  
  return(ggplot(sentiment_data, aes(x = year, y = sentiment)) +
    geom_line() +
    geom_point(aes(color = sentiment)) +
    scale_color_gradient2(low = "red", mid = "yellow", high = "green", midpoint = 0, name = "Sentimentwert") +
    labs(
      title = paste("Sentimentanalyse der Reden von:", pres),
      x = "Jahr",
      y = "Sentimentwert"
    ))
}

#' Title
#'
#' @param years_start 
#' @param years_end 
#' @param lim 
#'
#' @return
#' @export
#'
#' @examples
renderWordcloud <- function(data, years_start, years_end, lim) {
  v <- tidytext::stop_words
  v <- v %>% filter(word != "states")
  
  filtered_textdata <- filter_year(data, years_start, years_end)
  
  words <- filtered_textdata %>%
    unnest_tokens(word, text, to_lower = TRUE) %>%
    anti_join(v)
  
  top_words <- words %>%
    count(word, sort = TRUE) %>%
    head(lim)
  
  wordcloud2(data.frame(name = top_words$word, size = top_words$n),
             size = 1, backgroundColor = "white", fontFamily = "sans-serif"
  )
}