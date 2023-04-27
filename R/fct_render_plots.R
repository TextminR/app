library(ggplot2)
library(reshape2)
library(stringr)
library(tidytext)
library(wordcloud2)
library(dplyr)

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
  return(dplyr::filter(data, date >= year_start & date <= year_end))
}

filter_pres <- function(data, pres){
  if (pres == "Alle") {
    return(data)
  }
  
  return (data %>% filter(president == pres))
}

renderTrendgraph <- function(words, pres, years_start, years_end) {
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
  
  return (ggplotly(ggplot() +
    geom_smooth(merged2, mapping = aes(date, value, color = variable, size=2), se = FALSE) +
    labs(title = "Wortfrequenzen (über alle Jahre)", x = "Zeit", y = "Frequenz") +
    theme(text = element_text(size=18)))) 
}

renderBarchart <- function(words, pres, years_start, years_end) {
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
  
  return(ggplotly(ggplot(data = dataframe, aes(x = word, y = count)) +
    geom_bar(stat = "identity", aes(fill = count)) +
    labs(title = "Wortfrequenzen (insgesamt)", x = "Wörter", y = "Frequenz") + 
    theme(text=element_text(size=18))))
}

renderSentiment <- function(pres, years_start, years_end) {
  selected_president_data <- textdata
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
  
  return(ggplotly(ggplot(sentiment_data, aes(x = year, y = sentiment)) +
    geom_line() +
    geom_point(aes(color = sentiment)) +
    scale_color_gradient2(low = "red", mid = "yellow", high = "green", midpoint = 0, name = "Sentimentwert") +
    labs(
      title = paste("Sentimentanalyse der Reden von:", pres),
      x = "Jahr",
      y = "Sentimentwert"
    )))
}

renderWordcloud <- function(years_start, years_end, lim) {
  v <- tidytext::stop_words
  v <- v %>% filter(word != "states")
  
  filtered_textdata <- filter_year(textdata, years_start, years_end)
  
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