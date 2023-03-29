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

dfList <- list()
words <- c("war", "peace")
i=1
for (word in words){
  dfList[[i]] = plooten(word)
  i=i+1
}


merged <- Reduce(merge_fun, dfList)

merged2 <- melt(merged, id.vars = c("speech_doc_id", "date", "president"))

p <- ggplot() +
  geom_smooth(merged2, mapping = aes(date, value, color = variable))

p <- ggplotly(p)
p
