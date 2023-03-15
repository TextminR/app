# Load the required packages
library(wordcloud)
library(tm)
library(wordcloud2)
library("stopwords")
library(tidyverse)
library(tidytext)

textdata2 <- base::readRDS(url("https://slcladal.github.io/data/sotu_paragraphs.rda", "rb"))


# Load the text file
text2 <- textdata2[2, 6]



# Convert the text to lowercase and remove punctuation
text2 <- tolower(text2)
text2 <- gsub("[[:punct:]]", "", text2)

# Tokenize the text
text2 <- unlist(strsplit(text2, "\\s+"))

textdata <- data.frame(
  text = text2
)

words <- textdata$text

wordcloud(words, min.freq = 100, scale=c(4,.5))

