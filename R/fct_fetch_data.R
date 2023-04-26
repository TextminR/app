library(dplyr)

fetch_data <- function() {
  return(base::readRDS(url("https://slcladal.github.io/data/sotu_paragraphs.rda", "rb")))
}

prepare_data <- function(textdata) {
  return(textdata %>%
    group_by(speech_doc_id, date, president) %>%
    summarise(text = paste(text, collapse = "")))
}
