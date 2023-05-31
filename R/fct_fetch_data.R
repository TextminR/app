
#' fetch_data() function to get data of American president speeches
#'
#' @return the speeches
fetch_data <- function() {
  
  # Return the speeches of American presidents.
  return(base::readRDS(url("https://slcladal.github.io/data/sotu_paragraphs.rda", "rb")))
}

#' prepares and filters speeches of the president
#'
#' @param textdata the speeches
#'
#' @return the filtered speeches
prepare_data <- function(textdata) {
  
  # Groups the data by speech_doc_id, dates and presidents and joins all speech-parts to make one single cell, where the whole speech is stored.
  # (In the original data, the speeches are divided into multiple parts. speech_doc_id shows, which parts belong to which speech.)
  return(textdata %>%
           group_by(speech_doc_id, date, president) %>%
           summarise(text = paste(text, collapse = "")))
}