library(tm)
library(igraph)

textdata <- base::readRDS(url("https://slcladal.github.io/data/sotu_paragraphs.rda", "rb"))

# Umwandlung der Textdaten in ein Corpus (Sammlung von Textdokumenten)
corpus <- Corpus(VectorSource(textdata))

# Satzzeichen entfernen
corpus <- tm_map(corpus, removePunctuation)

# Zahlen entfernen
corpus <- tm_map(corpus, removeNumbers)

# Text toLower transformieren
corpus <- tm_map(corpus, tolower)

# Stopwords entfernen
corpus <- tm_map(corpus, removeWords, stopwords("english"))

# Wandelt Wörter in ihre Stammform um. Von "stopped" wird "stop".
corpus <- tm_map(corpus, stemDocument)

tdm <- TermDocumentMatrix(corpus, control = list(wordLengths=c(3,Inf)))
m <- as.matrix(tdm)
word_pairs <- colnames(m)[sapply(gregexpr("\\s\\w+", colnames(m)), `[`, 1) != -1]
word_pair_counts <- sapply(word_pairs, function(x) sum(m[, grepl(x, colnames(m))]))
word_pair_df <- data.frame(word_pairs, word_pair_counts)

