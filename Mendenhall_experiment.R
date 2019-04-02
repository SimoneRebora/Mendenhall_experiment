### The following code tests the stylometric proposal by T.C. Mendenhall, 1887, "THE CHARACTERISTIC CURVES OF COMPOSITION" (http://science.sciencemag.org/content/ns-9/214S/237)
### Data is in the "corpus" folder
### As a convention, name of the author is the first string of characters before the underscore, i.e. dannunzio_forse_1910.txt -> dannunzio

library(stylo)
library(tidyverse)

### Prepare the corpus
setwd("corpus")
files <- list.files()
authors <- strsplit(files, "_")
authors <- unlist(lapply(authors, function(x) x[1]))
authors_sel <- unique(authors)
authors_sel_ids <- list()
for(i in 1:length(authors_sel)){
  authors_sel_ids[[i]] <- which(authors == authors_sel[i])
}
corpus <- lapply(files, readLines)
corpus <- unlist(lapply(corpus, function(x) paste(x, collapse = " ")))

### Calculate word length for each author in the corpus
author_stats <- list()
for(i in 1:length(authors_sel)){
  author_sel_corpus <- corpus[authors_sel_ids[[i]]]
  author_sel_corpus <- paste(author_sel_corpus, collapse = " ")
  author_sel_corpus <- stylo::txt.to.words.ext(author_sel_corpus, language = "Italian", preserve.case = T)
  author_sel_corpus <- nchar(author_sel_corpus)
  author_stats[[i]] <- table(author_sel_corpus)
}

### Calculate proportions by dividing each count for the total number of words
author_stats <- lapply(author_stats, function(x) {x/sum(x)})

### Prepare dataframe for visualization
full_df <- data.frame(author = character(), word_length = numeric(), proportion = numeric())
for(i in 1:length(authors_sel)){
  for(n in 1:length(author_stats[[i]])){
    df_tmp <- data.frame(author = authors_sel[i], word_length = n, proportion = author_stats[[i]][n])
    full_df <- rbind(full_df, df_tmp)
  }
}

### Print plot
p1 <- ggplot(full_df, mapping = aes(x=word_length, y=proportion, group=author, color=author)) +
  geom_line()
p1

### Save plot
setwd("..")
ggsave(p1, filename = "Mendenhall_experiment.svg", width = 4*2, height = 3*2)

### Run stylo analyses
# Traditional dendrogram
stylo()

# Network
library(networkD3)
network_result <- stylo.network()
saveNetwork(network = network_result, file = "Stylo_network.html")
