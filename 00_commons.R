
library(tm)

## CONSTANTS

LANG <- 'en'
LOCALE <- 'en_US'

input_dir <- paste('final/', LOCALE, sep = "")
output_dir <- paste('output/', LOCALE, sep = "")
corpus_file <- paste(LOCALE, ".blogs.txt", sep = "")


## FUNCTIONS

saveCorpus <- function(corpus, pattern) {
    file_names <- sapply(corpus, function(x) paste(pattern, x$meta[["id"]], sep = ""), USE.NAMES = FALSE)
    writeCorpus(corpus, path = output_dir, filenames = file_names)
    print(paste("Saved cleaned data to ", output_dir, "/", file_names, sep = ""))
}

loadCorpus <- function(pattern) {
    corpus <- VCorpus(DirSource(output_dir, pattern = pattern), 
                      readerControl = list(reader = readPlain, language = "eng"))
    print(paste("Loaded corpus from ", output_dir, "/", names(corpus), sep = ""))
    
    for (idx in 1:length(corpus)) {
        file_name <- meta(corpus[[idx]], "id")
        meta(corpus[[idx]], "id") <- substr(file_name, nchar(pattern) + 1, nchar(file_name))        
    }
    
    return(corpus)
}

corpusExists <- function(pattern) {
    file.exists(paste(output_dir, "/", pattern, corpus_file, sep = ""))
}

