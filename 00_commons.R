
library(tm)
library(qdapRegex)

## CONSTANTS

LANG <- "en"
LOCALE <- 'en_US'

input_dir <- paste('project/final/', LOCALE, sep = "")
output_dir <- paste('project/output/', LOCALE, sep = "")
corpus_file <- paste(LOCALE, ".blogs.txt", sep = "")


## FUNCTIONS

saveCorpus <- function(corpus, pattern) {
    file_names <- sapply(corpus, function(x) paste(pattern, x$meta[["id"]], sep = ""), USE.NAMES = FALSE)
    writeCorpus(corpus, path = output_dir, filenames = file_names)
    print(paste("Saved cleaned data to ", output_dir, "/", file_names, sep = ""))
}

loadCorpus <- function(pattern) {
    corpus <- VCorpus(DirSource(output_dir, pattern = pattern), 
                      readerControl = list(reader = readPlain, language = LOCALE))
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


removeInternetStuff <- function(x) {
    rm_tag(
        rm_email(
            rm_url(x, trim = FALSE, clean = FALSE), 
            trim = FALSE, clean = FALSE), 
        trim = FALSE, clean = FALSE)
}

removeRepeats <- function(x) {
    rm_repeated_words(
        rm_repeated_characters(x, trim = FALSE, clean = FALSE), 
        trim = FALSE, clean = FALSE)
}

removeRedundantWhitespace <- function(x) {
    str_trim(stripWhitespace(x))
}

splitWords <- function(x) {
    str_split(str_trim(x), "\\s", simplify = TRUE)
}

sliceVector <- function(v, n) {
    res <- list()
    idx <- 1
    while (idx <= length(v)) {
        res[[(idx %/% n) + 1]] <- v[idx:min(idx+n-1, length(v))]
        idx <- idx + n
    }
    return(res)
}
