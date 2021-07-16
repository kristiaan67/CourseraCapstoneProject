
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

expandAbbreviations <- function(x) {
    res <- str_replace_all(x, "aren't", "are not")
    res <- str_replace_all(res, "can't", "cannot")
    res <- str_replace_all(res, "couldn't", "could not")
    res <- str_replace_all(res, "didn't", "did not")
    res <- str_replace_all(res, "doesn't", "does not")
    res <- str_replace_all(res, "don't", "do not")
    res <- str_replace_all(res, "hadn't", "had not")
    res <- str_replace_all(res, "hasn't", "has not")
    res <- str_replace_all(res, "haven't", "have not")
    res <- str_replace_all(res, "he'd", "he had")
    res <- str_replace_all(res, "he'll", "he will")
    res <- str_replace_all(res, "he's", " he has")
    res <- str_replace_all(res, "here's", "here is")
    res <- str_replace_all(res, "how's", "how is")
    res <- str_replace_all(res, "i'd", "i had")      
    res <- str_replace_all(res, "i'll", "i will")
    res <- str_replace_all(res, "i'm", "i am")
    res <- str_replace_all(res, "i've", "i have")
    res <- str_replace_all(res, "isn't", "is not")
    res <- str_replace_all(res, "it's", "it is")
    res <- str_replace_all(res, "let's", "let us")
    res <- str_replace_all(res, "mustn't", "must not")
    res <- str_replace_all(res, "shan't", "shall not")
    res <- str_replace_all(res, "she'd", "she had")
    res <- str_replace_all(res, "she'll", "she will")
    res <- str_replace_all(res, "she's", "she has")
    res <- str_replace_all(res, "shouldn't", "should not")
    res <- str_replace_all(res, "that's", "that is")
    res <- str_replace_all(res, "there's", "there is")
    res <- str_replace_all(res, "they'd", "they had")
    res <- str_replace_all(res, "they'll", "they will")
    res <- str_replace_all(res, "they're", "they are")
    res <- str_replace_all(res, "they've", "they have")
    res <- str_replace_all(res, "wasn't", "was not")
    res <- str_replace_all(res, "we'd", "we had")
    res <- str_replace_all(res, "we'll", "we will")
    res <- str_replace_all(res, "we're", "we are")
    res <- str_replace_all(res, "we've", "we have")
    res <- str_replace_all(res, "weren't", "were not")
    res <- str_replace_all(res, "what's", "what is")
    res <- str_replace_all(res, "when's", "when is")
    res <- str_replace_all(res, "where's", "where is")
    res <- str_replace_all(res, "who's", "who is")
    res <- str_replace_all(res, "why's", "why is")
    res <- str_replace_all(res, "won't", "will not")
    res <- str_replace_all(res, "wouldn't", "would not")
    res <- str_replace_all(res, "you'd", "you had")
    res <- str_replace_all(res, "you'll", "you will")
    res <- str_replace_all(res, "you're", "you are")
    res <- str_replace_all(res, "you've", "you have")
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
