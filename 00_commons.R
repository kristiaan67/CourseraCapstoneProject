
library(tm)
library(qdapRegex)
library(stringr)

## CONSTANTS

LANG <- "en"
LOCALE <- 'en_US'

input_dir <- paste('project/final/', LOCALE, sep = "")
output_dir <- paste('project/output/', LOCALE, sep = "")
corpus_file <- paste(LOCALE, ".blogs.txt", sep = "")

set.seed(1234)

## FUNCTIONS

# CORPUS STUFF

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


# DATA CLEANING STUFF

removeInternetStuff <- function(x) {
    rm_tag(rm_email(rm_url(x)))
}

removeRepeats <- function(x) {
    rm_repeated_words(x)
}

removeRedundantWhitespace <- function(x) {
    str_trim(stripWhitespace(x))
}

# DATA ANALYSIS STUFF

calculateCoverage <- function(thresholds, word_data_freq) {
    tot_words <- length(word_data_freq)
    tot_freq_words <- sum(word_data_freq)
    cov_num_words <- c()
    percents <- c()
    for (coverage in thresholds) {
        freq_words <- 0
        num_words <- 0
        while (freq_words < tot_freq_words * coverage) {
            num_words <- num_words + 1
            freq_words <- freq_words + word_data_freq[num_words]
        }
        cov_num_words <- c(cov_num_words, num_words)
        percentage <- num_words * 100 / tot_words
        percents <- c(percents, percentage)
        print(sprintf("%d unique words (of %d words in dictionary) cover %d%% of all word frequencies in corpus, or %.1f%% of dictionary", 
                      num_words, tot_words, coverage * 100, percentage))
    }
    return(data.table(coverage = thresholds, num_words = cov_num_words, 
                      percentages = percents, min_freq = word_data_freq[cov_num_words]))
}

# UTIL STUFF

splitWords <- function(x) {
    str_split(str_trim(x), "\\s+", simplify = TRUE)
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
