
######################
## Task 3: Model Data
######################

#########################
# Input: main_word_data
# Output: lookup_table
########################

library(data.table)
library(dplyr)
library(ngram)
library(stringr)

source('project/00_commons.R', echo = FALSE)

stopifnot(corpusExists("final_"))

## CONSTANTS

# the maximum number of words in a phrase for which an n-gram is generated
MAX_NGRAM <- 4

## START

## Algorithm

## Data Model:
#  - generate n-gram phrase tables
#  - split the words of the phrases (the last word is the prediction in context of the words before)
#  - only use the phrases of which the last word (the prediction) is part of the main words coverage
#  - save the words and the probability of the phrase in a data table
#  - create an index for each word column

lookupTableFileName <- function(n) {
    paste(output_dir, "/", n, "-lookup_table.csv", sep = "")
}

# Checks whether at least one word of the context is a main word.
contextHasMainWord <- function(contexts, main_word_data) {
    sapply(contexts, function(context) { 
        for (word in splitWords(context)) {
            if (word %in% main_word_data$word) {
                return(TRUE)
            }
        }
        return(FALSE)
    })
}

createCorpusNgramTable <- function(n, corpus, main_word_data) {
    # generate n-gram phrase tables
    print(paste("Generating ", n, "-gram phrase tables...", sep = ""))
    
    # collect all the sentences of the corpus 
    # (filtering out sentences that have less than 'n' words)
    phrases <- c()
    for (idx in 1:length(corpus)) {
        phrases <- c(phrases,
                     corpus[[idx]]$content[sapply(corpus[[idx]]$content, 
                                                  function(p) length(splitWords(p)) >= n, 
                                                  USE.NAMES = FALSE)])
    }
    
    pt <- as.data.table(get.phrasetable(ngram(phrases, n = n))) %>%
        mutate(ngrams = str_trim(ngrams))
    
    ngram_table <- data.table(context = word(pt$ngrams, 1, -2), prediction = word(pt$ngrams, -1), 
                              freq = pt$freq, prop = pt$prop, ngram = n)
    setindex(ngram_table, "context")
    setindex(ngram_table, "prediction")
    if (n == 2) {
        ngram_table <- ngram_table[(prediction %in% main_word_data$word) & 
                                       (context %in% main_word_data$word)]
        
    } else {
        ngram_table <- ngram_table[prediction %in% main_word_data$word & 
                                       contextHasMainWord(context, main_word_data)]
    }

    fwrite(ngram_table, file = lookupTableFileName(n))
    print(paste("Saved ", n, "-gram lookup table: ", lookupTableFileName(n), sep = ""))

    return(ngram_table)
}

createLookupTables <- function() {
    corpus <- loadCorpus("final_")
    
    main_words_file <- paste(output_dir, "/main_word_data.csv", sep = "")
    stopifnot(file.exists(main_words_file))
    main_word_data = fread(main_words_file)
    setkey(main_word_data, "word")

    lookup_tables <- list()
    for (n in 2:MAX_NGRAM) {
        ngram_table <- createCorpusNgramTable(n, corpus, main_word_data)
        lookup_tables[[n - 1]] <- ngram_table
    }

    return(lookup_tables)
}

loadLookupTables <- function() {
    lookup_tables <- list()
    for (n in 2:MAX_NGRAM) {
        file_name <- lookupTableFileName(n)
        if (file.exists(file_name)) {
            ngram_table <- fread(file_name, colClasses = c(character(), character(), numeric(), numeric(), numeric()))
            setindex(ngram_table, "context")
            setindex(ngram_table, "prediction")
            lookup_tables[[n - 1]] <- ngram_table
        }
    }  
    
    return(lookup_tables)      
}

