
######################
## Task 3: Model Data
######################

library(data.table)
library(ngram)
library(stringr)

source('project/00_commons.R', echo = FALSE)

## CONSTANTS

# the maximum number of words in a phrase for which an n-gram is generated
MAX_NGRAM <- 3
LOOKUP_FILE <- paste(output_dir, "/lookup_table.csv", sep = "")
MAIN_WORDS_FILE <- paste(output_dir, "/main_word_data.csv", sep = "")

## START

## Algorithm

# Data Model:
#  - generate n-gram phrase tables
#  - split the words of the phrases (the last word is the prediction in context of the words before)
#  - only use the phrases of which the last word (the prediction) is part of the main words coverage
#  - save the words and the probability of the phrase in a data table
#  - create an index for each word column

createContext <- function(words) {
    context <- NULL
    for (word in words) {
        if (is.null(context)) {
            context <- word
        } else {
            context <- sprintf("%s_%s", context, word)
        }
    }
    return(context)
}

createLookupTable <- function(corpus, main_word_data) {
    # concatenate the documents of the corpus
    text <- concatenate(sapply(corpus, function(c) concatenate(c$content), USE.NAMES = FALSE))
    
    lookup_table <- data.table(context = character(), prediction = character(), 
                               freq = numeric(), prop = numeric(), ngram = numeric())
    setindex(lookup_table, "context")

    for (n in 2:MAX_NGRAM) {
        # generate n-gram phrase tables
        print(paste("Generating ", n, "-gram phrase table...", sep = ""))
        pt <- get.phrasetable(ngram(text, n = n))
        # split the words of the phrases and create lookup table
        wt <- splitWords(pt$ngrams)
        contexts <- wt[,1:(n - 1)]
        predictions <- wt[,n]
        if (n == 2) {
            lookup_table <- rbind(lookup_table, 
                                  cbind(context = contexts, prediction = predictions, 
                                        freq = pt$freq, prop = pt$prop, ngram = n))
        } else {
            lookup_table <- rbind(lookup_table, 
                                  cbind(context = apply(contexts, 1, createContext),
                                        prediction = predictions, freq = pt$freq, 
                                        prop = pt$prop, ngram = n))
        }
        # only use the phrases of which the last word (the prediction) is part of the main words coverage
        lookup_table <- lookup_table[prediction %in% main_word_data$word]
        
        rm(pt, wt, contexts, predictions)
    }
    rm(text)
    
    return(lookup_table)
}

lookup_table <- NULL
if (file.exists(LOOKUP_FILE)) {
    lookup_table <- fread(LOOKUP_FILE, colClasses = c(character(), character(), numeric(), numeric(), numeric()))
    setindex(lookup_table, "context")

} else {
    if (!corpusExists("final_")) {
        source('01_prepare-data.R', echo = FALSE)
        stopifnot(corpusExists("final_"))
    }
    corpus <- loadCorpus("final_")
    
    stopifnot(file.exists(MAIN_WORDS_FILE))
    main_word_data = fread(MAIN_WORDS_FILE)
    
    lookup_table <- createLookupTable(corpus, main_word_data)
    fwrite(lookup_table, file = LOOKUP_FILE)
    print(paste("Saved lookup table to", LOOKUP_FILE))
    rm(corpus, main_word_data)
}
print(str(lookup_table))
print(head(lookup_table))

# Lookup Process:
#  - the input is a phrase consisting of 'n' words
#  - clean and stem the input phrase (using the same steps used to clean the corpus)
#  - split the input phrase and retrieve the last word
#  - retrieve the records from the data model that have this word as 'n-1' word
#  - if the input phrase consists of more than one word check for the other words in the context as well
#  - if matches were found, sort first on the number of words in the match and second the probability 
#  - return the list of predictions

cleanPhrase <- function(phrase) {
    clean_phrase <- stemDocument(
        removeRedundantWhitespace(
            removeWords(
                removeNumbers(
                    removePunctuation(
                        removeRepeats(
                            removeInternetStuff(
                                str_to_lower(
                                    iconv(phrase, to = "latin1", sub = "")))))), 
                stopwords(LANG))))
    return(clean_phrase)
}

predictNextWords <- function(phrase) {
    res <- NULL
    lookup_words <- splitWords(cleanPhrase(phrase))
    for (n in (MAX_NGRAM - 1):1) {
        c <- createContext(lookup_words[(length(lookup_words) - n + 1):length(lookup_words)])
        if (is.null(res)) {
            res <- lookup_table[context == c]
        } else {
            res <- rbind(res, lookup_table[context == c])
        }
    }
    
    setorder(res, -ngram, -freq)
    return(res)
}

