
#########################################
## Task 1: Getting and Cleaning the Data
#########################################

########################################
# Input: data set Coursera-SwiftKey.zip
# Output: clean corpus "final_"
########################################

library(hunspell)

source('project/00_commons.R', echo = FALSE)

# 1. Tokenization - identifying appropriate tokens such as words, punctuation, and numbers. 
#    Writing a function that takes a file as input and returns a tokenized version of it.
# 2. Profanity filtering - removing profanity and other words you do not want to predict.

## FUNCTIONS

removeMisspelledWords <- function(corpus) {
    wrong_words <- generateMisspelledWords(corpus)

    loaded_tmp_file_count <- 0
    pattern <- paste("tmp_", loaded_tmp_file_count, "_", sep = "")
    while (corpusExists(pattern)) {
        loaded_tmp_file_count <- loaded_tmp_file_count + 1
        pattern <- paste("tmp_", loaded_tmp_file_count, "_", sep = "")
    }
    if (loaded_tmp_file_count > 0) { # load the last available corpus
        corpus <- loadCorpus(paste("tmp_", loaded_tmp_file_count - 1, "_", sep = ""))
    }
    
    count <- 1
    saved_tmp_file_count <- loaded_tmp_file_count
    num_batch <- 100 # after every 'batch' a temp corpus is written
    num_words_batch <- 1000 # the number of words that is processed in 1 batch
    for (batch in sliceVector(wrong_words, num_words_batch)) {
        if (count > loaded_tmp_file_count * num_batch) {
            corpus <- tm_map(corpus, removeWords, batch)
            print(paste("         * removed batch of wrong words:", count))
            if (count %% num_batch == 0) {
                saveCorpus(corpus, paste("tmp_", saved_tmp_file_count, "_", sep = ""))
                saved_tmp_file_count <- saved_tmp_file_count + 1
            }
        }
        count <- count + 1
    }
    
    return(corpus)
}

generateMisspelledWords <- function(corpus) {
    wrong_words_file <- paste(output_dir, "/wrong-words.txt", sep = "")
    if (!file.exists(wrong_words_file)) {
        wrong_words <- c()
        for (c in 1:length(corpus)) {
            wrong_words <- unique(c(wrong_words, unlist(hunspell(corpus[[c]]$content))))
        }
        writeLines(sort(wrong_words), wrong_words_file)
        print("         * generated wrongly spelled or foreign words")
        
    } else {
        wrong_words <- readLines(wrong_words_file)
        print("         * loaded wrongly spelled or foreign words")
    }
    
    return(wrong_words)
} 

removeProfaneWords <- function(corpus) {
    if (!file.exists("project/bad-words.txt")) {
        # see https://www.cs.cmu.edu/~biglou/resources/bad-words.txt
        download.file("https://www.cs.cmu.edu/~biglou/resources/bad-words.txt",
                      destfile = "project/bad-words.txt", method = "curl")
    }
    
    bad_words <- readLines("project/bad-words.txt")
    count <- 1
    for (batch in sliceVector(bad_words, 1500)) {
        corpus <- tm_map(corpus, removeWords, stemDocument(batch))
        print(paste("         * removed batch of profane words:", count))
        count <- count + 1
    }
    
    return(corpus)
}


## START

print("Loading and cleaning data...")

if (!file.exists("project/Coursera-SwiftKey.zip")) {
    download.file("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip",
                  destfile = "project/Coursera-SwiftKey.zip", method = "curl")
    unzip("project/Coursera-SwiftKey.zip")
}

dictionary(LOCALE)

## 1. Clean Data
if (corpusExists("base_")) {
    corpus <- loadCorpus("base_")
    
} else {
    if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
    }
    
    corpus <- VCorpus(DirSource(input_dir), 
                      readerControl = list(reader = readPlain, language = LOCALE))
    print(inspect(corpus))
    
    print("Initial corpus created, cleaning and prepairing data...")
    print("    - transforming to 'ASCII'...")
    corpus <- tm_map(corpus, content_transformer(function(x) iconv(x, to = "ASCII", sub = "")))
    print("    - transforming to lowercase...")
    corpus <- tm_map(corpus, content_transformer(tolower))
    print("    - removing stopwords...")
    corpus <- tm_map(corpus, removeWords, stopwords(LANG))
    print("    - removing internet stuff...")
    corpus <- tm_map(corpus, content_transformer(removeInternetStuff))
    print("    - removing punctuation...")
    corpus <- tm_map(corpus, removePunctuation)
    print("    - removing numbers...")
    corpus <- tm_map(corpus, removeNumbers)
    
    # save base result
    saveCorpus(corpus, "base_")
}

if (corpusExists("wrong_")) {
    corpus <- loadCorpus("wrong_")
    
} else {
    print("    - removing wrongly spelled or foreign words...")
    corpus <- removeMisspelledWords(corpus)
    saveCorpus(corpus, "wrong_")
    rm(wrong_words)
}

if (corpusExists("stem_")) {
    corpus <- loadCorpus("stem_")
    
} else {
    print("    - stemming...")
    corpus <- tm_map(corpus, stemDocument)
    saveCorpus(corpus, "stem_")
}

## 2. Filter profane words
if (corpusExists("profane_")) {
    corpus <- loadCorpus("profane_")
    
} else {
    print("Data cleaned, filtering profane words...")
    corpus <- removeProfaneWords(corpus)

    print("    - removing repeats...")
    corpus <- tm_map(corpus, content_transformer(removeRepeats))
    print("    - stripping whitespace...")
    corpus <- tm_map(corpus, content_transformer(removeRedundantWhitespace))
}

saveCorpus(corpus, "final_")
