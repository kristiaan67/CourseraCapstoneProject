
#########################################
## Task 1: Getting and Cleaning the Data
#########################################

library(qdapRegex)
library(hunspell)

source('00_commons.R', echo = FALSE)

# 1. Tokenization - identifying appropriate tokens such as words, punctuation, and numbers. 
#    Writing a function that takes a file as input and returns a tokenized version of it.
# 2. Profanity filtering - removing profanity and other words you do not want to predict.

## FUNCTIONS

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
    gsub("´ll", " will", 
         gsub("´re", " are", 
              gsub("´ve", " have", 
                   gsub("´t", " not", x))))
}

replaceSpecialChars <- function(x) {
    gsub("\\s+$", "", 
         gsub("^\\s+", "", 
              gsub("[-–_ˆ“”‘’°…¬″′•♥■★»ª]", " ", x)))
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


## START

if (corpusExists("final_")) {
    print("Corpus already cleaned and prepaired, nothing to do!")    
    return();
}

print("Loading and cleaning data...")

if (!file.exists("Coursera-SwiftKey.zip")) {
    download.file("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip",
                  destfile = "Coursera-SwiftKey.zip", method = "curl")
    unzip("Coursera-SwiftKey.zip")
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
    print("    - transforming to 'latin1'...")
    corpus <- tm_map(corpus, content_transformer(function(x) iconv(x, to = "latin1", sub = "")))
    print("    - transforming to lowercase...")
    corpus <- tm_map(corpus, content_transformer(tolower))
    print("    - removing internet stuff...")
    corpus <- tm_map(corpus, content_transformer(removeInternetStuff))
    print("    - removing repeats...")
    corpus <- tm_map(corpus, content_transformer(removeRepeats))
    print("    - removing punctuation...")
    corpus <- tm_map(corpus, removePunctuation)
    print("    - expanding abbreviations...")
    corpus <- tm_map(corpus, content_transformer(expandAbbreviations))
    print("    - replacing special chars...")
    corpus <- tm_map(corpus, content_transformer(replaceSpecialChars))
    print("    - removing numbers...")
    corpus <- tm_map(corpus, removeNumbers)
    print("    - removing stopwords...")
    corpus <- tm_map(corpus, removeWords, stopwords(language = LOCALE))
    
    # save base result
    saveCorpus(corpus, "base_")
}
    
if (corpusExists("wrong_")) {
    corpus <- loadCorpus("wrong_")
    
} else {
    print("    - removing wrongly spelled or foreign words...")
    if (!file.exists("wrong-words.txt")) {
        wrong_words <- c()
        for (c in 1:length(corpus)) {
            wrong_words <- unique(c(wrong_words, unlist(hunspell(corpus[[c]]$content))))
        }
        writeLines(sort(wrong_words), "wrong-words.txt")
        print("         * generated wrongly spelled or foreign words")
        
    } else {
        wrong_words <- readLines("wrong-words.txt")
        print("         * loaded wrongly spelled or foreign words")
    }
    
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
    num_batch <- 100
    for (batch in sliceVector(wrong_words, 1000)) {
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
    
    saveCorpus(corpus, "wrong_")
}

if (corpusExists("stem_")) {
    corpus <- loadCorpus("stem_")
    
} else {
    print("    - stripping whitespace...")
    corpus <- tm_map(corpus, stripWhitespace)
    print("    - stemming...")
    corpus <- tm_map(corpus, stemDocument)
    
    saveCorpus(corpus, "stem_")
}

## 2. Filter profane words
if (corpusExists("profane_")) {
    corpus <- loadCorpus("profane_")
    
} else {
    print("Data cleaned, filtering profane words...")
    if (!file.exists("bad-words.txt")) {
        # see https://www.cs.cmu.edu/~biglou/resources/bad-words.txt
        download.file("https://www.cs.cmu.edu/~biglou/resources/bad-words.txt",
                      destfile = "bad-words.txt", method = "curl")
    }
    bad_words <- readLines("bad-words.txt")
    count <- 1
    for (batch in sliceVector(bad_words, 1500)) {
        corpus <- tm_map(corpus, removeWords, batch)
        print(paste("         * removed batch of profane words:", count))
        count <- count + 1
    }
    
    saveCorpus(corpus, "profane_")
}

saveCorpus(corpus, "final_")
