
source('00_commons.R', echo = FALSE)
if (!corpusExists("final_")) {
    source('01_prepare-data.R', echo = FALSE)
    stopifnot(corpusExists("final_"))
}

######################
## Task 3: Model Data
######################

library(ngram)

corpus <- loadCorpus("final_")

# What are the frequencies of 2-grams and 3-grams in the data set? 

text <- concatenate(sapply(corpus, function(c) concatenate(c$content), USE.NAMES = FALSE))

ng2 <- ngram(text, n = 2)
pt2 <- get.phrasetable(ng2)
print(head(pt2, n = 25))

ng3 <- ngram(text, n = 3)
pt3 <- get.phrasetable(ng3)
print(head(pt3, n = 25))

