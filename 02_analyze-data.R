
#####################################
## Task 2: Exploratory Data Analysis
#####################################

library(data.table)
library(dplyr)
library(ggplot2)
library(wordcloud2)

source('project/00_commons.R', echo = FALSE)
if (!corpusExists("final_")) {
    source('project/01_prepare-data.R', echo = FALSE)
    stopifnot(corpusExists("final_"))
}

## START

corpus <- loadCorpus("final_")

# 1. Some words are more frequent than others - what are the distributions of word frequencies? 

corpusDTM <- TermDocumentMatrix(corpus)
word_matrix <- sort(rowSums(as.matrix(corpusDTM)), decreasing = TRUE)
word_data <- data.table(word = names(word_matrix), freq = word_matrix)
# filter stop words from word data
word_data <- word_data[!(word %in% stemDocument(stopwords(LANG))),]
print(head(word_data, 25))

# distribution of word frequencies
print(summary(word_data$freq))

tot_words <- nrow(word_data)
tot_freq_words <- sum(word_data$freq)
print(sprintf("Corpus has %d unique words and %d in total", tot_words, tot_freq_words))

# word cloud
set.seed(1234)
wordcloud2(data = word_data)

# How many unique words do you need in a frequency sorted dictionary to cover 
# 50% of all word instances in the language? 90%?
freqs <- c()
percents <- c()
for (coverage in c(0.5, 0.75, 0.9, .95)) {
    freq_words <- 0
    num_words <- 0
    while (freq_words < tot_freq_words * coverage) {
        num_words <- num_words + 1
        freq_words <- freq_words + word_data$freq[num_words]
    }
    print(sprintf("%d unique words (of %d words in dictionary) cover %d%% of all word frequencies in corpus, or %.1f%% of dictionary", 
                  num_words, tot_words, coverage * 100, num_words * 100 / tot_words))
    freqs <- c(freqs, num_words)
    percents <- c(percents, num_words * 100 / tot_words)
}
cover_data <- data.table(coverage = c(0.5, 0.75, 0.9, .95), 
                         num_words = freqs, 
                         percents = percents,
                         min_freq = word_data$freq[freqs])
print(cover_data)

p <- ggplot(cover_data, 
            aes(x = sprintf("%d%%", coverage * 100), y = num_words)) + 
    theme_bw() + theme(legend.position = "none", 
                       panel.grid.major = element_blank(), 
                       panel.grid.minor = element_blank()) + 
    geom_col(fill = "dodgerblue") + 
    geom_text(aes(label = sprintf("%d (%.1f%%)", num_words, percents)), nudge_y = 125) + 
    labs(title = "Word Coverage") + xlab("Coverage (%)") + ylab("Number of Words")
print(p)

# Can you think of a way to increase the coverage -- 
# identifying words that may not be in the corpora or using a smaller number 
# of words in the dictionary to cover the same number of phrases?

# - 11% of unique words cover 90%, 19% cover 95% of all the words in the corpus.
# - To be within this set, a word has to occur at least 1673 (90%) or 625 (95%) times
# - So we could remove all words that are not within the coverage or occur less than for example 500 times.

main_word_data <- word_data[1:max(cover_data$num_words),]
fwrite(main_word_data, file = paste(output_dir, "/main_word_data.csv", sep = ""))

# top words
top_words <- 20
p <- ggplot(main_word_data[1:top_words,], aes(x = factor(word, levels = word), y = freq)) + 
    theme_bw() + theme(legend.position = "none", 
                       panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                       axis.title.x = element_blank(), axis.text.y = element_blank()) + 
    geom_col(fill = "dodgerblue") + 
    geom_text(aes(label = sprintf("%.0fK", freq/1000)), nudge_y = 4000) + 
    labs(title = sprintf("Top %d Words", top_words)) + ylab("Frequency")
print(p)

# distribution of frequencies
main_word_data <- main_word_data %>%
    mutate(bin = ifelse(freq > 20000, "[20001,)",
                        ifelse(freq > 10000, "[10001,20000]",
                               ifelse(freq > 5000, "[5001,1 10000]",
                                      ifelse(freq > 2500, "[2501, 5000]",
                                             ifelse(freq > 1000, "[1001, 2500]",
                                                    ifelse(freq > 500, "[501, 1000]", "[0, 500]")))))))

word_freqs <- group_by(main_word_data, bin) %>%
    summarise(counts = n())

p <- ggplot(word_freqs, aes(x = factor(bin, levels = c("[0, 500]", "[501, 1000]", "[1001, 2500]", "[2501, 5000]", "[5001,1 10000]", "[10001,20000]", "[20001,)")), 
                            y = counts)) + 
    theme_bw() + theme(legend.position = "none", 
                       panel.grid.major = element_blank(), 
                       panel.grid.minor = element_blank()) +
    geom_col(fill = "dodgerblue") +
    geom_text(aes(label = sprintf("%d (%.1f%%)", counts, counts*100/tot_words)), nudge_y = 30) +
    labs(title = "Distribution of Word Frequency") + xlab("Frequency") + ylab("Number of Words")
print(p)
