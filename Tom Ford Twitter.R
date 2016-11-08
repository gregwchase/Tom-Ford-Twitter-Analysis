# TOM FORD Twitter Analysis

# Software Requirements
# R, version 3.2.5 or greater.
# RStudio, version 1.0.40 or greater.

# Load Libraries
library(base64enc)
library(devtools)
library(httr)
library(stringr)
library(RColorBrewer)
library(tm)
library(twitteR)
library(wordcloud)

# Twitter Authentication (DEV)
auth <- setup_twitter_oauth(# Consumer Key,
                            # Consumer Secret Key,
                            # Access Token,
                            # Access Secret Token
                            )

# Request tweets using a hashtag
tf <- searchTwitter("#tomford", 
                    n = 10, # Request up to 5000 tweets per hour
                    lang = "en"
)


# Remove hyperlinks and any characters that aren't text.
tfText <- sapply(tf, function(x) x$getText())
tfText <- gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", "", tfText)
tfText <- gsub("(f|ht)tps(s?)://(.*)[.][a-z]+", "", tfText)
tfText <- gsub("https","",tfText)
tfText <- str_replace_all(tfText,"[^a-zA-Z\\s]", " ")

# Create the corpus
tfCorpus <- Corpus(VectorSource(tfText))
tfCorpus <- tm_map(tfCorpus,PlainTextDocument)
tdm <- TermDocumentMatrix(
        tfCorpus,
        control = list(
        stopwords = c("tomford",
            stopwords("english")))
)

# Count the word frequency, and what appears most often.
m <- as.matrix(tdm)
wordFrequency <- sort(rowSums(m), decreasing = TRUE)
wordFrequency <- wordFrequency
words <- data.frame(word = names(wordFrequency), frequency = wordFrequency)

# Create the wordcloud
tfWordcloud <- wordcloud(words$word, words$freq, random.order = FALSE,scale=c(3,0.5), 
                         colors = brewer.pal(8, "PiYG"))


# View the results of the top 10 words most used in tweets relating to requested hashtag.
words[1:10,]
