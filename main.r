#install
install.packages('tm')
install.packages('wordcloud')
install.packages('wordcloud2')
install.packages('syuzhet')
install.packages('lubridate')
install.packages('ggplot2')
install.packages('scales')
install.packages('reshape2')
install.packages('dplyr')

#Load
library(tm)
library(wordcloud)
library(wordcloud2)
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

# Read CSV 
apple <- read.csv(file.choose(), header = TRUE)
str(apple)


# Build the corpus 
corpus <- iconv(apple$text, to = "utf-8")
corpus <- Corpus(VectorSource(corpus))

# Text Cleaning
corpus <- tm_map(corpus, content_transformer(tolower))           
corpus <- tm_map(corpus, removePunctuation)                      
corpus <- tm_map(corpus, removeNumbers)                         
corpus <- tm_map(corpus, removeWords, stopwords('english'))     

# Custom cleaning
removeURL <- function(x) gsub('http[[:alnum:]]*', '', x)          
corpus <- tm_map(corpus, content_transformer(removeURL))
corpus <- tm_map(corpus, removeWords, c('aapl', 'apple'))         
corpus <- tm_map(corpus, gsub, pattern = 'stocks', replacement = 'stock')  
corpus <- tm_map(corpus, stripWhitespace)                      

# Term-document matrix
tdm <- TermDocumentMatrix(corpus)
tdm <- as.matrix(tdm)

# Bar plot for word frequency
w <- rowSums(tdm)
w <- subset(w, w >= 25)                                         
barplot(w, las = 2, col = rainbow(50), ylab = 'Frequency', main = 'Word Frequency in Tweets')


# Sentiment Analysis
tweets <- iconv(apple$text, to = 'utf-8')                     
s <- get_nrc_sentiment(tweets)                                     
head(s)

# Example sentiment for specific text
print(tweets[4])
get_nrc_sentiment('delay')

# Sentiment scores bar plot
barplot(colSums(s), las = 2, col = rainbow(10), ylab = 'Count', main = 'Sentiment Scores for Apple Tweets')


#first run apple.csv for before sales
#second run apple2.csv for after sales