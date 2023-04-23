# read data

df <- read.csv("/Users/christos/Downloads/Reviews and comments.csv")
str(df)

# build corpus

library(tm)

corpus <- iconv(df$CONTENT)
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])

# clean text
corpus <- tm_map(corpus, tolower)
inspect(corpus[1:5])

corpus <- tm_map(corpus, removePunctuation)
inspect(corpus[1:5])

corpus <- tm_map(corpus, removeNumbers)
inspect(corpus[1:5])

cleanset <- tm_map(corpus, removeWords, stopwords('english'))
inspect(cleanset[1:5])

cleanset <- tm_map(cleanset, removeWords, c('aapl', 'apple'))
cleanset <- tm_map(cleanset, gsub,
                   pattern = 'stocks',
                   replacement = 'stock')
cleanset <- tm_map(cleanset, stemDocument)
cleanset <- tm_map(cleanset, stripWhitespace)
inspect(cleanset[1:5])


# term document matrix
#  matter the incidence of every word, to perceive famous or trending topics
tdm <- TermDocumentMatrix(cleanset)
tdm <- as.matrix(tdm)
tdm[1:10, 1:20]

w <- rowSums(tdm)
w <- subset(w, w>=25)


# most frequent words
barplot(w[order(w,decreasing = TRUE)],
        las = 2,
        #col = rainbow(50)
        )



#word cloud

library(wordcloud)
w <- sort(rowSums(tdm), decreasing = TRUE)
set.seed(222)
wordcloud(words = names(w),
          freq = w,
          max.words = 150,
          random.order = F,
          min.freq = 5,
          colors = brewer.pal(8, 'Dark2'),
          scale = c(5, 0.3),
          rot.per = 0.7)


library(wordcloud2)
w <- data.frame(names(w), w)
colnames(w) <- c('word', 'freq')
wordcloud2(w,
           size = 0.7,
           shape = 'triangle',
           rotateRatio = 0.5,
           minSize = 1)

# sentiment analysis

library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

reviews <- iconv(df$CONTENT)

s <- get_nrc_sentiment(reviews)
head(s)

# plots

barplot(

  colSums(s),
        las = 1,
        #col = rainbow(10),
        ylab = 'Count',
        main = 'Sentiment Scores Reviews')


barplot(
  sort(colSums(prop.table(s[, 1:10]))), 
  horiz = TRUE, 
  cex.names = 0.7, 
  las = 1, 
  main = "Emotions in Reviews", xlab="Percentage"
)


