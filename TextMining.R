#install.packages("twitteR")
#install.packages("ROAuth")
#install.packages("tm")
#install.packages("stringi")
#install.packages("topicmodels")
#install.packages("wordcloud")
#install.packages("RColorBrewer")
#install.packages("qdap")
#install.packages("dplyr")
#install.packages("streamgraph")
#install.packages("xts")
library(ggplot2)
library(qdap)
library(dplyr)
library(SnowballC)
library(wordcloud)
library(topicmodels)
library(stringi)
library(data.table)
#install.packages("date")
library(date)
library(tm)
library("twitteR")
library("ROAuth")
#install.packages("base64enc")
library(base64enc)
#install.packages("httr")
library(httr)
#install.packages("devtools")
library(devtools)
library(sentiment)
devtools::install_github('sentiment140', 'okugami79')
#devtools::install_github("jrowen/twitteR", ref = "oauth_httr_1_0")
# Download "cacert.pem" file
#devtools::install_version("httr", version="1.3.1", repos="http://cran.us.r-project.org")
#packageurl <- "https://cran.r-project.org/src/contrib/Archive/httr/httr_1.2.1.tar.gz"
#install.packages(packageurl, repos=NULL, type="source")

download.file(url="http://curl.haxx.se/ca/cacert.pem",destfile="cacert.pem")
setwd("C:/Users/Home/Desktop/R_Session")
# Set constant requestURL
requestURL <- "https://api.twitter.com/oauth/request_token"
# Set constant accessURL
accessURL <- "https://api.twitter.com/oauth/access_token"
# Set constant authURL
authURL <- "https://api.twitter.com/oauth/authorize"
consumer_key <- "O7MWF0zm5QyLE0zIZW2lr6Yul"
consumer_secret='beQHDvDpT5VjccpjcUMiGgQoP306LfdwYAaWDu4VgYjW8CoxCU'
access_token='924243786980409345-F3mx0EBOB4k0KU8cwOz7NguHNw9gzSW'
access_secret='8SiawjDAr9EPOLuF19I2kDENdCVgYq4aTADW1Y4Y4RtNy'

twitteR::setup_twitter_oauth(consumer_key,consumer_secret,access_token,access_secret)

search.string <- "#Tesla"
no.of.tweets <- 1000

tweets.df <- twitteR::searchTwitter(search.string,n=no.of.tweets,lang="en")
tweets_DF <- do.call("rbind", lapply(tweets.df, as.data.frame))
#Create Text file
#tweets.df_text <- sapply(tweets.df, function(x) x$getText())
#Create Corpus
tweets.df_text_corpus <- Corpus(VectorSource(tweets_DF$text))
#Convert to Lower Case
tweets.df_text_corpus <- tm_map(tweets.df_text_corpus, content_transformer(stri_trans_tolower))
#Remove links
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)  
tweets.df_text_corpus <- tm_map(tweets.df_text_corpus, content_transformer(removeURL))
#Remove anything except the english language and space 
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)   
tweets.df_text_corpus <- tm_map(tweets.df_text_corpus, content_transformer(removeNumPunct))
#Remove stop words
tweetstopWords<- c((stopwords('english')),c("rt"))
tweets.df_text_corpus<- tm_map(tweets.df_text_corpus,removeWords , tweetstopWords) 
#Remove single letter words
removeSingle <- function(x) gsub(" . ", " ", x)   
tweets.df_text_corpus <- tm_map(tweets.df_text_corpus, content_transformer(removeSingle))
#remove extra white spaces
tweets.df_text_corpus<- tm_map(tweets.df_text_corpus, stripWhitespace) 
#keep a corpus copy
tweets.df_text_corpus_Copy<- tweets.df_text_corpus
#Stem the words in corpus
tweets.df_text_corpus<-tm_map(tweets.df_text_corpus, stemDocument)
writeLines(strwrap(tweets.df_text_corpus[[250]]$content,60))
#Build a Term document Matrix
tdm <- TermDocumentMatrix(tweets.df_text_corpus, control = list(wordLengths = c(1, Inf)))
tdm
idx <- which(dimnames(tdm)$Terms %in% c("tesla", "car","electric"))
as.matrix(tdm[idx,21:60])
#Find the terms used most frequently
(freq.terms <- findFreqTerms(tdm, lowfreq = 50))
term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq > 50)
df <- data.frame(term = names(term.freq), freq= term.freq)
#plotting the graph of frequent terms
ggplot(df, aes(reorder(term, freq),freq)) + theme_bw() + geom_bar(stat = "identity")  + coord_flip() +labs(list(title="Term Frequency Chart", x="Terms", y="Term Counts")) 
#calculate the frequency of words and sort it by frequency and setting up the Wordcloud
word.freq <-sort(rowSums(as.matrix(tdm)), decreasing= F)
pal<- brewer.pal(8, "Dark2")
wordcloud(words = names(word.freq), freq = word.freq, min.freq = 2, random.order = F, colors = pal, max.words = 100)
# Identify and plot word correlations. For example - electr
data("tweets.df_text_corpus")
tdm <- TermDocumentMatrix(tweets.df_text_corpus)

toi <- "electr" # term of interest
corlimit <- 0.25 #  lower correlation bound limit.
ele_0.25 <- data.frame(corr = findAssocs(tdm, toi, corlimit)[[1]],
                      terms = names(findAssocs(tdm, toi, corlimit)[[1]]))
ele_0.25$terms <- factor(ele_0.25$terms ,levels = ele_0.25$terms)

ggplot(ele_0.25, aes( y = terms  ) ) +
  geom_point(aes(x = corr), data = ele_0.25) +
  xlab(paste0("Correlation with the term ", "\"", toi, "\""))

#data("tweets.df_text_corpus")
#WordCorr <- apply_as_df(tweets.df_text_corpus[1:500],word_cor,word = "lithium",r=.25)
#plot(WordCorr)

#qheat(vect2df(tweets.df_text_corpus[1:500], "word", "cor"), values=TRUE, high="red",
#      digits=2, order.by ="cor", plot = FALSE) + coord_flip()

#Messages with word electr
#df <- data.frame(text=sapply(tweets.df_text_corpus, `[[`, "content"), stringsAsFactors=FALSE)
#head(unique(df[grep("electr", df$text), ]), n=10)

# Find association with a specific keyword in the tweets like electr, lithium
findAssocs(tdm, "lithium", 0.2)
findAssocs(tdm, "electr", 0.2)

# Topic Modelling to identify latent/hidden topics using LDA technique
dtm <- as.DocumentTermMatrix(tdm)

rowTotals <- apply(dtm , 1, sum)

NullDocs <- dtm[rowTotals==0, ]
dtm   <- dtm[rowTotals> 0, ]

if (length(NullDocs$dimnames$Docs) > 0) {
  tweets.df <- tweets.df[-as.numeric(NullDocs$dimnames$Docs),]
}

lda <- LDA(dtm, k = 5) # find 5 topic
term <- terms(lda, 7) # first 7 terms of every topic
(term <- apply(term, MARGIN = 2, paste, collapse = ", "))


topics<- topics(lda)
topics<- data.frame(date=as.IDate(tweets_DF$created), topic = topics)
qplot (date, ..count.., data=topics, geom ="density", fill= term[topic], position="stack")


posText <- read.delim("positive-words.txt", header=FALSE, stringsAsFactors=FALSE)
posText <- posText$V1
posText <- unlist(lapply(posText, function(x) { stringr::str_split(x, "\n") }))
negText <- read.delim("negative-words.txt", header=FALSE, stringsAsFactors=FALSE)
negText <- negText$V1
negText <- unlist(lapply(negText, function(x) { stringr::str_split(x, "\n") }))
pos.words = c(posText)
neg.words = c(negText)


score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  # Parameters
  # sentences: vector of text to score
  # pos.words: vector of words of positive sentiment
  # neg.words: vector of words of negative sentiment
  # .progress: passed to laply() to control of progress bar
  # create a simple array of scores with laply
  scores = laply(sentences,
                 function(sentence, pos.words, neg.words)
                 {
                   # remove punctuation
                   sentence = gsub("[[:punct:]]", "", sentence)
                   # remove control characters
                   sentence = gsub("[[:cntrl:]]", "", sentence)
                   # remove digits?
                   sentence = gsub('\\d+', '', sentence)
                   # define error handling function when trying tolower
                   tryTolower = function(x)
                   {
                     # create missing value
                     y = NA
                     # tryCatch error
                     try_error = tryCatch(tolower(x), error=function(e) e)
                     # if not an error
                     if (!inherits(try_error, "error"))
                       y = tolower(x)
                     # result
                     return(y)
                   }
                   # use tryTolower with sapply 
                   sentence = sapply(sentence, tryTolower)
                   # split sentence into words with str_split (stringr package)
                   word.list = stringr::str_split(sentence, "\\s+")
                   words = unlist(word.list)
                   # compare words to the dictionaries of positive & negative terms
                   pos.matches = match(words, pos.words)
                   neg.matches = match(words, neg.words)
                   # get the position of the matched term or NA
                   # we just want a TRUE/FALSE
                   pos.matches = !is.na(pos.matches)
                   neg.matches = !is.na(neg.matches)
                   # final score
                   score = sum(pos.matches) - sum(neg.matches)
                   return(score)
                 }, pos.words, neg.words, .progress=.progress )
  # data frame with scores for each sentence
  scores.df = data.frame(text=sentences, score=scores)
  return(scores.df)
}
result = score.sentiment(tweets_DF$text, pos.words,neg.words)


result$positive <- as.numeric(result$score >0)
result$negative <- as.numeric(result$score >0)
result$neutral <- as.numeric(result$score==0)


hist(result$score,col ="blue", main ="Score of tweets", ylab =" Count of tweets", Xlab="Score")
count(result$score)
qplot(result$score,xlab = "Score of tweets")

