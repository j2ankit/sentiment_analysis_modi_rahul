## Sentiment Analysis
## BIDM Project
## Created by - Faizan Sarwar, Annirudha G, Ankit Jetwani
## Date - 2018-feb-15

#calling libraries
library(twitteR)
library(ROAuth)
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(syuzhet)
library(RColorBrewer)
library(tm)          
library(wordcloud)   
library(SnowballC)
library(RCurl)       
library(rtweet)
library(RTextTools)
library(caTools)
library(e1071)


######### MODULE 1 - Extracting Twitter Data #####################

##deriving the keys from twitter app

consumerKey <- "EnnCXl7htR4mgBCl22JBxJ2eL"
consumerSecret <- "npqmCYva5Eh5VC0jI2UuqEpT3Rsgb42v3PDtUAqT31vBKNq5Rg"


download.file(url="http://curl.haxx.se/ca/cacert.pem",destfile="cacert.pem")

requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"

# setting up connection 

twitCred <- OAuthFactory$new(consumerKey=consumerKey,
                             consumerSecret=consumerSecret,
                             requestURL=requestURL,
                             accessURL=accessURL,
                             authURL=authURL)

twitter_tokens <- create_token(app = "bidm_project",consumer_key = consumerKey,consumer_secret = consumerSecret)

##Extracting data using pre determined key words

tweet.list <- search_tweets( "@narendramodi OR @PMOIndia OR #Modi OR NaMo OR narendra modi OR Narendra Modi OR NARENDRA MODI OR NarendraModi OR #NarendraModi OR narendramodi OR #NaMo", n= 20000, type = "recent", include_rts = F, retryonratelimit = TRUE)
write_as_csv(tweet.list, "tweet_list_final_modi_v2.csv")

tweet.list.RaGa <- search_tweets( "@OfficeOfRG OR #RahulGandhi OR RaGa OR rahul gandhi OR Rahul Gandhi OR RAHUL GANDHI OR RahulGandhi OR #RahulGandhi OR rahulgandhi OR #RaGa", n= 20000, type = "recent", include_rts = F, retryonratelimit = TRUE)
write_as_csv(tweet.list.RaGa, "tweet_list_final_RaGa.csv")


##### MODULE 2 - Cleaning the Data #######################################

## Note - Currently the data analyses only sentiment related to Modi
##        Need to build data aggregation code for analysis of all leaders
data <- read.csv("tweet_list_final_modi_v2.csv")

## Note - uncomment the below line of code for development purpose
data <- head(data,100)
data.text <- data$text
data.text <- as.matrix(data.text)

# removing URL
removeURL <- function(x) gsub("...?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", "", x)
data.text <- apply(data.text, c(1,2), removeURL)

# remove_non_ascii <- function(x)gsub("[^\x01-\x7F]", "", x)
# data.text <- apply(data.text, c(1,2), remove_non_ascii)

## removing emoticons and non ascii characters
data.text <- iconv(data.text, "UTF-8", "ASCII")

# build a corpus, and specify the source to be character vectors
myCorpus <- Corpus(VectorSource(data.text))

# convert to lower case 
myCorpus <- tm_map(myCorpus, content_transformer(tolower))

# remove punctuation
myCorpus <- tm_map(myCorpus, removePunctuation) 

# remove numbers
myCorpus <- tm_map(myCorpus, removeNumbers)

# add two extra stop words: 'available' and 'via'
myStopwords <- c(stopwords("english"), "available", "via")
# remove stopwords from corpus
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)

# stripping white space
myCorpus <- tm_map(myCorpus, stripWhitespace)

#￼# keep a copy of corpus to use later as a dictionary for stem completion
myCorpusCopy <- myCorpus
# stem words
myCorpus <- tm_map(myCorpus, stemDocument)


######### MODULE 3 - Building Word Cloud and Other Viz ######################

dtm <- TermDocumentMatrix(myCorpus)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)

wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


########## MODULE 4 - Sentiment Analysis using NRC Lexicon #################

tweet.senti <- sapply(myCorpus, as.character)

d<-get_nrc_sentiment(tweet.senti)
td<-data.frame(t(d))

td_new <- data.frame(rowSums(td[2:length(tweet.senti)]))
#The function rowSums computes column sums across rows for each level of a grouping variable.

#Transformation and  cleaning
names(td_new)[1] <- "count"
td_new <- cbind("sentiment" = rownames(td_new), td_new)
rownames(td_new) <- NULL
td_new2<-td_new[1:8,]

# Plotting the new sentiment
plot(td_new2)

## Preparing the data for machine learning algo
## The data will be test and train at 80:20 ratio


tweet.senti.df <- as.data.frame(tweet.senti)
classification.df <- as.data.frame(d)

data.model <- cbind(tweet.senti.df,classification.df)
data.model <- data.model[,c(1,10:11)]
names(data.model)[1] <- "tweet"

data.model <- filter(data.model, tweet != "NA")

data.model$sentiment <- (data.model$positive - data.model$negative)
data.model <- data.model[,c(1,4)]

data.model.pos <- filter(data.model, sentiment > 0)
data.model.neg <- filter(data.model, sentiment < 0)

data.model.pos$sentiment <- "positive"
data.model.neg$sentiment <- "negative"

## creating test train dataset

sample = sample.split(data.model.pos$tweet, SplitRatio = .8)
data.model.pos.train = subset(data.model.pos, sample == TRUE)
data.model.pos.test  = subset(data.model.pos, sample == FALSE)

sample = sample.split(data.model.neg$tweet, SplitRatio = .8)
data.model.neg.train = subset(data.model.neg, sample == TRUE)
data.model.neg.test  = subset(data.model.neg, sample == FALSE)
  
data.train <- rbind(data.model.pos.train,data.model.neg.train)
data.test <- rbind(data.model.pos.test,data.model.neg.test)

data.train$sentiment <- as.factor(data.train$sentiment)
data.test$sentiment <- as.factor(data.test$sentiment)

###Modelling Naive Bayes


model <- naiveBayes(data.train$sentiment ~ data.train$tweet, data = data.train)

pred <- predict(model,data.test)


table(data.test$sentiment, pred)
recall_accuracy(data.test$sentiment, pred)
