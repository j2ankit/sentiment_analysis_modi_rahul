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


consumerKey <- "Ec0nDR6Xg3iPH5vvA3ve1uFFB"
consumerSecret <- "mUTTi3ibNO7xQ39vMmU7F6aywZL4erkY1pAI06sDWgMsFjmQ8F"
accessToken <- "18930468-XxCb6iMsHLPd3GzlhieaV8M87eUh0JtYqmKhuSOsW"
accessTokenSecret <- "S94Ec1jQoTv6YxHhvgjI1K2sBdLvjQ3d3nrQPHOFfVJ2T"

setup_twitter_oauth(consumerKey, consumerSecret, accessToken, accessTokenSecret)

## Data extraction for modi

list_of_tags_modi <- list("@narendramodi", "@PMOIndia", "#Modi", "NaMo", "narendra modi", "Narendra Modi", "NARENDRA MODI", "NarendraModi", "#NarendraModi", "narendramodi", "#NaMo")
tweet.list.final.modi <- list(0)

for (i in 1:length(list_of_tags_modi)) {
  tweet.list <- searchTwitter( paste0(list_of_tags_modi[i]," -filter:retweets"), n= 10000,  lang = "en", since = '2018-02-01', until='2018-02-09', retryOnRateLimit = 120)
  #tweet.list.stripped <- strip_retweets(tweet.list, strip_manual = F, strip_mt = F)
  tweet.list.final.modi <- c(tweet.list.final.modi, tweet.list)
  rm("tweet.list")
}

tweet.list.final.modi.2<- tweet.list.final.modi[tweet.list.final.modi != "0"]
tweet.list.final.modi.df <- twListToDF(tweet.list.final.modi.2)
write.csv(tweet.list.final.modi.df, "tweet_list_final_modi.csv")


## Data extraction for Rahul Gandhi

# list_of_tags_rahul <- list()
# tweet.list.final.rahul <- list(0)
# 
# for (i in 1:length(list_of_tags_rahul)) {
#   tweet.list <- searchTwitter( paste0(list_of_tags_rahul[i]," -filter:retweets"), n= 10000,  lang = "en", since = '2018-02-01', until='2018-02-09', retryOnRateLimit = 120)
#   #tweet.list.stripped <- strip_retweets(tweet.list, strip_manual = F, strip_mt = F)
#   tweet.list.final.rahul <- c(tweet.list.final.rahul, tweet.list)
#   rm("tweet.list")
# }
# 
# tweet.list.final.rahul.2<- tweet.list.final.rahul[tweet.list.final.rahul != "0"]
# tweet.list.final.rahul.df <- twListToDF(tweet.list.final.rahul.2)
# write.csv(tweet.list.final.rahul.df, "tweet_list_final_rahul.csv")


############# SENTIMENT ANALYSIS WITH THE ABOVE DATA  ##############


data <- read.csv("tweet_list_final_modi.csv")
 data <- head(data,3)



## SENTIMENT SCORE = POSITIVE SCORE - NEGATIVE SCORE
# function score.sentiment
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  # Parameters
  # sentences: vector of text to score
  # pos.words: vector of words of postive sentiment
  # neg.words: vector of words of negative sentiment
  # .progress: passed to laply() to control of progress bar
  
  # create simple array of scores with laply
  scores = laply(sentences,
                 function(sentence, pos.words, neg.words)
                 {
                   # remove punctuation
                   sentence = gsub("[[:punct:]]", "", sentence)
                   # remove control characters
                   sentence = gsub("[[:cntrl:]]", "", sentence)
                   # remove digits?
                   sentence = gsub('\\d+', '', sentence)
                   # removing links and useless things -- code given by gango 
                   sentence = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", sentence)
                   sentence = gsub("[[:cntrl:]]", "", sentence)
                   sentence = gsub(" ?(f|ht)tp(s?)://(.*)[.][a-z]+", "", sentence)
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
                   word.list = str_split(sentence, "\\s+")
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

#  IMPORT POSITIVE AND NEGATIVE WORDS
pos.words = scan("positive_words.txt", what = 'character', comment.char = ";")
neg.words = scan("negative_words.txt", what = 'character', comment.char = ";")
data$text <- as.factor(data$text)
data.score <- score.sentiment(data$text, pos.words, neg.words, .progress ='text')

### change name of the file to be writte
write.csv(data.score, "TestScore.csv", row.names = F)

## PLOTTING THE OUTCOME
hist(data.score$score, xlab = "Score of Tweets", main = "Frequency of Twits sentiment")



########################### MODULE 3 ################################
############### TEXT MINING AND WORDCLOUND DRAWING IN R #############

### My code 

text <- readLines(file.choose())

docs <- Corpus(VectorSource(text))


toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
docs <- tm_map(docs, toSpace, "-")


# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))t_map
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
#docs <- tm_map(docs, stemDocument)


dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 20)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))




########################### THE END  ######################

