#calling libraries
library(twitteR)
library(ROAuth)
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)


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
