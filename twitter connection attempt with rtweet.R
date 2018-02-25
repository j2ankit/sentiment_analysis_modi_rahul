############### twitter connection #####################33


library(rtweet)

consumerKey <- "EnnCXl7htR4mgBCl22JBxJ2eL"
consumerSecret <- "npqmCYva5Eh5VC0jI2UuqEpT3Rsgb42v3PDtUAqT31vBKNq5Rg"


download.file(url="http://curl.haxx.se/ca/cacert.pem",destfile="cacert.pem")

requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"


twitCred <- OAuthFactory$new(consumerKey=consumerKey,
                             consumerSecret=consumerSecret,
                             requestURL=requestURL,
                             accessURL=accessURL,
                             authURL=authURL)

twitter_tokens <- create_token(app = "bidm_project",consumer_key = consumerKey,consumer_secret = consumerSecret)
# 
# twitCred$handshake(cainfo="cacert.pem")
# 
# registerTwitterOAuth(twitCred)
# 
# save(list="twitCred", file="twitteR_credentials")
# 
# load("twitteR_credentials")
# registerTwitterOAuth(twitCred)

# 
# list_of_tags_modi <- list("@narendramodi", "@PMOIndia", "#Modi", "NaMo", "narendra modi", "Narendra Modi", "NARENDRA MODI", "NarendraModi", "#NarendraModi", "narendramodi", "#NaMo")
# tweet.list.final.modi <- list(0)
# 
# for (i in 1:length(list_of_tags_modi)) {
#   tweet.list <- search_tweets( list_of_tags_modi[i], n= 10, type = "recent", include_rts = F)
#   tweet.list.final.modi <- c(tweet.list.final.modi, tweet.list)
#   rm("tweet.list")
# }

tweet.list <- search_tweets( "@narendramodi OR @PMOIndia OR #Modi OR NaMo OR narendra modi OR Narendra Modi OR NARENDRA MODI OR NarendraModi OR #NarendraModi OR narendramodi OR #NaMo", n= 20000, type = "recent", include_rts = F, retryonratelimit = TRUE)
write_as_csv(tweet.list, "tweet_list_final_modi_v2.csv")


tweet.list.RaGa <- search_tweets( "@OfficeOfRG OR #RahulGandhi OR RaGa OR rahul gandhi OR Rahul Gandhi OR RAHUL GANDHI OR RahulGandhi OR #RahulGandhi OR rahulgandhi OR #RaGa", n= 20000, type = "recent", include_rts = F, retryonratelimit = TRUE)
write_as_csv(tweet.list.RaGa, "tweet_list_final_RaGa.csv")