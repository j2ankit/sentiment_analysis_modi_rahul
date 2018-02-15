


#################### FIRST DEV ATTEMPT ###################################################


# build a corpus, and specify the source to be character vectors
myCorpus <- Corpus(VectorSource(data$text))
# convert to lower case # myCorpus <- tm_map(myCorpus, tolower)
# tm v0.6

myCorpus <- tm_map(myCorpus, content_transformer(tolower))
# remove punctuation
myCorpus <- tm_map(myCorpus, removePunctuation) 
# remove numbers
myCorpus <- tm_map(myCorpus, removeNumbers)
# remove URLs
removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
### myCorpus <- tm_map(myCorpus, removeURL, lazy=TRUE) 
myCorpus <- tm_map(myCorpus, content_transformer(removeURL))  #??
# add two extra stop words: 'available' and 'via'
myStopwords <- c(stopwords("english"), "available", "via")
# remove 'r' and 'big' from stopwords
myStopwords <- setdiff(myStopwords, c("r", "big"))
# remove stopwords from corpus
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
#
myCorpus <- tm_map(myCorpus, stripWhitespace)

#￼# keep a copy of corpus to use later as a dictionary for stem
# completion
myCorpusCopy <- myCorpus
# stem words
myCorpus <- tm_map(myCorpus, stemDocument)


########### ABG DEV ##############################################################################


docs <- Corpus(VectorSource(text))


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



############# third dev attempt  ###########################################################


data <- read.csv("tweet_list_final_modi_v2.csv")


data <- head(data,100)
data.text <- data$text
data.text <- as.matrix(data.text)

removeURL <- function(x) gsub("...?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", "", x)
data.text <- apply(data.text, c(1,2), removeURL)

# remove_non_ascii <- function(x)gsub("[^\x01-\x7F]", "", x)
# data.text <- apply(data.text, c(1,2), remove_non_ascii)

data.text <- iconv(data.text, "UTF-8", "ASCII")

# build a corpus, and specify the source to be character vectors
myCorpus <- Corpus(VectorSource(data.text))
# convert to lower case # myCorpus <- tm_map(myCorpus, tolower)
# tm v0.6



myCorpus <- tm_map(myCorpus, content_transformer(tolower))
# remove punctuation
myCorpus <- tm_map(myCorpus, removePunctuation) 
# remove numbers
myCorpus <- tm_map(myCorpus, removeNumbers)
# remove URLs
# removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
# ### myCorpus <- tm_map(myCorpus, removeURL, lazy=TRUE) 
# myCorpus <- tm_map(myCorpus, content_transformer(removeURL))  
# add two extra stop words: 'available' and 'via'
myStopwords <- c(stopwords("english"), "available", "via")
# remove stopwords from corpus
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
#
myCorpus <- tm_map(myCorpus, stripWhitespace)

#￼# keep a copy of corpus to use later as a dictionary for stem
# completion
myCorpusCopy <- myCorpus
# stem words
myCorpus <- tm_map(myCorpus, stemDocument)


dtm <- TermDocumentMatrix(myCorpus)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 20)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

library('syuzhet')

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

plot(td_new2)


# # split sentence into words with str_split (stringr package)
# word.list = str_split(myCorpus, "\\s+")
# words = unlist(word.list)
# 
# pos.words = scan("positive_words.txt", what = 'character', comment.char = ";")
# neg.words = scan("negative_words.txt", what = 'character', comment.char = ";")
# 
# 
# 
# # compare words to the dictionaries of positive & negative terms
# pos.matches = match(words, pos.words)
# neg.matches = match(words, neg.words)
# 
# # get the position of the matched term or NA
# # we just want a TRUE/FALSE
# pos.matches = !is.na(pos.matches)
# neg.matches = !is.na(neg.matches)
# 
# # final score
# score = sum(pos.matches) - sum(neg.matches)
# (score)
# 
# 

