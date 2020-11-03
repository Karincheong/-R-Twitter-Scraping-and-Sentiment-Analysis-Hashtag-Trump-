# Twitter-Scraping-and-Sentiment-Analysis-Hashtag-Trump-

![image](https://user-images.githubusercontent.com/68969621/98043018-770a1880-1e1c-11eb-9c9c-be694b6169b8.png)

First is to develop a Twitter developer account, once this is achieved, then the security credential codes will then provide by the platform and is ready for the actual scraping process.

![image](https://user-images.githubusercontent.com/68969621/98043195-bf293b00-1e1c-11eb-9741-9f27c3c242ce.png)


# Set working directory 

getwd()


##Twitter Web scraping package  

![image](https://user-images.githubusercontent.com/68969621/98043259-ce0fed80-1e1c-11eb-9fc3-3d07c1e820e9.png)

#library (rtweet)
#library(openxlsx)
#library(xlsx)
#library(twitteR)
#library(ROAuth)
#library (rtweet)

# Set up Twitter token and authorisation 


twitter_token <- create_token(
app = "Trump get_tweets script",
consumer_key = "Key",
consumer_secret = "Key",
access_token = 'Key',
access_secret = 'Key',
set_renv = FALSE)

# Search @Trump, sample size = 1000 

Trump<- search_tweets("Trump", n=1000, include_rts=FALSE, retryonratelimit=TRUE, lang="en")

![image](https://user-images.githubusercontent.com/68969621/98043435-1d561e00-1e1d-11eb-89c3-8f071ac936c8.png)


# Into data frame

Trump<-twListToDF(Trump)
View(Trump)

![image](https://user-images.githubusercontent.com/68969621/98044123-4e831e00-1e1e-11eb-99ed-b9ae11a21ffd.png)


--------------------------------------------------
# Write into xlsx file 

Next is to export #Trump data frame data into a workable excel file. 

export(Trump,"DonaldTrump_Tweets.xlsx")

-----------------------------------------------------------
# Read DonaldTrump_Tweets.xlsx

Import #Trump data to working directory 

library(readxl)
Trump <- read_excel("DonaldTrump_Tweets.xlsx", 
                                     sheet = "Sheet 1")
View(Trump)

-----------------------------------------------------------
# Analysis #trump ##################################################

#package 
install.packages("rtweet")
install.packages("SnowballC")
install.packages('devtools')
install.packages("slam")
install.packages("wordcloud")
install.packages('tm')
install.packages("dplyr")
install.packages("tidytext")
install.packages("ggplot2")
install.packages("forestmangr")
install.packages("multcomp")
install.packages("purrr")
install.packages("twitteR", dependencies = TRUE)
install.packages("party", dependencies = TRUE)
library (rtweet)
library(SnowballC)
library(devtools)
library(slam)
library(wordcloud)
library(tm)
library(dplyr)
library(tidytext)
library(ggplot2)
library(forestmangr)
library(multcomp)
library(twitteR)
library(party)
library(tidyverse)

#----------------------------------

Trump$created_at<-as.Date(Trump$created_at)

# Remove retweets
Trump_C <- Trump[Trump$is_retweet==FALSE, ] 
# Remove replies
Trump_C<- subset(Trump_C, is.na(Trump_C$reply_to_status_id)) 

Trump_C <- Trump_C %>% arrange(-favorite_count)
Trump_C[1,5]
Trump_C <- Trump_C %>% arrange(-retweet_count)
Trump_C[1,5]  

#Keeping only the retweets
Trump_retweets <- Trump[Trump$is_retweet==TRUE,]
# Keeping only the replies
Trump_replies <- subset(Trump, !is.na(Trump$reply_to_status_id))

# Creating a data frame
data <- data.frame(
  category=c( "Retweets", "Replies"),
  count=c( 192, 120)
)

#remove punchuation and other symbols from text
Trump$text <-  gsub("https\\S*", "", Trump$text)
Trump$text <-  gsub("@\\S*", "", Trump$text) 
Trump$text  <-  gsub("amp", "", Trump$text) 
Trump$text  <-  gsub("[\r\n]", "", Trump$text)
Trump$text  <-  gsub("[[:punct:]]", "", Trump$text)

#Clean location info 
Trump$quoted_location<-  gsub("https\\S*", "", Trump$quoted_location)
Trump$quoted_location <-  gsub("@\\S*", "", Trump$quoted_location) 
Trump$quoted_location  <-  gsub("amp", "", Trump$quoted_location) 
Trump$quoted_location  <-  gsub("[\r\n]", "", Trump$quoted_location)
Trump$quoted_location  <-  gsub("[[:punct:]]", "", Trump$quoted_location)

#Clean source(device) info 
Trump$source<-  gsub("https\\S*", "", Trump$source)
Trump$source <-  gsub("@\\S*", "", Trump$source) 
Trump$source  <-  gsub("amp", "", Trump$source) 
Trump$source  <-  gsub("[\r\n]", "", Trump$source)
Trump$source  <-  gsub("[[:punct:]]", "", Trump$source)

#-------------------------------------------------------------------------

---------------------------------
#Remove certain words from source

Trump$source <- str_replace(Trump$source, "for", "") 
Trump$source <- str_replace(Trump$source, "Twitter", "") 
--------------------------------------------------------
------------------------------
#======================================================================
############Looking into location info 
# different location 
unique(Trump$quoted_location)

#[1] 406 unique share locations
length(unique(Trump$quoted_location))

# location word cloud to identify the different
# locations as they are inputted by users as strings
Trump$quoted_location <- as.character(Trump$quoted_location)

set.seed(1234)
wordcloud(Trump$quoted_location, min.freq=4, scale=c(5, .5), random.order=FALSE, rot.per=0.3, 
          colors=brewer.pal(8, "Dark2"))

## graph has added the function to show n number of locations, in this case 20
Trump %>%
  count(quoted_location, sort = TRUE) %>%
  mutate(quoted_location = reorder(quoted_location, n)) %>%
  top_n(20) %>%
  ggplot(aes(x = quoted_location, y = n)) +
  geom_col() +
  coord_flip() +
  labs(x = "Location",
       y = "Count",
       title = "Top 20 locations of Twitter users")

#==========================================================
# Type of device 
############Looking into device info 
# different devices (source) 
unique(Trump$source)

#[1] 130 unique device types 
length(unique(Trump$source))

# location word cloud to identify the different
# locations as they are inputted by users as strings
Trump$source <- as.character(Trump$source)

set.seed(1234)
wordcloud(Trump$source, min.freq=4, scale=c(5, .5), random.order=FALSE, rot.per=0.3, 
          colors=brewer.pal(8, "Dark2"))

#graph has added the function to show n number of locations, in this case 20
Trump %>%
  count(source, sort = TRUE) %>%
  mutate(source = reorder(source, n)) %>%
  top_n(20) %>%
  ggplot(aes(x = source, y = n)) +
  geom_col() +
  coord_flip() +
  labs(x = "Source",
       y = "Count",
       title = "Top 20 type of sources")


#######################################################################
#         list of tweets with the most amount of favourites          #
######################################################################

View(Trump$favorite_count >45,"TRUE")

sort(Trump$favorite_count)

View(Trump$favorite_count >200, TRUE)

sum(Trump$favorite_count >200, TRUE)


#===========
Trump %>%
count(favorite_count, sort.default(TRUE)) %>%
  mutate(favorite_count = reorder(favorite_count, n)) %>%
  na.omit() %>%
  top_n(10) %>%
  ggplot(aes(x = favorite_count, y = n)) +
  geom_col() +
  coord_flip() +
  labs(x = "Count",
       y = "Tweet",
       title = "Twitter favorite count ")

unique(Trump$favorite_count)
summary(Trump$favorite_count)
boxplot(Trump$favorite_count)
qqplot(Trump$retweet_count, Trump$favorite_count)
qqline(Trump$retweet_count, Trump$favorite_count)
#==================================
wilcox.test(Trump$retweet_count, Trump$favorite_count)

############Wilcoxon rank sum test with continuity correction

###   data:  Trump$retweet_count and Trump$favorite_count
###   W = 140205946, p-value < 2.2e-16
###   alternative hypothesis: true location shift is not equal to 0
# ==============================================
#Option2   
library(tm)
# build a corpus, and specify the source to be character vectors
myCorpus <- Corpus(VectorSource(Trump$text))
# convert to lower case
myCorpus <- tm_map(myCorpus, content_transformer(tolower))
# remove URLs
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeURL))
# remove anything other than English letters or space
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))
# remove stopwords
myStopwords <- c(setdiff(stopwords('english'), c("r", "big")),
                 "use", "see", "used", "via", "amp")
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
# remove extra whitespace
myCorpus <- tm_map(myCorpus, stripWhitespace)
# keep a copy for stem completion later
myCorpusCopy <- myCorpus

myCorpus <- tm_map(myCorpus, stemDocument) # stem words

wordcloud(myCorpus, min.freq=4, scale=c(5, .5), random.order=FALSE, rot.per=0.3, 
          colors=brewer.pal(8, "Dark2"))

#====================================================
#with Corpus
writeLines(strwrap(myCorpus[[8534]]$content, 60))
writeLines(strwrap(myCorpus[[7123]]$content, 60))

#without 2nd way of working 

writeLines(strwrap(Trump$text[8534], 60))
writeLines(strwrap(Trump$text[7123], 60))

#Look at tweet 8534 
Trump[8534, c("created_at", "text", "source", "display_text_width",
              "is_retweet")]

#sentiment analysis on myCorpus example from tweet #7123 
get_nrc_sentiment("you got me Way to not answer any questions and instead
                  attack the individual Sounds exactly like Trump Congrats")
#1 Anger 1 negative 
myCorpus<-as.character(myCorpus)
trump_sentiment<-get_nrc_sentiment(myCorpus)
sentimentscores<-data.frame(colSums(trump_sentiment[,]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL

library(ggplot2)
ggplot(data=sentimentscores,aes(x=sentiment,y=Score))+
  geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("Scores")+
  ggtitle("Total sentiment based on scores")+
  theme_minimal()

summary(sentimentscores$Score)
summary(trump_sentiment)
#=====================================================

library(tidytext)
library(stringr)
library(dplyr)
library(janeaustenr)

#nrc emotion lexicon
get_sentiments("nrc")
#nrc emotion #negative or positive 
get_sentiments("bing")
#with score 
get_sentiments("afinn")


tweets <- Trump %>%
  select(text) %>%
  unnest_tokens(word, text)
tweets <- tweets %>%
  anti_join(stop_words) %>%
  filter(!word =="trump" ) %>%
  filter(!word =="win" ) %>%
  filter(!word =="fake") %>%
  filter(!word =="war" ) %>%
  filter(!word =="bomb" ) %>%
  filter(!word =="im" ) %>%
  filter(!word =="it's" ) %>%
  filter(!word =="i'm " )

# bar chart of the most frequent words found in the tweets
tweets %>% 
  count(word, sort = TRUE) %>%
  top_n(20) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Most frequent words found in the tweets",
       subtitle = "Stop words removed from the list")

#word cloud for most appeared words 
tweets <- as.character(tweets)
tweets <- gsub("c\\(", " ", tweets)
set.seed(1234)
wordcloud(tweets, min.freq=4, scale=c(5, .1), random.order=FALSE, rot.per=.1, 
          colors=brewer.pal(9, "Set2"))

