# Twitter-Scraping-and-Sentiment-Analysis-Hashtag-Trump-

![image](https://user-images.githubusercontent.com/68969621/98050972-c6efdc00-1e2a-11eb-8f0c-d2ff3dcb77db.png)

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

![image](https://user-images.githubusercontent.com/68969621/98044397-b5a0d280-1e1e-11eb-9598-625d4485114b.png)


-----------------------------------------------------------
# Analysis Trump 

# Package used 

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
# Changing Class type 

Initially, the “created_at” column having multiple information, next is to change the class type “POSIXct” to “as.date”. This can immediately take away the timing information. 

Trump$created_at<-as.Date(Trump$created_at)

![image](https://user-images.githubusercontent.com/68969621/98044469-d79a5500-1e1e-11eb-9211-707bc57448be.png)


![image](https://user-images.githubusercontent.com/68969621/98044476-db2ddc00-1e1e-11eb-8710-1968a667e89e.png)

# Remove certain words from source
Subset “Twitter” and “for” words from source column and turn that into empty character, by using the function “str_replace”

Trump$source <- str_replace(Trump$source, "for", "") 

Trump$source <- str_replace(Trump$source, "Twitter", "") 

# Subset punctuation and symbol from text column
Here, we have prepared two different methods for subset the punctuation and symbols from the text (tweet message) columns. 

# Method 1 –Gsub

By using the function (gsub) to subset any unnecessary symbols or punctuation from the “text” columns from Trump dataset. As shown in Figure X, all the necessary characters have been removed. 

# Remove punchuation and other symbols from text
Trump$text <-  gsub("https\\S*", "", Trump$text)
Trump$text <-  gsub("@\\S*", "", Trump$text) 
Trump$text  <-  gsub("amp", "", Trump$text) 
Trump$text  <-  gsub("[\r\n]", "", Trump$text)
Trump$text  <-  gsub("[[:punct:]]", "", Trump$text)

# Before removal -Example
![image](https://user-images.githubusercontent.com/68969621/98044942-a0787380-1e1f-11eb-9a44-9f35bb3245af.png)

# After removal -Example
![image](https://user-images.githubusercontent.com/68969621/98044950-a2dacd80-1e1f-11eb-9a33-30dd97f8e01d.png)


# Clean location info 
Trump$quoted_location<-  gsub("https\\S*", "", Trump$quoted_location)
Trump$quoted_location <-  gsub("@\\S*", "", Trump$quoted_location) 
Trump$quoted_location  <-  gsub("amp", "", Trump$quoted_location) 
Trump$quoted_location  <-  gsub("[\r\n]", "", Trump$quoted_location)
Trump$quoted_location  <-  gsub("[[:punct:]]", "", Trump$quoted_location)

# Clean source(device) info 
Trump$source<-  gsub("https\\S*", "", Trump$source)
Trump$source <-  gsub("@\\S*", "", Trump$source) 
Trump$source  <-  gsub("amp", "", Trump$source) 
Trump$source  <-  gsub("[\r\n]", "", Trump$source)
Trump$source  <-  gsub("[[:punct:]]", "", Trump$source)

# Method 2 – Create corpus

•	Use the Function (tm), first is to create a corpus by inputting the Text column into the character vectors.  

#Option2   

library(tm)

#build a corpus, and specify the source to be character vectors

myCorpus <- Corpus(VectorSource(Trump$text))

#convert to lower case

myCorpus <- tm_map(myCorpus, content_transformer(tolower))

#remove URLs

removeURL <- function(x) gsub("http[^[:space:]]*", "", x)

myCorpus <- tm_map(myCorpus, content_transformer(removeURL))

#remove anything other than English letters or space

removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)

myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))

#remove stopwords

myStopwords <- c(setdiff(stopwords('english'), c("r", "big")),
                 "use", "see", "used", "via", "amp")
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)

#remove extra whitespace

myCorpus <- tm_map(myCorpus, stripWhitespace)

# keep a copy for stem completion later

myCorpusCopy <- myCorpus

myCorpus <- tm_map(myCorpus, stemDocument) # stem words


![image](https://user-images.githubusercontent.com/68969621/98045873-116c5b00-1e21-11eb-8bbc-f41c050c75b7.png)


![image](https://user-images.githubusercontent.com/68969621/98045909-2648ee80-1e21-11eb-8f05-341e17be927a.png)


# Findings 

The actual dataset has cleaned in two methods, there are slightly differences on each output, here we will look into the tweet number #8534, both methods are used function (writeLines) to review the tweet content.

# Method 1- gusb 

#with Corpus
writeLines(strwrap(myCorpus[[8534]]$content, 60))


#without 2nd way of working 

writeLines(strwrap(Trump$text[8534], 60))

![image](https://user-images.githubusercontent.com/68969621/98046021-56908d00-1e21-11eb-9e8d-a7526d62eba9.png)


# Method 2- Corpus

writeLines(strwrap(Trump$text[8534], 60))

![image](https://user-images.githubusercontent.com/68969621/98046058-65773f80-1e21-11eb-90e3-0a9cef10c0f6.png)

# Comment
Both results have removed the punctuation, empty spaces and special characters. However, in method 1 we can see all the letters are in lower case but the characters are automatically corrected and completed by the software. 
In method 2, all the characters are in lower case, but we can see the word ‘family’ is not corrected.


# Remove retweets
Trump_C <- Trump[Trump$is_retweet==FALSE, ] 
# Remove replies
Trump_C<- subset(Trump_C, is.na(Trump_C$reply_to_status_id)) 

Trump_C <- Trump_C %>% arrange(-favorite_count)
Trump_C[1,5]
Trump_C <- Trump_C %>% arrange(-retweet_count)
Trump_C[1,5]  

# Keeping only the retweets

Trump_retweets <- Trump[Trump$is_retweet==TRUE,]

# Keeping only the replies

Trump_replies <- subset(Trump, !is.na(Trump$reply_to_status_id))


![image](https://user-images.githubusercontent.com/68969621/98044693-2e079380-1e1f-11eb-9b43-a7f20d60dc72.png)


# Creating a data frame

data <- data.frame(
  category=c( "Retweets", "Replies"),
  count=c( 192, 120)
)


![image](https://user-images.githubusercontent.com/68969621/98044730-38c22880-1e1f-11eb-95df-6190c1500c12.png)



#-------------------------------------------------------------------------


# How many unique locations?

unique(Trump$quoted_location)

![image](https://user-images.githubusercontent.com/68969621/98046820-c3f0ed80-1e22-11eb-9738-d22d50b13d20.png)


406 unique share locations

length(unique(Trump$quoted_location))


## Graph has added the function to show n number of locations, in this case 20
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
       

![image](https://user-images.githubusercontent.com/68969621/98046971-1205f100-1e23-11eb-99db-150ee0d875ee.png)


       
# Location word cloud to identify the different

Trump$quoted_location <- as.character(Trump$quoted_location)

set.seed(1234)
wordcloud(Trump$quoted_location, min.freq=4, scale=c(5, .5), random.order=FALSE, rot.per=0.3, 
          colors=brewer.pal(8, "Dark2"))

![image](https://user-images.githubusercontent.com/68969621/98047031-2813b180-1e23-11eb-9e8d-8ce717f8428d.png)

          
#==========================================================

# Different devices (source) 

unique(Trump$source)

![image](https://user-images.githubusercontent.com/68969621/98047074-3792fa80-1e23-11eb-9d16-19a1c4996178.png)

#130 unique device types 
length(unique(Trump$source))

# Top 20 source of devices 

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

![image](https://user-images.githubusercontent.com/68969621/98049037-d53bf900-1e26-11eb-910e-12bec03e9897.png)


# Word cloud for most frequently found device types 

Trump$source <- as.character(Trump$source)

set.seed(1234)
wordcloud(Trump$source, min.freq=4, scale=c(5, .5), random.order=FALSE, rot.per=0.3, 
          colors=brewer.pal(8, "Dark2"))

![image](https://user-images.githubusercontent.com/68969621/98047584-141c7f80-1e24-11eb-8f48-6f8ad27f245d.png)


# Words that frequent used within tweets 

#Remove any stop words 

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

# Bar chart of the most frequent words found in the tweets
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
       
![image](https://user-images.githubusercontent.com/68969621/98048584-e33d4a00-1e25-11eb-84ef-ac6859dd6960.png)
       
# Word cloud for most appeared words 

tweets <- as.character(tweets)
tweets <- gsub("c\\(", " ", tweets)
set.seed(1234)
wordcloud(tweets, min.freq=4, scale=c(5, .1), random.order=FALSE, rot.per=.1, 
          colors=brewer.pal(9, "Set2"))      

![image](https://user-images.githubusercontent.com/68969621/98048650-0e279e00-1e26-11eb-9d34-815409ff67d6.png)


# Sentiment Analysis with Trump 

#library(tidytext)
#library(stringr)
#library(dplyr)
#library(janeaustenr)

#nrc emotion lexicon
get_sentiments("nrc")
#nrc emotion #negative or positive 
get_sentiments("bing")
#with score 
get_sentiments("afinn")

library(ggplot2)
ggplot(data=sentimentscores,aes(x=sentiment,y=Score))+
  geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("Scores")+
  ggtitle("Total sentiment based on scores")+
  theme_minimal()

![image](https://user-images.githubusercontent.com/68969621/98048744-44651d80-1e26-11eb-92be-ba7877dd49e2.png)

# Output 

Generally falling onto the negative side as words of negative, fear, anger, and sadness are presented and there are less than 200 scores which is joyful.  Based on the statistic, majority of these outputs are coming from US citizen and there are clear disappointed voices are lying under these numbers.
However, the indictors in blue (positive) and pink (trust) are in locating on the opposite location, where seemed to be Donald Trump’s supporters.


