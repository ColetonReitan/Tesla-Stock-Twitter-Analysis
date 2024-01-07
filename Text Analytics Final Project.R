library(lubridate)
library(textcat)
library(tidyverse)
library(plyr)
library(stringr)
library(ggplot2)
library(tm)
library(readr)
library(dplyr)
library(SnowballC)
library(caret)
#Coleton, Sukh, Ryans

#Setting WD
setwd("C:/Users/colet/Downloads/Adelphi/DSC 785 Text Analytics")

#Import Data
Hashtag.Tesla.Tweets <- read.csv("Hashtag Tesla Tweets.csv", sep=",", header=TRUE, stringsAsFactors=FALSE) #Read and import tesla tweets dataframe
TSLA <- read.csv("TSLA.csv", sep=",", header=TRUE, stringsAsFactors=FALSE) #Read and import tesla stock dataframe

#Remove Unecessary columns from twitter data. 
tweets <- data.frame(Hashtag.Tesla.Tweets$Date...Time, Hashtag.Tesla.Tweets$Twitter.ID, Hashtag.Tesla.Tweets$Tweet.Text) #Remove unnecessary columns from the original tweets dataframe by creating a new shortened version
tweets <- subset(tweets, nchar(as.character(Hashtag.Tesla.Tweets.Tweet.Text)) > 1) #Remove the blanks (preliminary)
rownames(tweets) <- 1:nrow(tweets) #Fix the indexes after rows were removed

#Rename the column names in the new tweets dataframe to make it look nicer
colnames(tweets)[1] <- 'Date'
colnames(tweets)[2] <- 'Twitter.ID'
colnames(tweets)[3] <- 'Tweet.Text'

#Creating Datetime data and using data before july 1
TSLA$Date <- mdy(TSLA$Date) #Turn our dates into proper datetime format
TSLA_short <- TSLA[TSLA[["Date"]] < "2022-07-01", ] #Remove any stock data after June 30th, 2022 and declare another (cleaner) stock dataframe

#Cleaning twitter data and adjusting timeframe
tweets$Date <- gsub(" at.*","",tweets$Date) #Remove anything after the " at" in the Date column of the tweets dataset
tweets$Date <- mdy(tweets$Date) #Turn our dates into proper datetime format
TWEETS_short <- tweets[tweets[["Date"]] < "2022-07-01", ] #Remove any twitter data after June 30th, 2022 and declare another (cleaner) twitter dataframe
TWEETS_short <- na.omit(TWEETS_short) #Omit NAs (last column)

##################################################################
#Run line below for computer to identify languages or skip this line to line45 and import the preread language file. 
TWEETS_short$Language <- textcat(TWEETS_short$Tweet.Text) #Use textcat to identify tweet languages
#write.csv(TWEETS_short, file = "TWEETS_short.csv", row.names=FALSE) #Saved code and emailed it to group since texctcat takes 10 minutes to run
TWEETS_short<-read.csv("TWEETS_short w lang.csv")
head(TWEETS_short)
TWEETS_short$Date<-mdy(TWEETS_short$Date)
head(TWEETS_short)
##################################################################


#keeping only the english tweets
TWEETS_short <- subset(TWEETS_short, TWEETS_short$Language == "english") #Remove any foreign language
rownames(TWEETS_short) <- 1:nrow(TWEETS_short) #Fix the indexes again after removal
head(TWEETS_short)


#Getting volume of tweets per day and adding to tweets short
TWEETS_short <- ddply(TWEETS_short, "Date", mutate, tweet_volume = length(Date))
head(TWEETS_short)

#Grouping tweets together by date. Creating one big corpus of all tweets from each date. 
df<- TWEETS_short %>% #Create new dataframe that groups all tweets of a given date into one corpus
  group_by(Date, tweet_volume) %>% #Use the group_by function to group by date
  summarise(Corpus = paste(Tweet.Text, collapse = ","))
head(df)

#Loading in positive and negative words for the sentiment analysis
posText <- read.delim("positive-words.txt", header=FALSE, stringsAsFactors=FALSE) #Read the positive library document
pos.words = c(posText$V1) #create a list of the positive words
negText <- read.delim("negative-words.txt", header=FALSE, stringsAsFactors=FALSE) #Read the negative library document
neg.words = c(negText$V1) #create a list of the negative words

#create the sentiment function
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  results = laply(sentences,
                  function(sentence, pos.words, neg.words)
                  {
                    originalsentence=sentence
                    #Remove punctuation
                    sentence = gsub("[[:punct:]]", "", sentence)
                    #Remove control characters
                    sentence = gsub("[[:cntrl:]]", "", sentence)
                    #Remove digits
                    sentence = gsub('\\d+', '', sentence)
                    #Change to lower case
                    sentence = tolower(sentence)
                    #Split sentence into words with str_split (stringr package)
                    word.list = str_split(sentence, "\\s+")
                    words = unlist(word.list)
                    #Remove stop words
                    words<-words[!words %in% stopwords("english")]
                    #Remove specific words if needed
                    mywordlist<-c('Tesla','TSLA')
                    words<-words[!words %in% mywordlist]
                    cleansentence<-paste( unlist(words), collapse=' ')
                    #Compare words to the dictionaries of positive & negative terms
                    pos.matches = match(words, pos.words) #This returns the position of the word in the pos.word
                    neg.matches = match(words, neg.words)
                    #User TRUE/FALSE to identify if the word exists in the sentiment library
                    pos.count = !is.na(pos.matches) #if there the word exists in the sentiment library (there is a position number), the output will be TRUE
                    neg.count = !is.na(neg.matches)
                    #Final score, sum the number of TRUE to calculate the pos and neg score
                    score = sum(pos.count) - sum(neg.count)
                    result<-list("sentence"=as.character(originalsentence),"cleansentence"=cleansentence,"score"=score)
                    return(result)
                  }, pos.words, neg.words, .progress=.progress )
  #Create a data frame with scores for each sentence
  scores.df = data.frame(results)
  return(scores.df)
}

#Apply the function to our data
scores = score.sentiment(df$Corpus, pos.words, neg.words, .progress='text')
#Add sentiment score to df dataset
df$sentimentscore<-unlist(scores$score)
#add sentiment polarity to df dataset
df$sentimentpolarity <- ifelse(df$sentimentscore >0,"positive",ifelse(df$sentimentscore < 0,"negative","neutral"))
head(df)

#summarize the scores
pos_count=sum(df$sentimentpolarity =="positive")
neg_count=sum(df$sentimentpolarity == "negative")
neu_count=sum(df$sentimentpolarity == "neutral")
total=pos_count+neg_count+neu_count
#create a list
sent_count<-c(pos_count,neg_count,neu_count)
#calculate percentage
sent_percent = round( 100 * sent_count / total )
#create label
category<-c("pos","neg","neu")
comparison <- paste(sent_percent,"%",sep="")
comparison <- paste(category,comparison)
#create pie chart
pie(sent_percent, labels = comparison, col = rainbow(length(comparison)), main = "Tesla Twitter Sentiment Analysis")


#Merging the dataframes then dropping NA's
combined_df<- df %>%
  full_join(TSLA_short, by = "Date") %>%
  arrange(Date)
combined_df <- na.omit(combined_df) #Drop NAs
head(combined_df)

#Dropping uneeded columns from combined df
combined_df <- subset(combined_df, select = -c(6,7,8,9)) #open, close, high, low dropped
head(combined_df)



#Running regression model on key words in corpus to predict sentiment for data
#establish the corpus
corpus <- VCorpus(VectorSource(combined_df$Corpus))

#create the term by document matrix
corpus <- tm_map(corpus, removeNumbers) #remove numbers
corpus <- tm_map(corpus, removePunctuation) #remove punctuation
corpus <- tm_map(corpus, removeWords, stopwords("english")) #remove stop words
corpus <- tm_map(corpus, content_transformer(tolower)) #change to lower case
corpus <- tm_map(corpus, stemDocument) #stemming

#create the transposed term by document matrix
dtm <- as.matrix(DocumentTermMatrix(corpus))#prepare Term document matrix
dim(dtm)

#identify the key words for classification
f <- sort(colSums(dtm),decreasing=TRUE) #sum word frequency and sort matrix by frequency
wordfrequency <- data.frame(word = names(f),freq=f) #turn f to a table
head(wordfrequency)
#an example of just using the high frequency words
keywords <- subset(wordfrequency, freq > 1000)
keydtm <- subset(dtm, select = keywords$word)
tail(combined_df)

#extract pattern (predict sentiment and compare)
combined_df$t_sent <- ifelse(combined_df$sentimentpolarity == 'Positive', 1, 0) #turn sentiment to numerical variable
cleandata <- data.frame(cbind(combined_df$t_sent,keydtm)) #combine the sentiment with all predicting factors
colnames(cleandata)[1] <- "t_sent" #add column name

#create trainning and testing dataset
train <- cleandata[1:35,]
test <- cleandata[36:54,]

#create a simple model
model <- glm(t_sent ~ ., family = 'binomial', data = train) #create a simple logistic regression model
summary(model)

predict.probability <- predict(model, test, type = "response") #predict the probability
test$predict.sentiment <- ifelse(predict.probability > 0.5, "Positive" , "Negative") #identify the sentiment

#compare the predicted result with the pre-classified
results <- data.frame(cbind(test$predict.sentiment,combined_df[36:54,5]))
colnames(results)<- c("predicted","predetermined")
comparison<-table(results)

#Running regression to predict stock price using;
#Variables: Tweet volume, Sentiment Score, 

#Making our dtm into a dataframe
key_words<-as.data.frame(keydtm)
head(key_words)

#Creating a separate dates dataframe to apply to our keywords dataframe
dates$Date <-seq(as.Date("2022-04-11"),as.Date("2022-06-30"),by = 1)
dates<-data.frame(dates)
head(dates)

#Dropping dates that are irrelevant to our data
dates <- dates %>%  filter(!row_number() %in% c(5,6,7,13,14,17,18,20,21,27,28,34,35,41,42,48,49,50,55,56,62,63,69,70,71,76,77))
updated <- cbind(key_words, dates)
head(updated)


#Merging the keywords dataframe and our combined_df dataframe then dropping NA's
okay <- combined_df %>%
  full_join(updated, by = "Date") %>%
  arrange(combined_df)

#Dropping words we thought were not applicable, as well as other columns from the combined_df dataframe 
finally_df <- subset(okay, select = -c(8,9,11,12,16,17,18,20,23,24,25,26,27,28,29,31,35)) #open, close, high, low dropped
colnames(finally_df)

# Fit the multiple linear regression model
reg_model <- lm(formula = Adj.Close ~ tweet_volume + sentimentscore + elonmusk + car + compani + model +spacex + amp + 
                  twitter + buy + love + like + want,data = finally_df)
summary(reg_model)

plot(reg_model)

