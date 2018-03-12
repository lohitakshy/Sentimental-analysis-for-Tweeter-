
#Provides functions for general HTTP requests, GET and POST forms
install.packages('RCurl')

#To create an interface with the Twitter web API
install.packages('twitteR')

#Needed for the summarise function. Here we summarise the tweets and number them
install.packages('dplyr')

#Used to split, add functionality and combine data. Here we use to clean our data
install.packages('plyr')

#Provides creative data visualisation
install.packages('ggplot2')

#To manipulate strings
install.packages('stringr')

#For 3D graph visualisation
install.packages('plotrix')


#making the packagaes for the particular session as they are not included by default
require(twitteR)
require(RCurl)
require(dplyr)
require(ggplot2)
require(plyr)
require(stringr)
require(plotrix)

library(twitteR)
library(RCurl)
library(dplyr)
library(ggplot2)
library(plyr)
library(stringr)
library(plotrix)

#The details of the twitter application created at dev.twitter.com via a twitter account
consumer_key <- 'HqB7aGKH1vLuPQ7gX0lWcQuc2'
consumer_secret <- 'DZtrxs7eLq8DNCASXsCu8jwGwUtwUsP4kLbnHAXuUn3Nz0JU2z'
access_token <- '849363124780138496-Kcb11TXTFK3aLCRaqYXevZOKu0KVaBV'
access_secret <- 'bkZXT8GvNQcdwQJBlZh14OLMTegvR3TsVLIl2Uo51GG5J'


#Accessing the twitter API using the above details
#Select option 2, that is NO, when prompted after executing this command.
#We are NOT using a local file for authorisation of credentials and hence No.
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)


#Obtaining n tweets using the searchTwitter function for a particular time period
ManchesterCityTweets <- searchTwitter("#ManchesterCity", n = 2000, since = '2007-10-30', resultType = 'recent',
                            lang = "en")
ManchesterCityTweets


#Converting the list of tweets to a dataframe
mcdf <- twListToDF(ManchesterCityTweets)


#Cleaning the data and by removing blank spaces, punctuation marks, symbols and so on.
#We do this by replacing the unwanted characters by a "".
#gsub function is used for replacement.
ManchesterCityTweets$text = gsub("[:blank:]", "", ManchesterCityTweets$text)
ManchesterCityTweets$text = gsub("[[:punct:]]", "", ManchesterCityTweets$text)
ManchesterCityTweets$text = gsub("[:cntrl:]", "", ManchesterCityTweets$text)
ManchesterCityTweets$text = gsub("[[:digit:]]", "", ManchesterCityTweets$text)
ManchesterCityTweets$text = gsub("[:blank:]", "", ManchesterCityTweets$text)
ManchesterCityTweets$text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ",  ManchesterCityTweets$text)
ManchesterCityTweets$text = gsub("@\\w+", "", ManchesterCityTweets$text)
ManchesterCityTweets$text = gsub("http\\w+", "", ManchesterCityTweets$text)


#When the tweets are obtained, all the details related to the tweet like user, number of retweets,
#and so on are also obtained.
#Here we create a subset that contains only the text content of the tweets.
mcdf_subset = subset(mcdf, select = c(text))
mcdf_subset


#The column names are arranged in alphabetical order.
#order () is used for the ordered arrangemnt.
#names () contains the column names
# , indicates all columns.
names(mcdf)
mcdf <- mcdf[, order(names(mcdf))]


#The date the dataframe is created in converted into YYYY-MM-DD format using the strftime function.
mcdf$created <- strftime(mcdf$created, '%Y-%m-%d')


#If file doesn't exist, then create a file with a particular name.
#paste() combines two strings
#write.csv function converts the dataframe into a csv file.
if (file.exists(paste('#ManchesterCity', 'Proj.csv'))==FALSE) 
  write.csv(df, file=paste('#ManchesterCity', 'Proj.csv'), row.names=F)


#Read the previously created CSV file and remove duplicate text.
#read.csv() is used to read the CSV file.
#rbind () is used to combine the dataframe and CSV file.
ReadTweetFile <- read.csv(file=paste('#ManchesterCity', 'Proj.csv'))
ReadTweetFile <- rbind(ReadTweetFile, mcdf)
ReadTweetFile <- subset(ReadTweetFile, !duplicated(ReadTweetFile$text))
write.csv(ReadTweetFile, file=paste('#ManchesterCity', 'Proj.csv'), row.names=F)


#function to calculate sentiment score
#lapply() is used to apply a function to a dataframe
#str_split() is used to split the individual words in the sentence  
#unlist() creates a vector using the individual components of str_split
#This vector is compared with our stock of positive and negative words and a score is calucalated
#based on number of positive words - number of negative words
SentimentScoreFunction <- function(OurSentence, PositiveWordList, NegativeWordList, .progress='none')
{
  require(plyr)
  require(stringr)
  TweetScore <- laply(OurSentence, function(sentence, PositiveWordList, NegativeWordList){
  sentence <- gsub('[[:punct:]]', "", sentence)
  sentence <- gsub('[[:cntrl:]]', "", sentence)
  sentence <- gsub('\\d+', "", sentence)
  sentence <- tolower(sentence)
  OurWordList <- str_split(sentence, '\\s+')
  Words <- unlist(OurWordList)
  PositiveWordMatches <- match(Words, PositiveWordList)
  NegativeWordMatches <- match(Words, NegativeWordList)
  PositiveWordMatches <- !is.na(PositiveWordMatches)
  NegativeWordMatches <- !is.na(NegativeWordMatches)
  SentimentScore <- sum(PositiveWordMatches) - sum(NegativeWordMatches)
  return(SentimentScore)
}, PositiveWordList, NegativeWordList, .progress=.progress)
TweetScoredf <- data.frame(score=TweetScore, text=OurSentence)
return(TweetScoredf)
}


#Loading the positive words wordlist using the scan() function 
pos <- scan('E:/UNCC/Survey of Programming Languages/Project/positive-words.txt',
           what='character', comment.char=';')


#Loading the negative words wordlist using the scan() function
neg <- scan('E:/UNCC/Survey of Programming Languages/Project/negative-words.txt',
            what='character', comment.char=';')


#as.factor convert the vector into factor
#the text along with the positive and negative word list is sent to score.sentiment() function
#as arguments
ReadFile <- read.csv("#ManchesterCity Proj.csv")
ReadFile$text <- as.factor(ReadFile$text)
TweetScore <- SentimentScoreFunction(ReadFile$text, pos, neg, .progress='text')
write.csv(TweetScore, file=
            paste("#ManchesterCity", 'scores.csv'), 
          row.names=TRUE)


#as.date() converts the date of creation into the date format.
#mutate() adds new variables while preserving the old variables.
#In our case, the tweet score variable is added preserving the existing creation date variable.
#tweet score -> greater than 1 indicates a positive tweet, less than 1 indicates a negative tweet,
#and 0 indicates a neutral tweet. This is implemented using the if-else function.
Statistic <- TweetScore
Statistic$created <- ReadTweetFile$created
Statistic$created <- as.Date(Statistic$created)
Statistic <- mutate(Statistic, tweet=ifelse(Statistic$score > 0, 'positive',
                                  ifelse(Statistic$score < 0, 'negative', 'neutral')))


#group_by() function combines the arguments into a dataframe.
#dplyr::summarise collapses the dataframe created into a single row.
#This information is exported to a CSV file using the write.csv function.
Tweet <- group_by(Statistic, tweet, created)
Tweet <- dplyr::summarise(Tweet, number=n())
write.csv(Tweet, file=paste("#ManchesterCity", 'opin.csv'), row.names=TRUE)


#A plot graph based on the tweet sentiment score
ggplot(Tweet, aes(created, number)) + geom_line(aes(group=tweet, color=tweet), size=2) +
  geom_point(aes(group=tweet, color=tweet), size=4) +
  theme(text = element_text(size=18), axis.text.x = element_text(angle=90, vjust=1)) +
  ggtitle("#ManchesterCity")


#A qplot graph based on the tweet sentiment score
qplot(TweetScore$score, xlab = "Score")


#A histogram based on the tweet sentiment score
hist(TweetScore$score, main = 'Histogram of Sentiment Score')


#Calculating the number of negative, positive and neutral tweets
opin <- read.csv(file = "#ManchesterCity opin.csv")

i <- 0

total_negative <- 0
total_positive <- 0
total_neutral <- 0

for(i in 1:length(opin$tweet))
{
  if(opin$tweet[i] == "negative")
  {
    total_negative <- total_negative + opin$number[i]
  }
  else if(opin$tweet[i] == "positive")
  {
    total_positive <- total_positive + opin$number[i]
  }
  else
  {
    total_neutral <- total_neutral + opin$number[i]
  }
  
}


total_negative
total_positive
total_neutral


#A 3D piechart based on the tweet sentiment score
library(plotrix)
slices <- c(total_negative, total_neutral, total_positive)
lbls <- c("Negative", "Neutral", "Positive")
pie3D(slices, labels = lbls,  explode=0.1,
      main="Pie Chart of Tweet Sentiment 
      Analysis")