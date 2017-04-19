library(twitteR)    ##Connecting R with twitter
library(httr)
library(tm)         ## for text mining
library(SnowballC)  ## provides wordstem() function
library(wordcloud)  ## for visualizing text data in the form of cloud
library(e1071)      ## for naive bayes implementation
library(gmodels)

> setup_twitter_oauth(consumer_key='XXXXXX', consumer_secret='XXXXXX', access_token= 'XXXXXX', access_secret='XXXXXX')     ##setting up a connection with twitter
> starbucks_tweets <-searchTwitter("starbucks",n=10000,lang="en")                                                          ##Fetching 10000 tweets containing the                                                                                                                                  ##keyword starbucks.                                                                                                                              

## Next step is to store the tweets in either database or csv file and categorize them as positive or negative. These tweets will be used to create data dictionary.Lets assume that tweets are stored in a CSV file(tweets_list.csv) with 2 columns text and type


> tweets_raw <- read.csv("tweets_list.csv", header = T, stringsAsFactors = F)                                             ##CSV file is imported into R dataframe :                                                                                                                               ##tweets_raw with 2 features:text and type                                                     

## Next step is data cleaning and data preparation. this can be done using tm (text mining package)

## VCorpus is the function that stores all the text data

> tweet_corpus <- VCorpus(VectorSource(tweets_raw$text))

## for unhappy, UNHAPPY, unHaPPy words (just an example) to be counted as one word - we need to do Data cleansing. It will take care all other words with similar occurances
## for this we use tm_map() function.It will convert all the words to lowercase

> tweet_corpus_clean <- tm_map(sms_corpus, content_transformer(tolower))

## lets remove numbers from tweets as most of them will not provide any good insights

> tweet_corpus_clean <- tm_map(tweet_corpus_clean, removeNumbers)

## now remove filler words such as to, and , but. they appear frequently and do not provide any useful insights
## for this we will use stopwords() function provided by tm package

> tweet_corpus_clean <- tm_map(tweet_corpus_clean, removeWords, stopwords())

## now remove punctuation

> tweet_corpus_clean <- tm_map(tweet_corpus_clean, removePunctuation)

## to remove suffix as in : experiencing, experiences, experienced and convert them to word: experience use stemDocument()

> tweet_corpus_clean <- tm_map(tweet_corpus_clean, stemDocument)

## now remove white spaces if any 

>tweet_corpus_clean <- tm_map(tweet_corpus_clean, stripWhitespace)

## Data preparation --- splitting tweets into words

>tweet_dtm <- DocumentTermMatrix(tweet_corpus_clean)

## Data preparation --- creating training and test datasets

>tweet_dtm_train <- tweet_dtm[1:8000, ]
>tweet_dtm_test <- tweet_dtm[8001:10000, ]


>tweet_train_labels <- tweet_dtm[1:8000, ]$type
>tweet_test_labels <- tweet_dtm[8001:10000, ]$type


## Visualizing text data --- word clouds -- This will create word clouds for positive and negative words

>positive <- subset(tweet_raw, type == "positive")
>negative <- subset(tweet_raw, type == "negative")

>wordcloud(positive$text, max.words = 40, scale = c(3, 0.5))
>wordcloud(negative$text, max.words = 40, scale = c(3, 0.5))


## creating indicator features for frequent words

>tweet_freq_words <- findFreqTerms(tweet_dtm_train, 5)
>str(tweet_freq_words)

>tweet_dtm_freq_train<- tweet_dtm_train[ , tweet_freq_words]
>tweet_dtm_freq_test <- tweet_dtm_test[ , tweet_freq_words]

>convert_counts <- function(x) {
>  x <- ifelse(x > 0, "Yes", "No")
>}

>tweet_train <- apply(tweet_dtm_freq_train, MARGIN = 2,
                   convert_counts)
>tweet_test <- apply(tweet_dtm_freq_test, MARGIN = 2,
                    convert_counts)


## training the model

>tweet_classifier <- naiveBayes(tweet_train, tweet_train_labels)

## Based on the training, model will predict whether a tweet is positive or negative

>tweet_pred <- predict(tweet_classifier, tweet_test)


##Comparing the predicted result with the actual values

>CrossTable(tweet_test_pred, tweet_test_labels,
             prop.chisq = FALSE, prop.t = FALSE,
             dnn = c('predicted', 'actual'))


##improving model performance

tweet_classifier2 <- naiveBayes(tweet_train, tweet_train_labels,
                              laplace = 1)

tweet_test_pred2 <- predict(tweet_classifier2, tweet_test)

##Finally, we'll compare the predicted classes to the actual classifications using cross tabulation

CrossTable(tweet_test_pred2, tweet_test_labels,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
dnn = c('predicted', 'actual'))

##Once the model is evaluated and working as expected, it can be deployed to work in realtime