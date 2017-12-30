Twitter Sentiment Analysis
================
Sandeep Gunda
December 22, 2017

Due to the immense growth of the value of Bitcoin, I wanted to see what the people were talking about it, and what they felt about it. Twitter being the most popular medium where people openly expressed their opinions, I intended to perform a sentiment analysis to understand more about this. Due to restrictions, I performed the analysis on 6000 tweets between 1 December and 21 December 2017 posted in english.

### Lets import all the packages we'll need for this project

``` r
library(RColorBrewer)
library(wordcloud)
library(tm)
library(twitteR)
library(ROAuth)
library(plyr)
library(stringr)
library(base64enc)
library(tm)
library(ggplot2)
library(tidyverse)
```

Part 1 Data Extraction, Cleaning and Visualization
--------------------------------------------------

### Setup the variables for authentication with twitter

After creating a twitter app in the twitter developer page, we can get 4 things important for authentication with twitter, to extract the tweets. They are:

1.  Consumer Key
2.  Consumer Secret
3.  Access Token
4.  Access Token Secret

Copy them and store them as variables

Now we are ready to authenticate our requests to twitter. The setup\_twitter\_oath() method allows us the provide the neccessary crediantials to twitter and perform a OAuth. The access token is used to make API requests on your account's behalf.

``` r
setup_twitter_oauth(consumer_key,
                    consumer_secret,
                    access_token,
                    access_token_secret)
```

    ## [1] "Using direct authentication"

### Extract the tweets and set them up for pre-processing

The **searchTwitter()** method from the **twitteR** library allows us to search a string of keywords, separated by a '+' sign, we can also specify the number of tweets we want to retrieve, the language, and the time period ('since' and 'until'). In my case, I'm gonna look for the string "\#Bitcoin+\#bitcoincrash" and pull 6000 tweets from December 1, 2017 till December 21, 2017, in English.

``` r
extracted_tweets <- searchTwitter('#Bitcoin', n=6000, lang='en',since = '2017-12-01', until='2017-12-21')
extracted_tweets_text <- sapply(extracted_tweets, function(x) x$getText())
```

Replace all graphical characters from the tweets with spaces

``` r
extracted_tweets_text <- str_replace_all(extracted_tweets_text,"[^[:graph:]]", " ")
```

Create a corpus of documents from the tweets

``` r
extracted_tweets_text_corpus <- Corpus(VectorSource(extracted_tweets_text))
```

### Pre-process the text, for analysis

There are 5 important cleaning steps needed to perform, before any analysis can be done on the corpus of textual data. They are:

1.  Remove punctuations from the text. For all the words we have, their situational occurances must not matter for the word recognition. For example, the word 'fun' in the sentences "Are you having fun?", "I'm having fun!", "Define fun.", shown by "fun?", "fun!", and "fun." all should be recognized as just "fun".
2.  Convert all the documents to lower case, such that 'Cash' and 'cash' both are interpreted as the same word.
3.  Remove all the common stop words which often occur in the text, like 'a', 'an', 'the', etc.
4.  Remove any words which are obvious to appear at a higher rate for the specific situation.
5.  Remove urls from the text, they dont contribute to any sentiment.

``` r
clean_corpus <- function(corpus, custom_stop_words){
  # Remove the punctuations
  corpus <- tm_map(corpus, removePunctuation)
  # Convert all tweets to lower case
  corpus <- tm_map(corpus, content_transformer(tolower))
  # Remove all the common stop words
  corpus <- tm_map(corpus, function(x) removeWords(x, stopwords()))
  # Remove stop words specific to the situation
  corpus <- tm_map(corpus, removeWords, custom_stop_words)
  # Finally remove any urls from the tweets
  corpus <- tm_map(corpus, content_transformer(function(x) gsub("http[[:alnum:]]*", "", x)))
  
  return(corpus)
}

# Make a list of all the common recurring words that may occur in the use case
custom_stop_words <- c("rter","retweets","RT", "bitcoin", "cryptocurrency","btc", "bitcoins", "will", "crypto", "one", "now", "like", "1118")

# Call the function to clean the data
extracted_tweets_text_corpus <- clean_corpus(extracted_tweets_text_corpus, custom_stop_words = custom_stop_words)
```

### Build a Term Document Matrix

So now we have a list of documents(tweets) in a corpus. A term document matrix observes the frequency of occurance of a word in all the document. With that we can see which words apear most often, and which least, with the help of wordclouds and barplots.

``` r
# Build a Term Document Matrix to find out the Term Frequency (TF) then convert it into dataframe
term_doc_matrix <- TermDocumentMatrix(extracted_tweets_text_corpus) %>%
  as.matrix(.)
term_doc_df <- sort(rowSums(term_doc_matrix), decreasing = TRUE) %>%
  data.frame(word = names(.), freq = .)
```

### Visialize the word frequencies

``` r
term_doc_df$word <- factor(term_doc_df$word, levels = term_doc_df$word[order(term_doc_df$freq)])

ggplot(term_doc_df[1:10,], 
       aes(x=word, 
           y=freq,
           fill  = freq == max(freq),
           label = freq)) +
  geom_bar(stat='identity') + 
  scale_fill_manual(values = c('grey', 'red') ) +
  geom_label(aes(label = freq), 
             colour = "white", 
             fontface = "bold") +
  labs(title = "Most Popular words with their Frequencies\n", 
       x = "Words", 
       y = "Frequency", 
       fill = "Most Popular Word") +
  ggtitle(label = "Most Popular words with their Frequencies") + 
  coord_flip()
```

![](knit_test_files/figure-markdown_github/visualize-1.png)

Wordcloud

``` r
# Lets have a look at the wordcloud
set.seed(1234)

wordcloud(extracted_tweets_text_corpus, min.freq = 1, max.words = 100, scale = c(2.2, 1),
          colors = brewer.pal(8, "Dark2"), random.color = T, random.order = F)
```

![](knit_test_files/figure-markdown_github/wordcloud-1.png)

Some of the most common words are bitcoincash, blockchain, cnbcfastmoney. Most people are concerned about the bitcoin cash technology, which is a peer to per electronic cash, to be usable to make purchases, and how it'll affect the bitcoin prices. Depending on how consumers adopt Bitcoincash, the bitcoin prices may change drastically. Next, Blockchain technology is what Bitcoin is based on, so with Bitcoin gaining more and more popularity, people who dont know about blockchain yet, may stumble upon it on the internet, and want to know how changes in their tech can affect bitcoin prices. The Twitter handle of cnbcfastmoney looks to have gathered a lot of traction, as they have been covering the news articles, and predictions even on twitter as well, and looking at how cnbcfastmoney word is more popular, this means that they posted bitcoin related tweets more times than others, and people follow them more for investment related news.

Part 2 Sentiment Analysis through Lexical Analysis
--------------------------------------------------

There are multiple ways to perform sentiment analysis, but the basic one is lexical analysis. We will be doing lexical analysis through afinn method. FINN is a list of english words of about 4783 negative words and about 2006 positive words, including commonly misspeled words. Very Negative (rating -5 or -4) Negative (rating -3, -2, or -1) Positive (rating 1, 2, or 3) Very Positive (rating 4 or 5) between minus five (negative) and plus five (positive).

1.  Scan the positive and negative lists into R.

``` r
positive_words <- scan("pve.csv", what = 'character')
negative_words <- scan("nve.csv", what = 'character')
```

1.  Add custom words to the list

``` r
positive_words = c(positive_words, 'new','nice' ,'good', 'horizon')
negative_words = c(negative_words
                   
                   , 'wtf', 'behind','feels', 'ugly', 'back','worse' , 'shitty', 'bad', 'no','freaking','sucks','horrible')
```

1.  Make a sentiment generator function

``` r
sentiment_score_generator <- function(sentences, positive_words, negative_words)
{
  sentiment_scores <- laply(sentences, function(sentence, positive_words, negative_words) {
    
    # Remove punctuations, control characters and digits then transform all the text to lower case and split them into words
    words <- gsub('[[:punct:]]', '', sentence) %>%
      gsub('[[:cntrl:]]', '', .) %>%
      gsub('\\d+', '', .) %>%
      tolower() %>%
      str_split(., '\\s+') %>%
      unlist()
    
    # compare our words to the dictionaries of positive & negative terms
    positive_matches = match(words, positive_words)
    negative_matches = match(words, negative_words)
    
    # match() returns the position of the matched term or NA, 
    # we can ask for true or false, for not na and na respectively.
    positive_matches = !is.na(positive_matches)
    negative_matches = !is.na(negative_matches)
    
    # TRUE/FALSE will be treated as 1/0 by sum():
    sentiment_score = sum(positive_matches) - sum(negative_matches)
    
    
    return(sentiment_score)
  }, positive_words, negative_words)
  
  df = data.frame(sentiment_score=sentiment_scores, text=sentences)
  return(df)
}
```

1.  Put tweets to data frame and apply the above sentiment function

``` r
# put tweets into data frame
df_tweets <- ldply(extracted_tweets, function(tweets) tweets$toDataFrame())

# Apply sentiment function to tweets
tweet_sentiments <- sentiment_score_generator(df_tweets$text,positive_words,negative_words)
```

1.  Visualize the sentiments

Summary of scores

``` r
summary(tweet_sentiments$sentiment_score)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ## -5.0000  0.0000  0.0000  0.1223  1.0000  5.0000

Hisogram of scores

``` r
ggplot(tweet_sentiments, 
       aes(tweet_sentiments$sentiment_score))+
  geom_histogram(bins = 10, 
                 col="white", 
                 aes(fill=..count..)) +
  stat_bin(binwidth=1, 
           geom="text", 
           colour="black", 
           size=3.5,
           aes(label=..count.., 
               y=(..count..))) +
  labs(title = "Distribution of Tweets According to Sentiment Scores\n", 
       x = "Score", 
       y = "Count")
```

![](knit_test_files/figure-markdown_github/hist-1.png)

Count of tweets per score

``` r
tweet_sentiments %>% count(.$sentiment_score)
```

    ## # A tibble: 11 x 2
    ##    `.$sentiment_score`     n
    ##                  <int> <int>
    ##  1                  -5     2
    ##  2                  -4     2
    ##  3                  -3    34
    ##  4                  -2   181
    ##  5                  -1   883
    ##  6                   0  3247
    ##  7                   1  1323
    ##  8                   2   228
    ##  9                   3    83
    ## 10                   4    14
    ## 11                   5     3

Conclusion
----------

So in the end, we have 1100 tweets (18.3%) with negative sentiments, 1652 tweets (27.5%) with positive sentiment, and an overwhelming 3248 tweets (54.1%) with neither positive nor negative sentiment. This means that most people are neutral about the situation, whereas, more people think positively about bitcoin than not, due to investor optimism.

Due to rate limits by twitter, only a limited amounts of tweets could be extracted. For a limited sample set of 6000 tweets, the analysis is approximately close to the real situation, if not accurate.
