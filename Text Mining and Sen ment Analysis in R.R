

# code to know our current working directory
getwd()

# 2 code to create a single data frame that consist of all the tweets data in the dataset provided.
Tweets <- read.csv("cs_data2")

tweets_df <- Tweets
# Step 3

install.packages("dplyr")
install.packages("ggplot2")
install.packages("lubridate")
install.packages("stringr")
install.packages("tidytext")
install.packages("textdata")
install.packages("sentimentr")
install.packages("lexicon")
install.packages("tm")
install.packages("tibble")
install.packages("afinn")


library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(tidytext)
library(textdata)
library(sentimentr)
library(lexicon)
library(tm)
library(tibble)
library(afinn)

         
         
# creating a new data frame 
clean_tweets_df <- tweets_df 



# Removing URLs
clean_tweets_df$text <- str_remove_all(clean_tweets_df$text, "\\bhttps?://[[:alnum:]]+\\.[[:alnum:]]+(/[[:alnum:].,_/-]*)?")


# Removing emojis 
clean_tweets_df$text <- str_remove_all(clean_tweets_df$text, "\\p{Emoji}")


# removing Profanity
#Profanity list using 3 in the provided list.

profanity_list <- c(lexicon::profanity_alvarez, lexicon::profanity_banned, lexicon::profanity_arr_bad)

words_in_tweets_df <- clean_tweets_df %>% 
  dplyr::select(text) %>% 
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  #anti_join(data.frame(profanity_list)) %>% 
  filter(!word %in% c("rt", "t.co", profanity_list))


# reformatting the created_at field of new data frame using POSIXct
clean_tweets_df$created_at <- as.POSIXct(clean_tweets_df$created_at, format = "%a %b %d %H:%M:%S %z %Y")




# step 4
# creating a new data frame with additional 6 columns from crated_at

clean_tweets_addition <- clean_tweets_df %>%
  mutate(
    year = year(created_at),
    month = month(created_at),
    day = day(created_at),
    hour = hour(created_at),
    date = as.Date(created_at),
    weekday = weekdays(as.Date(created_at))
  )



# step5 
# creating a new data frame containing all of the non- stop words from all of the tweets
# Function to remove stop words




# Get a list of English stop words
stop_words <- tm::stopwords("en")

# Function to remove stop words from a text
remove_stopwords <- function(text) {
  words <- unlist(strsplit(as.character(text), " "))
  meaningful_words <- words[!(words %in% stop_words)]
  return(meaningful_words)
}

# Apply the function to create a new data frame with non-stop words
tweets_non_stopwords <- clean_tweets_df %>%
  mutate(cleaned_text = sapply(text, remove_stopwords))




# Create a new data frame containing all non-stop words
non_stop_words <- clean_tweets_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)




# step6
# 6.a

# Find the top 20 unique words
top_words <- words_in_tweets_df %>%
  unnest_tokens(word, word) %>%
  count(word, sort = TRUE) %>%
  top_n(20)

# Visualizing the top 20 unique words in a plot
ggplot(top_words, aes(x = reorder(word, n), y = n, fill = word)) +
  geom_col() +
  xlab("Word") +
  ylab("Frequency") +
  ggtitle("Top 20 Unique Words in Tweets") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill=FALSE)



# 6.b


# Filtering out all non-ASCII rows from the data frame
non_ASCII_df <- words_in_tweets_df %>%
  filter(!Encoding(word) == "known")




# 6.c


top_words2 <- non_ASCII_df %>%
  unnest_tokens(word, word) %>%
  count(word, sort = TRUE) %>%
  top_n(20)


ggplot(top_words2, aes(x = reorder(word, n), y = n, fill = word)) +
  geom_col() +
  xlab("Word") +
  ylab("Frequency") +
  ggtitle("Top 20 Words after filtering out all non-ASCII in Tweets") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill=FALSE)



# step7
# 7.a

# Filter the inbound tweets
inbound_tweets <- clean_tweets_df %>%
  filter(inbound == "True")



# Create a Corpus
tweet_corpus <- Corpus(VectorSource(inbound_tweets$text))

# Preprocessing: Convert to lower case, remove punctuation, numbers, and stop words
tweet_corpus <- tm_map(tweet_corpus, content_transformer(tolower))
tweet_corpus <- tm_map(tweet_corpus, removePunctuation)
tweet_corpus <- tm_map(tweet_corpus, removeNumbers)
tweet_corpus <- tm_map(tweet_corpus, removeWords, stopwords("english"))

# Convert the Corpus back to a character vector
cleaned_tweets <- sapply(tweet_corpus, as.character)

# Replace the tweets in the dataset with the cleaned tweets
inbound_tweets$text <- cleaned_tweets


# Find the top 20 unique words for inbound tweets
top_inbound_words <- inbound_tweets %>%
  unnest_tokens(word, text) %>%
  count(word, sort = TRUE) %>%
  top_n(20)

# Visualize the top 20 unique words for inbound tweets in a plot
ggplot(top_inbound_words, aes(x = reorder(word, n), y = n, fill = word)) +
  geom_col() +
  xlab("Word") +
  ylab("Frequency") +
  ggtitle("Top 20 Unique Words in Inbound Tweets") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill=FALSE)




# 7.b


# Filter the outbound tweets
outbound_tweets <- clean_tweets_df %>%
  filter(inbound == "False")

tweet_corpus <- Corpus(VectorSource(outbound_tweets$text))

# Preprocessing: Convert to lower case, remove punctuation, numbers, and stop words
tweet_corpus <- tm_map(tweet_corpus, content_transformer(tolower))
tweet_corpus <- tm_map(tweet_corpus, removePunctuation)
tweet_corpus <- tm_map(tweet_corpus, removeNumbers)
tweet_corpus <- tm_map(tweet_corpus, removeWords, stopwords("english"))

# Convert the Corpus back to a character vector
cleaned_tweets <- sapply(tweet_corpus, as.character)

# Replace the tweets in the dataset with the cleaned tweets
outbound_tweets$text <- cleaned_tweets

# Find the top 20 unique words for outbound tweets
top_outbound_words <- outbound_tweets %>%
  unnest_tokens(word, text) %>%
  count(word, sort = TRUE) %>%
  top_n(20)


# Visualize the top 20 unique words for outbound tweets in a plot
ggplot(top_outbound_words, aes(x = reorder(word, n), y = n, fill = word)) +
  geom_col() +
  xlab("Word") +
  ylab("Frequency") +
  ggtitle("Top 20 Unique Words in Outbound Tweets") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill = FALSE)



# step8
# 8.a


get_sentiments("afinn")

afinn_word_counts <- words_in_tweets_df %>% 
  inner_join(get_sentiments("afinn")) %>%
  mutate(sentiment = if_else(value>0, 'positive', 'negative')) %>% 
  count(word, value, sentiment, sort = T) %>% 
  ungroup()

afinn_word_counts %>% 
  group_by(sentiment) %>% 
  top_n(15) %>% 
  ungroup() %>% 
  mutate(word = reorder(word,n)) %>% 
  ggplot(aes(word, n, fill=sentiment))+
  geom_col(show.legend = F)+
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="sentiment", x= "Words",
       title = "Afinn sentiment analysis on tweets")+
  coord_flip()


# 8.b


# Filter the inbound tweets
inbound_tweets2 <- clean_tweets_df %>%
  filter(inbound == "True")

# Perform AFINN sentiment analysis on inbound tweets and find the top 10 positive and negative words
afinn_word_counts_inbound <- inbound_tweets2 %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("afinn")) %>%
  mutate(sentiment = if_else(value > 0, 'positive', 'negative')) %>%
  count(word, value, sentiment, sort = TRUE) %>%
  ungroup()

  # Visualize and save the plot for the top 10 positive and negative words in inbound tweets 
afinn_word_counts_inbound %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Sentiment", x = "Words", title = "Top 10 (+) and (-) Words in Inbound Tweets using AFINN ") +
  coord_flip()


# 8.c

# Filter the outbound tweets
outbound_tweets2 <- clean_tweets_df %>%
  filter(inbound == "False")

# Perform AFINN sentiment analysis on outbound tweets and find the top 10 positive and negative words
afinn_word_counts_outbound <- outbound_tweets2 %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("afinn")) %>%
  mutate(sentiment = if_else(value > 0, 'positive', 'negative')) %>%
  count(word, value, sentiment, sort = TRUE) %>%
  ungroup()

# Visualize and save the plot for the top 10 positive and negative words in outbound tweets as 
afinn_word_counts_outbound %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Sentiment", x = "Words", title = "Top 10 (+) and (-) Words in Outbound Tweets using AFINN") +
  coord_flip()


# step9
# 9.a

get_sentiments("bing")


bing_word_counts <- words_in_tweets_df %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, sort = T) %>% 
  ungroup()

bing_word_counts %>% 
  group_by(sentiment) %>% 
  top_n(15) %>% 
  ungroup() %>% 
  mutate(word = reorder(word,n)) %>% 
  ggplot(aes(word, n, fill=sentiment))+
  geom_col(show.legend = F)+
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="sentiment", x= "Words",
       title = "Bing sentiment analysis on Tweets")+
  coord_flip()


# 9.b


# Perform Bing sentiment analysis on inbound tweets
bing_word_counts_inbound <- clean_tweets_df %>%
  filter(inbound == "True") %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

# Visualize and save the plot for the top 10 positive and negative words in inbound tweets using Bing sentiment analysis
bing_word_counts_inbound %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Sentiment", x = "Words", title = "Top 10 (+) and (-) Words in Inbound Tweets using Bing ") +
  coord_flip()


# 9.c

# Perform Bing sentiment analysis on outbond tweets
bing_word_counts_outbound <- clean_tweets_df %>%
  filter(inbound == "False") %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

# Visualize and save the plot for the top 10 positive and negative words in outbound tweets using Bing sentiment analysis
bing_word_counts_outbound %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Sentiment", x = "Words", title = "Top 10 (+) and (-) Words in outbound Tweets using Bing ") +
  coord_flip()


# step10
# 10.a

get_sentiments("nrc")

nrc_word_counts <- words_in_tweets_df %>% 
  inner_join(get_sentiments("nrc")) %>% 
  count(word, sentiment, sort = T) %>% 
  ungroup()


nrc_word_counts %>% 
  group_by(sentiment) %>% 
  top_n(10) %>% 
  ungroup() %>% 
  mutate(word = reorder(word,n)) %>% 
  ggplot(aes(word, n, fill=sentiment))+
  geom_col(show.legend = F)+
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="sentiment", x= "Words",
       title = "NRC sentiment analysis on Tweets")+
  coord_flip()




# 10.b

# Perform NRC sentiment analysis on inbound tweets
nrc_word_counts_inbound <- clean_tweets_df %>%
  filter(inbound == "True") %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

# Visualize and save the plot for the top 10 words for each emotion in inbound tweets using NRC sentiment analysis
nrc_word_counts_inbound %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ sentiment, scales = "free_y") +
  labs(y = "Sentiment", x = "Words", title = "Top 10 Words for Each Emotion in Inbound Tweets using NRC Analysis") +
  coord_flip()


# 10.c

# Perform NRC sentiment analysis on outbound tweets
nrc_word_counts_outbound <- clean_tweets_df %>%
  filter(inbound == "False") %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

# Visualize and save the plot for the top 10 words for each emotion in outbound tweets using NRC sentiment analysis
nrc_word_counts_outbound %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ sentiment, scales = "free_y") +
  labs(y = "Sentiment", x = "Words", title = "Top 10 Words for Each Emotion in outbound Tweets using NRC Analysis") +
  coord_flip()



# step11

# Extract the day of the week from the 'created_at' column
tweets_df$day_of_week <- weekdays(clean_tweets_addition$created_at)

# Plotting the number of tweets by day of the week
tweets_df %>%
  ggplot(aes(x = day_of_week)) +
  geom_bar(fill = "#ff5733") +
  labs(x = "Day of the Week", y = "Number of Tweets", title = "Number of Tweets by Day of the Week") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better visibility



##############################################################################################################################



