# Datafile for the milestone report that we will be utilizing the Coursera Swiftkey information
# In addition we will be running some algorithms in order to complete some analysis.  

setwd("path")
list.files()

library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(plotly)
library(tidytext)
library(stringi)
library(igraph)
library(ggraph)

train_size <- 0.7
test_size  <- 1 - train_size


blog <- readLines("en_US.blogs.txt", encoding = "UTF-8", warn = FALSE, skipNul = TRUE)
news <- readLines("en_US.news.txt", encoding = "UTF-8", warn = FALSE, skipNul = TRUE)
twitter <- readLines("en_US.twitter.txt", encoding = "UTF-8", warn = FALSE, skipNul = TRUE)
#  In the next step we will be looking at doing some basic analysis of the files we downloaded

data(stop_words)
news_df    <- data_frame(line = 1:length(news), text = news) %>% mutate(Text_Source = "news")
blog_df    <- data_frame(line = 1:length(blog), text = blog) %>% mutate(Text_Source = "blog")
twitter_df <- data_frame(line = 1:length(twitter), text = twitter) %>% mutate(Text_Source = "twitter")
combined_df <- bind_rows(news_df, blog_df, twitter_df)

news_words     <- news_df %>% 
                   unnest_tokens(word, text) %>%
                    filter(!str_detect(word,"[0-9]"))
blog_words     <- blog_df %>% 
                    unnest_tokens(word, text) %>%
                     filter(!str_detect(word,"[0-9]"))
twitter_words  <- twitter_df %>% 
                    unnest_tokens(word, text) %>%
                      filter(!str_detect(word,"[0-9]"))
# combined_words <- combined_df %>% unnest_tokens(word, text)

# We use the tidy text "unnest_tokens".  By using unnest tokens we get
#   Punctuation has been stripped
#   Converts the tokens to lower case

WPL <- sapply(list(blog,news,twitter),
            function(x) summary(stri_count_words(x))[c('Min.','Mean','Max.')])
colnames(WPL) <- c("News_WPL", "Blog_WPL", "Twitter_WPL")
Word_Counts <- as.data.frame(cbind(nrow(news_words), nrow(blog_words), nrow(twitter_words)))
Line_Counts <- as.data.frame(cbind(length(news), length(blog), length(twitter)))

Summary_Statistics <- bind_rows(Word_Counts, Line_Counts)
colnames(Summary_Statistics) <- c("News_Statistics", "Blog_Statistics", "Twitter_Statistics")
rownames(Summary_Statistics) <- c("Word_Counts", "Line_Counts")

## Now let's take a look at the most common words.  
#  We will remove common stop words to make the analysis a little more targeted
news_words <- news_words %>%
              anti_join(stop_words, by = c("word"))
blog_words <- blog_words %>%
                anti_join(stop_words, by=c("word"))
twitter_words <- twitter_words %>%
                  anti_join(stop_words, by=c("word"))

combined_words <- bind_rows(news_words, blog_words, twitter_words)

news_words %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill = "dark green") +
  ggtitle("News Top 10 Words") +
  xlab(NULL) +
  coord_flip()
  
blog_words %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill = "red") +
  ggtitle("Blog Top 10 Words") +
  xlab(NULL) +
  coord_flip()
  
twitter_words %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill = "#00aced") +
  ggtitle("Twitter Top 10 Words") +
  xlab(NULL) +
  coord_flip()

combined_words %>%
  count(word, sort = TRUE) %>%
  top_n(20) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill = "#1dcaff") +
  ggtitle("Combined Top 20 Words") +
  xlab(NULL) +
  coord_flip()

#  Will do a random samle of the dataframe for bi-grams and trigrams due to the size 

bigram_filtered <- combined_df %>%
                    group_by(Text_Source) %>%
                    sample_frac(0.05) %>%
                    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
                    separate(bigram, c("word1", "word2"), sep = " ") %>%
                    filter(!word1 %in% stop_words$word) %>%
                    filter(!word2 %in% stop_words$word) %>%
                    filter(!str_detect(word1,"[0-9]")) %>%
                    filter(!str_detect(word2,"[0-9]")) 
                    
bigram_united <-    bigram_filtered %>%
                    unite(bigram, word1, word2, sep = " ")  

set.seed(215)
bigram_graph <- bigram_filtered %>% 
                  count(word1, word2, sort = TRUE) %>%
                  filter(n > 35) %>%
                  graph_from_data_frame() 

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

trigram_filtered <- combined_df %>%
                      group_by(Text_Source) %>%
                      sample_frac(0.05) %>%
                      unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
                      separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
                      filter(!word1 %in% stop_words$word) %>%
                      filter(!word2 %in% stop_words$word) %>%
                      filter(!word3 %in% stop_words$word) %>%
                      filter(!str_detect(word1,"[0-9]")) %>%
                      filter(!str_detect(word2,"[0-9]")) %>%
                      filter(!str_detect(word3,"[0-9]"))

trigram_united <-    trigram_filtered %>%
                        unite(bigram, word1, word2, word3, sep = " ")  

set.seed(215)
trigram_graph <- trigram_filtered %>% 
  count(word1, word2, word3, sort = TRUE) %>%
  filter(n > 5) %>%
  graph_from_data_frame() 

ggraph(trigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

  
           trigram_filtered %>%
                    count(word1, word2, word3, sort = TRUE) %>%
                    unite(trigram, word1, word2, word3, sep = " ") %>%
                    filter(n > 16) %>% 
                    ggplot(aes(trigram, n)) +
                    geom_col(fill = "#1dcaff") +
                    ggtitle("Combined Top 11 Words") +
                    xlab(NULL) +
                    coord_flip()
           
           
# word_df <- list(news_df, blog_df, twitter_df, combined_df)
# names.list <- c("news", "blog", "twitter", "combined")
# names(word_df) <- names.list
# 
# lapply(names(word_df), function(df)
#   saveRDS(word_df[[df]], file = paste0(df, ".rds")))
