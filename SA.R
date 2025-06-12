
# SENTIMENT ANALYSIS ----------------

library(tidyverse)
library(syuzhet)
library(tidytext)

# Now let's do some Sentiment Analysis (SA). SA is 

load("corpus_token_sample.RData")
load("corpus_sentence.RData")
# load("corpus_docs.RData")

# you should now only have the data frames "corpus_docs", "corpus_sentence" and "corpus_token" in your environment.
# these two are the same, just in different shapes.
# you can check the content of both one more time to be sure

corpus_sentence %>% 
  select(author, doc_id, year) %>%
  distinct()

corpus_token_sample %>% 
  select(author, doc_id, year) %>%
  distinct()

# with this same scripts we can have a look at which and how many authors there are in our corpus

corpus_token_sample %>% 
  select(author) %>%
  distinct() 

corpus_token_sample %>% 
  select(author) %>%
  distinct() %>%
  nrow()


# YOUR TURN 2 ----------

# can you figure out how to print out which years are present in our corpus,
# and how many they are?






# Importing sentiment lexicons --------

# What you might want to do at this point, is to import the lexicons you will use for your sentiment analysis. 
# Lexicons are often available as part of several packages.
# for example, you may use the syuzhet package to import the nrc lexicons.

syuzhet_nrc <- get_sentiment_dictionary("nrc", language = "english")

# or even the lexicon provided by syuzhet itself

syuzhet_en <- get_sentiment_dictionary("syuzhet", language = "english")

# Notice that these measure different things. unless you want to compare performances,
# You might need to decide which lexicon works best for you, and be aware of how it was created.


# simple SA with syuzhet -----

# syuzhet allows you to run basic SA operations very easily, like extracting the sentiment value per sentence into a vector of values.

get_sentiment("I like apples and pears", language = "english", method = "syuzhet")
get_sentiment("I like apples and pears", language = "english", method = "bing")
get_sentiment("I like apples and pears", language = "english", method = "nrc")
get_sentiment("I like apples and pears", language = "english", method = "afinn")

get_sentiment("She was walking alone in a dark and eery night", language = "english", method = "syuzhet")

syuzhet::get_sent_values(char_v = c("I", "like", "apples", "and", "pears"), lexicon = "nrc")

syuzhet_sent_values <- corpus_sentence %>%
  group_by(sentence_id, sentence, doc_id) %>%
  mutate(sentiment_syuzhet = get_sentiment(sentence)) %>%
  ungroup()

# Tidy approach to SA ----------------
    
# I prefer to have a more "visual" (though more memory intensive) approach, applying the sentiment lexicon directly to our corpus (which gives us more control over what has been mapped as sentiment).

# for instance we can see the average sentiment per text:

library(tidytext)

corpus_token_sample %>%
  mutate(word = tolower(token)) %>%
  left_join(syuzhet_en, relationship = "many-to-many") %>%
  group_by(author, title) %>%
  summarise(sentiment_value = mean(value, na.rm=T)) %>%
  ungroup() %>%
  ggplot(aes(x=sentiment_value, y=reorder(title, sentiment_value), label=title)) +
  geom_bar(stat="identity", fill="steelblue") +
  # geom_label(size=3) +
  theme(legend.position = "none",
        # axis.text.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()

# We can do the same plot interactively, using plotly

if (!requireNamespace("plotly", quietly = TRUE)) {
  install.packages("plotly")
}

library(plotly)

p <- corpus_token_sample %>%
  mutate(word = tolower(token)) %>%
  left_join(syuzhet_en, relationship = "many-to-many") %>%
  group_by(author, title) %>%
  summarise(sentiment_value = mean(value, na.rm=T)) %>%
  ggplot(aes(x=sentiment_value, y=reorder(title, sentiment_value), label=title)) +
  geom_bar(stat="identity", fill="steelblue") +
  # geom_label(size=3) +
  theme(legend.position = "none",
        # axis.text.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()

ggplotly(p)


# or per pub_year

corpus_token_sample %>%
  mutate(word = tolower(token)) %>%
  left_join(syuzhet_en) %>%
  group_by(title, author, year) %>%
  summarise(sentiment_value = mean(value, na.rm=T)) %>%
  ggplot(aes(year, sentiment_value)) +
  geom_point(size=2) +
  geom_smooth(se = F) +
  geom_text(aes(label=paste0(title, ", ", author), vjust=-1.2), size = 3)


# or interactive:

p <- corpus_token_sample %>%
  mutate(word = tolower(token)) %>%
  left_join(syuzhet_en) %>%
  group_by(title, author, year) %>%
  summarise(sentiment_value = mean(value, na.rm=T)) %>%
  ggplot(aes(year, sentiment_value)) +
  geom_point(size=2) +
  geom_smooth() +
  geom_text(aes(label=paste0(title, ", ", author), vjust=-3), size = 3)

ggplotly(p, dynamicTicks = TRUE, tooltip = c("title", "author", "year", "sentiment_value"))


remove(p)


# Most times rather than full text sentiment, it is interesting to see how the sentiment changes within sentences.
# We can for example fin out the most positive and negative sentences in our corpus, and plot them as heatmaps, highlighting the most positive and negative words within the sentences.

corpus_token_sample %>%
  mutate(word = tolower(token)) %>%
  left_join(syuzhet_en, relationship = "many-to-many") %>%
  group_by(doc_id, sentence_id) %>%
  summarise(value = sum(value, na.rm=T)) %>%
  ungroup() %>%
  arrange(desc(value))


# Create a heatmap of the sentiment of the sentence 12540

test_sentence <- corpus_token_sample %>%
  filter(sentence_id == 212, doc_id == "ENG18670_Ouida") %>%
  mutate(word = tolower(token)) %>%
  left_join(syuzhet_en, relationship = "many-to-many")

test_sentence %>%
  mutate(line_number = rep(1:ceiling(n() / 10), each = 10, length.out = n())) %>%
  mutate(token_position = rep(1:10, length.out = n())) %>% # we can also use the row_number() function here
  ggplot(aes(x = token_position, # x-axis is the position of the word in the sentence
             y = -line_number, # y-axis is the line number (to have the first line on top)
             fill = value)) + 
  geom_tile() +
  geom_text(aes(label = token), size = 3) +
  scale_fill_viridis_c() +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle=60, hjust=1),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none") +
  labs(title = "Sentiment of the words in the selected sentence (using syuzhet lexicon)",
       subtitle = paste0(
         test_sentence$title[1], 
         " by ", 
         test_sentence$author[1],
         ", sentence n. ",
         test_sentence$sentence_id[1]
         ),
       x = "Word position",
       y = "Line number")

# YOUR TURN 3 -----------

# Can you do the same for the most negative sentence in the corpus?

# Hint: you can use the arrange() function to find the most negative sentence, and then filter() it out.


# Finally, we might want to look at sentiment over a certain narrative time, and we can do that for example by computing the sentiment per sentence.

# As we have seen above, this can be done fairly easily with the syuzhet package:

corpus_sentence %>%
  filter(doc_id == "ENG18471_Bronte") %>%
  group_by(sentence_id, sentence) %>%
  mutate(sentiment_syuzhet = get_sentiment(sentence)) %>%
  ggplot(aes(y = sentiment_syuzhet,  x = sentence_id)) +
  geom_point() +
  geom_smooth(se = F)



# Dealing with negation ---------------
# In a next step, we load the `sentimentr` package which allows us to extract negation-sensitive polarity scores for each sentences. In addition, we apply the `sentimentr` function to each sentence and extract their polarity scores.

if (!requireNamespace("sentimentr", quietly = TRUE)) {
  install.packages("sentimentr")
}

library(sentimentr)

# Let's have a look at how this woks:

# We can use the sentiment() function to extract the sentiment of a sentence, and it will return a data frame with the sentiment value for each sentence.

sentiment("I like apples and pears")
sentiment("I don't like apples and pears")

# We can see that the first sentence has a positive sentiment, while the second one has a negative sentiment, even though it contains the same words.
# We can also use the sentiment() function to extract the sentiment of a sentence, and it will return a data frame with the sentiment value for each sentence.

# Let's apply the sentiment() function to our corpus_sentence data frame, and plot the sentiment values for each sentence in a specific document.

# This can take a while, so only excecute the next line if you have really need to. Else, load the pre-computed values from the RData file.

# sentimentr_sent_values <- corpus_sentence %>%
#   group_by(doc_id, sentence_id, sentence) %>%
#   summarise(sentiment_value = sentiment(sentence)$sentiment) %>%
#   ungroup()
# 
# save(sentimentr_sent_values, file = "sentimentr_sent_values.RData")

load("sentimentr_sent_values.RData")

sentimentr_sent_values %>%
  filter(doc_id == "ENG18471_Bronte") %>%
  ggplot(aes(x = sentence_id, y = sentiment_value)) +
  geom_point() +
  geom_smooth(se = F) +
  labs(title = "Sentiment of the sentences in the selected document (using sentimentr package)",
       subtitle = paste0(
         "Document: ", 
         unique(corpus_sentence$title[corpus_sentence$doc_id == "ENG18471_Bronte"]),
         ", by ",
         unique(corpus_sentence$author[corpus_sentence$doc_id == "ENG18471_Bronte"])
       ),
       x = "Sentence ID",
       y = "Sentiment Value")


# We can see that the sentiment values are different, and that the sentimentr package is able to deal with negation in a more sophisticated way than syuzhet.

# Let's see how the sentiment values change if we caonsider the whole novels like we did before, but using the sentimentr package instead of syuzhet.

sentimentr_sent_values %>%
  group_by(doc_id) %>%
  summarise(sentiment_value = mean(sentiment_value, na.rm = TRUE)) %>%
  ungroup() %>%
  left_join(corpus_sentence %>% 
              select(doc_id, author, title, year) %>%
              distinct()) %>%
  ggplot(aes(x=sentiment_value, y=reorder(title, sentiment_value), label=title)) +
  geom_bar(stat="identity", fill="steelblue") +
  # geom_label(size=3) +
  theme(legend.position = "none",
        # axis.text.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()


