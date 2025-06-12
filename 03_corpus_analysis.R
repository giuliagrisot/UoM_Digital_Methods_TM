### Welcome to "Corpus Linguistics with R"!

# This R script was created by Dr. Giulia Grisot
# In this script, we'll explore basic corpus linguistic techniques using a tokenised corpus
# To run code: place your cursor on a line (or highlight multiple lines)
# Then press Ctrl+Enter (Windows/Linux) or Cmd+Enter (Mac)

# Load required packages
library(tidyverse)
library(tidytext)
library(readtext)

# Clear the environment
rm(list = ls()) # this removes all objects from the environment

# Clear memory
gc() # this clears the memory

# # If you need to create the corpus yourself, this is the code:
# 
# corpus_token <- readtext("ELTEC_en_corpus/*.txt", encoding = "UTF-8") %>% # # read text files from the ELTEC English corpus
#   unnest_sentences(input = "text", # unnest sentences from the text
#                    output = "sentence", # output column for sentences
#                    to_lower = F, drop = T) %>% # keep original case and drop empty sentences
#   as_tibble() %>% # convert to tibble for easier manipulation
#   mutate(doc_id = str_remove(doc_id, ".txt$")) %>% # remove the .txt extension from doc_id
#   unnest_sentences(input = "sentence", "sentence", to_lower = F, drop = F) %>% # unnest sentences again to ensure each sentence is a separate row
#   mutate(sentence = str_squish(sentence)) %>% # eliminate unwanted extra spaces
#   group_by(doc_id) %>% # group by document ID
#   mutate(sentence_id = seq_along(sentence)) %>% # add a sentence id
#   ungroup() %>% # ungroup to remove grouping
#   unnest_tokens(input = "sentence", output = "token", to_lower = F, drop = F) %>% # split sentences into tokens
#   group_by(doc_id, sentence) %>% # group by document ID and sentence
#   mutate(token_id = seq_along(token)) %>% # add a token id by sequence along the tokens
#   ungroup() # ungroup to remove grouping
# 
# metadata <- readtext("ELTEC_en_corpus/ELTeC-eng_metadata.tsv",
#                      encoding = "UTF-8") %>%
#   rename(author = author.name, # rename author.name to author
#          year = reference.year) %>% # rename pub.year to reference.year
#   select(-doc_id) %>% # remove doc_id column
#   rename(doc_id = filename)
# 
# # Join metadata with the corpus
# corpus_token <- corpus_token %>%
#   left_join(metadata) # join metadata by doc id
# 
# save(corpus_token, file = "corpus_token.RData") # save the corpus to an RData file
# 
# # let's make a smaller sample with 300 sentences per book
# 
# # corpus_token_sample <- corpus_token_sample %>%
# #   group_by(doc_id, sentence_id) %>% # group by document ID and sentence ID
# #   slice_sample(prop = .1) %>% # take a 10% sample of sentences per book
# #   ungroup()
# 
# corpus_token_sample <- corpus_token %>%
#   filter(sentence_id < 1000) # filter to keep only the first 3000 sentences per book
# # Note: Adjust the number of sentences as needed, here we take a sample of 1000 sentences per book
# 
# # Save the sample corpus
# 
# save(corpus_token_sample, file = "corpus_token_sample.RData") # save the sample corpus to an RData file
# 
# 
# # Let's create a sentence corpus
# corpus_sentence <- corpus_token_sample %>%
#   group_by(sentence_id) %>%
#   select(doc_id, title, sentence, sentence_id, author, year) %>%
#   distinct() %>%
#   ungroup()
# 
# save(corpus_sentence, file = "corpus_sentence.RData") # save the sentence corpus to an RData file
# 
# # And a corpus where each document is a row
# corpus_docs <- corpus_sentence %>%
#   group_by(doc_id, title, author, year) %>%
#   summarise(text = paste(sentence, collapse = " "), .groups = 'drop') %>%
#   ungroup()
# 
# save(corpus_docs, file = "corpus_docs.RData") # save the document corpus to an RData file


# Load the corpus -----------

load("corpus_token_sample.RData") # load the corpus from the RData file
load("corpus_sentence.RData") # load the sentence corpus
load("corpus_docs.RData") # load the document corpus

### 1. Inspecting the Data -----------

# View the structure of the corpus
glimpse(corpus_token_sample)

# View the first few rows
head(corpus_token_sample)

# How many tokens in total?
nrow(corpus_token_sample)

# How many unique tokens (types)?
length(unique(corpus_token_sample$token))

# How many unique authors?
length(unique(corpus_token_sample$author))



## For the sake of this excercise we can preserve only the essential variables

corpus_token_sample <- corpus_token_sample %>%
  select(doc_id, sentence, token, sentence_id, title, author, year)

### 2. Frequency Analysis -----------

# Most frequent words in the corpus
token_freq <- corpus_token_sample %>%
  count(token, sort = TRUE)

head(token_freq, 20)  # Top 20 tokens

# https://www.matthewjockers.net/macroanalysisbook/expanded-stopwords-list/

stopwords_en <- readtext("jockers_stopwords.txt") %>%
  unnest_tokens(input = "text", output = "stopword") %>%
  mutate(token = tolower(stopword)) %>% # Ensure stopwords are lowercase
  select(token)

# Plot top 20 frequent tokens (excluding stopwords if needed)
token_freq %>%
  mutate(token = tolower(token)) %>%  # Convert tokens to lowercase for consistency
  anti_join(stopwords_en) %>%  # Exclude common stopwords
  slice_max(n, n = 20) %>%
  ggplot(aes(x = reorder(token, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 20 Tokens", x = "Token", y = "Frequency")

### Your Turn (1) -----------

# Count how many times the word "love" appears
corpus_token_sample %>% filter(token == "love") %>% nrow()

# Count top 10 tokens used by a specific author (e.g. Woolf)
corpus_token_sample %>%
  mutate(token = tolower(token)) %>%  # Convert tokens to lowercase for consistency
  anti_join(stopwords_en) %>%  # Exclude common stopwords
  filter(grepl("Woolf", author)) %>%
  count(token, sort = TRUE) %>%
  head(10)


# Count top 10 tokens used by a specific author (e.g. Dickens)
corpus_token_sample %>%
  mutate(token = tolower(token)) %>%  # Convert tokens to lowercase for consistency
  anti_join(stopwords_en) %>%  # Exclude common stopwords
  filter(grepl("Dickens", author)) %>%
  count(token, sort = TRUE) %>%
  head(10)



### 3. Collocations and Concordance -----------

# Collocations are frequently co-occurring words or phrases in a corpus. They can reveal patterns of meaning and usage, by identifying common phrases or word pairs that appear together more often than would be expected by chance.

# Find collocations (ngrams) in the corpus

library(tidytext)
library(quanteda)

# Now we can find ngrams (n-word phrases) in the texts
ngrams <- corpus_docs %>%
  unnest_ngrams(input = text, output = ngram, n = 4) %>% # Change n to 2 for bigrams, 3 for trigrams, etc.
  count(ngram, sort = TRUE) %>%
  filter(n > 10)  # Filter to keep only ngrams that appear more than 10 times


### Your Turn (2) -----------
# Find collocations (ngrams) in the corpus





# Concordance -----------

# Function to find concordance for a specific word

# Concordance is a method to find occurrences of a word or phrase in context, allowing us to see how it is used in different sentences. This is also referred to as keyword in context (KWIC). Quanteda provides a convenient way to do this.

corpus_q <- corpus(corpus_docs, text_field = "text")

# save(corpus_q, file = "corpus_q.RData") # save the corpus for later use

corpus_q_tokens <- tokens(corpus_q, remove_punct = TRUE, remove_symbols = TRUE) %>%
  tokens_tolower()

# save(corpus_q_tokens, file = "corpus_q_tokens.RData") # save the tokenised corpus for later use

kwic(corpus_q_tokens, pattern = "power", window = 3) %>%
  as.data.frame() %>%
  head(10)  # Display the first 10 concordance lines for the word "power"

# if you want to look at the various forms of a word, you can use the `*` symbol, fir instance:

kwic(corpus_q_tokens, pattern = c("beaut*"), window = 3) %>%
  as.data.frame() %>%
  head(10)  # Display the first 10 concordance lines for the word "beaut*"

# it is also possible to look for keywords in context (KWIC) of a phrase, for instance:

kwic(corpus_q_tokens, pattern = phrase("married man"), window = 3) %>%
  as.data.frame() %>%
  head(10)  # Display the first 10 concordance lines for the phrase "salt and pepper"




### Your Turn (3) -----------

# Try concordance for another word
# Change the window size to see more or fewer words around the keyword






### 5. Time-based Frequency -----------

# One useful analysis in corpus linguistics is to examine how the frequency of certain words changes over time. This can reveal trends in language use, cultural shifts, or thematic changes in literature.

# First, ensure the year is in numeric format
corpus_token_sample <- corpus_token_sample %>%
  mutate(year = as.numeric(year))

# Token frequency over time (e.g. "war")
corpus_token_sample %>%
  filter(token == "war") %>%
  count(year) %>%
  ggplot(aes(x = year, y = n)) +
  geom_line(color = "darkred") +
  labs(title = "Frequency of 'war' over time", x = "Year", y = "Frequency")


# Token frequency over time (e.g. "myself")
corpus_token_sample %>%
  filter(token == "myself") %>%
  count(year) %>%
  ggplot(aes(x = year, y = n)) +
  geom_line(color = "steelblue") +
  labs(title = "Frequency of 'myself' over time", x = "Year", y = "Frequency")



### Your Turn (4) ------------

# Plot the frequency of a different word (e.g. "peace") over time




### 6. Word Co-occurrence (within sentences) -----------

# Word pairs that co-occur in the same sentence can reveal interesting patterns of meaning and usage. This is often referred to as co-occurrence analysis.

# Find co-occurrence of two words (e.g. "war" and "peace")

co_occurrence <- corpus_sentence %>%
  filter(grepl("war", sentence, ignore.case = TRUE) & 
         grepl("peace", sentence, ignore.case = TRUE)) %>%
  select(sentence_id, sentence)

# Display the co-occurrence sentences

co_occurrence %>%
  head(10)  # Display the first 10 sentences where "war" and "peace" co-occur

### Your Turn (5) -----------

# Try co-occurrence for a different set of words -----------








### Well Done!
# You've completed a basic corpus linguistic analysis in R!
# You now know how to: inspect data, count tokens, search keywords, and explore patterns over time.
# Next steps might include POS tagging, collocations, or sentiment analysis.


# Clear the environment
rm(list = ls()) # this removes all objects from the environment

# Clear memory
gc() # this clears the memory
