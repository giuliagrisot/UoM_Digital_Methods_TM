### Corpus Creation with readtext ---
# Author: Dr. Giulia Grisot

# This script shows one consistent workflow for importing and preparing texts:
# 1. read files with readtext()
# 2. add metadata from filenames
# 3. split documents into sentences
# 4. split sentences into tokens

# Packages ----

required_packages <- c("tidyverse", "tidytext", "readtext")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

library(tidyverse)
library(tidytext)
library(readtext)


# Check working directory ----

# This should be the main project folder, where the samples folder is located.
getwd()


# Read one text file ----

# readtext() imports one file as one row with a doc_id and text column.
pride <- readtext("samples/austen_pride_1813.txt", encoding = "UTF-8")

# Inspect the imported document.
head(pride)

# Split the full text into one sentence per row.
pride_sentences <- pride %>%
  unnest_sentences(
    output = "sentence",
    input = "text",
    to_lower = FALSE
  ) %>%
  mutate(sentence = str_squish(sentence))

head(pride_sentences)


# YOUR TURN 1 ----

# Use the same readtext() workflow with samples/melville_moby_1851.txt.

melville <- readtext("samples/melville_moby_1851.txt", encoding = "UTF-8")

melville_sentences <- melville %>%
  unnest_sentences(
    output = "sentence",
    input = "text",
    to_lower = FALSE
  ) %>%
  mutate(sentence = str_squish(sentence))

head(melville_sentences)


# Read multiple text files ----

# The pattern samples/*.txt means all .txt files in the samples folder.
corpus_docs_small <- readtext("samples/*.txt", encoding = "UTF-8") %>%
  as_tibble()

head(corpus_docs_small)

# Check which files were imported.
corpus_docs_small %>%
  distinct(doc_id)

# Remove the demonstration file that contains "nospace" in the filename.
corpus_docs_small <- corpus_docs_small %>%
  filter(!str_detect(doc_id, "nospace")) %>%
  filter(!str_detect(doc_id, "Cat"))

corpus_docs_small %>%
  distinct(doc_id)


# Add metadata from filenames ----

# The filenames follow the pattern author_title_year.txt.
corpus_docs_small <- corpus_docs_small %>%
  separate(
    doc_id,
    into = c("author", "title", "year"),
    sep = "_",
    remove = FALSE
  ) %>%
  mutate(
    year = str_remove(year, "\\.txt$"),
    year = as.numeric(year)
  )

corpus_docs_small %>%
  select(doc_id, author, title, year) %>%
  head()


# Create a sentence-level corpus ----

corpus_sentence_small <- corpus_docs_small %>%
  unnest_sentences(
    input = "text",
    output = "sentence",
    to_lower = FALSE,
    drop = TRUE
  ) %>%
  mutate(sentence = str_squish(sentence)) %>%
  group_by(doc_id) %>%
  mutate(sentence_id = row_number()) %>%
  ungroup()

head(corpus_sentence_small)


# Check back for errors ----

# Sentence splitting may treat abbreviations such as "Mr." as complete
# sentences. First we diagnose the problem.

corpus_sentence_small %>%
  filter(str_detect(sentence, "^(Mr|Mrs|Ms|Miss|Dr|Prof|Rev|Hon|Capt|Col|Gen|Sgt|Jr|Sr|St|Mme|Mlle)\\.$")) %>%
  count(sentence, sort = TRUE)

corpus_sentence_small %>%
  filter(str_count(sentence, "\\S+") <= 2) %>%
  count(sentence, sort = TRUE) %>%
  head(20)

# A pragmatic fix is to protect dots in common titles before sentence splitting,
# then restore those dots afterwards. This is a partial solution, not a perfect
# sentence tokenizer. It may be wrong if an abbreviation genuinely appears at
# the end of a sentence, but it handles a common and visible workshop problem.

protect_title_dots <- function(text) {
  str_replace_all(
    text,
    "\\b(Mr|Mrs|Ms|Miss|Dr|Prof|Rev|Hon|Capt|Col|Gen|Sgt|Jr|Sr|St|Mme|Mlle)\\.",
    "\\1<ABBR_DOT>"
  )
}

restore_title_dots <- function(text) {
  str_replace_all(text, "<ABBR_DOT>", ".")
}

corpus_sentence_small <- corpus_docs_small %>%
  mutate(text = protect_title_dots(text)) %>%
  unnest_sentences(
    input = "text",
    output = "sentence",
    to_lower = FALSE,
    drop = TRUE
  ) %>%
  mutate(sentence = restore_title_dots(sentence)) %>%
  mutate(sentence = str_squish(sentence)) %>%
  group_by(doc_id) %>%
  mutate(sentence_id = row_number()) %>%
  ungroup()

head(corpus_sentence_small)

corpus_sentence_small %>%
  filter(str_detect(sentence, "^(Mr|Mrs|Ms|Miss|Dr|Prof|Rev|Hon|Capt|Col|Gen|Sgt|Jr|Sr|St|Mme|Mlle)\\.$")) %>%
  count(sentence, sort = TRUE)


# Create a token-level corpus ----

corpus_token_small <- corpus_sentence_small %>%
  unnest_tokens(
    input = "sentence",
    output = "token",
    to_lower = FALSE,
    drop = FALSE
  )

corpus_token_small <- corpus_token_small %>%
  group_by(doc_id, sentence_id) %>%
  mutate(token_id = row_number()) %>%
  ungroup()

head(corpus_token_small, 10)
