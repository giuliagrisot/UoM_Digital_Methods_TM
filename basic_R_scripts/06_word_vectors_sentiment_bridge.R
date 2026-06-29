# BRIDGE EXERCISE: WORD VECTORS AND SENTIMENT ----------------

# This optional script connects word vectors and sentiment analysis. It uses
# word vectors to suggest a thematic vocabulary, finds those words back in the
# sentence corpus, and then inspects the sentiment of the retrieved sentences.

required_packages <- c("tidyverse", "text2vec", "tidytext")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

library(tidyverse)
library(text2vec)
library(tidytext)

# Load the document-level corpus for word-vector training.
load("corpus_docs.RData")

# Load the sentence-level corpus for retrieving textual examples.
load("corpus_sentence.RData")

# Load precomputed sentence sentiment values to keep the exercise fast.
load("sentimentr_sent_values.RData")

# READY TO FILL: replace these terms with a theme for your own research.
theme_seed_terms <- c("power", "king", "law", "rule")

# READY TO FILL: give your theme a readable label for tables and plots.
theme_label <- "power and authority"

# READY TO FILL: choose how many neighbouring words to keep.
n_neighbours <- 15

# Choose how many documents to use for training. Reduce this on Posit Cloud if
# the model is slow.
n_documents <- 100

set.seed(123)

# Prepare the texts for the word-vector model.
corpus_for_vectors <- corpus_docs %>%
  arrange(year, doc_id) %>%
  slice_head(n = n_documents) %>%
  mutate(text = str_to_lower(text))

# Tokenise the texts and create a text2vec iterator.
tokens <- word_tokenizer(corpus_for_vectors$text)
it <- itoken(tokens, ids = corpus_for_vectors$doc_id, progressbar = FALSE)

# Build a vocabulary and remove very rare terms.
vocab <- create_vocabulary(it, stopwords = tidytext::stop_words$word) %>%
  prune_vocabulary(term_count_min = 20)

vectorizer <- vocab_vectorizer(vocab)

# Build a term co-occurrence matrix using a small context window.
tcm <- create_tcm(it, vectorizer, skip_grams_window = 5)

# Train a compact GloVe model for the exercise.
glove <- GlobalVectors$new(rank = 50, x_max = 10)
main_vectors <- glove$fit_transform(tcm, n_iter = 8, convergence_tol = 0.01)
context_vectors <- glove$components
word_vectors <- main_vectors + t(context_vectors)

# Helper function: find words that are close to one or more seed terms.
nearest_words <- function(target_terms, word_vectors, n = 20) {
  available_terms <- intersect(target_terms, rownames(word_vectors))

  if (length(available_terms) == 0) {
    stop("None of the target terms are in the vocabulary. Try more common words.")
  }

  target_vector <- colMeans(word_vectors[available_terms, , drop = FALSE])
  similarities <- sim2(
    x = word_vectors,
    y = matrix(target_vector, nrow = 1),
    method = "cosine",
    norm = "l2"
  )[, 1]

  tibble(
    word = names(similarities),
    similarity = as.numeric(similarities)
  ) %>%
    filter(!word %in% available_terms) %>%
    arrange(desc(similarity)) %>%
    slice_head(n = n)
}

# Check which seed terms are available in the model.
tibble(term = theme_seed_terms) %>%
  mutate(in_model = term %in% rownames(word_vectors))

# Find words close to the theme.
theme_neighbours <- nearest_words(
  target_terms = theme_seed_terms,
  word_vectors = word_vectors,
  n = n_neighbours
)

theme_neighbours

# Plot the thematic neighbourhood.
theme_neighbours %>%
  mutate(word = fct_reorder(word, similarity)) %>%
  ggplot(aes(x = similarity, y = word)) +
  geom_col(fill = "steelblue") +
  labs(
    title = paste("Words near", theme_label),
    x = "Cosine similarity",
    y = NULL
  ) +
  theme_minimal()

# Combine the original seed words and their nearest neighbours.
terms_to_trace <- c(theme_seed_terms, theme_neighbours$word) %>%
  unique()

terms_to_trace

# READY TO FILL: remove neighbours that are too broad for your theme.
terms_to_remove <- c("lived", "true", "meet")

# Keep only the terms that should be traced back into the corpus.
terms_to_trace <- setdiff(terms_to_trace, terms_to_remove)

terms_to_trace

# Build a search pattern for full-word matches.
theme_pattern <- paste0("\\b(", str_c(terms_to_trace, collapse = "|"), ")\\b")

# Retrieve sentences that contain at least one theme term.
theme_sentences <- corpus_sentence %>%
  mutate(sentence_lower = str_to_lower(sentence)) %>%
  filter(str_detect(sentence_lower, theme_pattern)) %>%
  mutate(matched_terms = str_extract_all(sentence_lower, theme_pattern)) %>%
  mutate(n_matched_terms = lengths(matched_terms)) %>%
  mutate(matched_terms = map_chr(matched_terms, ~ str_c(unique(.x), collapse = ", "))) %>%
  select(-sentence_lower)

theme_sentences %>%
  select(doc_id, title, author, year, sentence_id, matched_terms, sentence) %>%
  head(10)

# Count how often each traced term appears in the retrieved sentences.
theme_term_counts <- theme_sentences %>%
  separate_rows(matched_terms, sep = ", ") %>%
  count(matched_terms, sort = TRUE)

theme_term_counts

# Collapse sentiment values to one row per document sentence before joining.
sentence_sentiment_values <- sentimentr_sent_values %>%
  group_by(doc_id, sentence_id) %>%
  summarise(
    sentiment_value = mean(sentiment_value, na.rm = TRUE),
    .groups = "drop"
  )

# Join retrieved sentences to precomputed sentence-level sentiment.
theme_sentence_sentiment <- theme_sentences %>%
  left_join(
    sentence_sentiment_values,
    by = c("doc_id", "sentence_id")
  )

theme_sentence_sentiment %>%
  select(doc_id, title, author, year, sentence_id, matched_terms, sentiment_value, sentence) %>%
  head(10)

# Summarise sentiment for the retrieved theme sentences by document.
theme_sentiment_by_document <- theme_sentence_sentiment %>%
  group_by(doc_id, title, author, year) %>%
  summarise(
    n_sentences = n(),
    average_sentiment = mean(sentiment_value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(n_sentences))

theme_sentiment_by_document

# Plot average sentiment for documents with enough retrieved sentences.
theme_sentiment_by_document %>%
  filter(n_sentences >= 5) %>%
  slice_max(n_sentences, n = 20) %>%
  mutate(title = fct_reorder(title, average_sentiment)) %>%
  ggplot(aes(x = average_sentiment, y = title, size = n_sentences)) +
  geom_point(alpha = 0.7) +
  labs(
    title = paste("Sentiment of sentences about", theme_label),
    x = "Average sentence sentiment",
    y = NULL,
    size = "Sentences"
  ) +
  theme_minimal()

# READY TO FILL: choose how many example sentences to inspect.
n_examples <- 8

# Inspect the most negative retrieved sentences.
theme_sentence_sentiment %>%
  arrange(sentiment_value) %>%
  select(title, author, year, matched_terms, sentiment_value, sentence) %>%
  slice_head(n = n_examples)

# Inspect the most positive retrieved sentences.
theme_sentence_sentiment %>%
  arrange(desc(sentiment_value)) %>%
  select(title, author, year, matched_terms, sentiment_value, sentence) %>%
  slice_head(n = n_examples)

# GUIDED EXERCISE 1: change the theme.

# Replace these values, then rerun the script from the start.
theme_seed_terms <- c("city", "street", "house", "room")
theme_label <- "space and place"

# GUIDED EXERCISE 2: compare two theme vocabularies.

theme_a_terms <- c("city", "street", "house")
theme_b_terms <- c("country", "field", "garden")

theme_comparison <- bind_rows(
  nearest_words(theme_a_terms, word_vectors, n = 10) %>%
    mutate(theme = "theme A"),
  nearest_words(theme_b_terms, word_vectors, n = 10) %>%
    mutate(theme = "theme B")
)

theme_comparison

# GUIDED EXERCISE 3: interpret the limits.

theme_sentence_sentiment %>%
  mutate(
    sentiment_group = case_when(
      sentiment_value > 0 ~ "positive",
      sentiment_value < 0 ~ "negative",
      TRUE ~ "neutral"
    )
  ) %>%
  count(sentiment_group)
