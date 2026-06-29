# WORD VECTORS AND THEMATIC NEIGHBOURHOODS ----------------

# This script introduces word vectors as a way to explore how words are used
# near other words in a corpus. The aim is not to "discover meaning" directly,
# but to generate evidence that can be checked through close reading.

required_packages <- c("tidyverse", "text2vec", "tidytext")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

library(tidyverse)
library(text2vec)
library(tidytext)

load("corpus_docs.RData")

# Posit Cloud can be slower than a local computer. The prepared corpus is small
# enough for this exercise, but reduce n_documents if your session is slow.

n_documents <- 100
set.seed(123)

corpus_for_vectors <- corpus_docs %>%
  arrange(year, doc_id) %>%
  slice_head(n = n_documents) %>%
  mutate(text = str_to_lower(text))

# Tokenise the texts. The text2vec iterator streams the corpus efficiently.

tokens <- word_tokenizer(corpus_for_vectors$text)
it <- itoken(tokens, ids = corpus_for_vectors$doc_id, progressbar = FALSE)

# Build a vocabulary. Removing very rare words keeps the exercise fast and
# reduces noise, but it also removes some potentially interesting terms.

vocab <- create_vocabulary(it, stopwords = tidytext::stop_words$word) %>%
  prune_vocabulary(term_count_min = 20)

vectorizer <- vocab_vectorizer(vocab)

# The term co-occurrence matrix records which words appear near each other.
# A larger window means broader context; a smaller window means tighter context.

tcm <- create_tcm(it, vectorizer, skip_grams_window = 5)

# Train GloVe word vectors. These parameters are deliberately modest for class.

glove <- GlobalVectors$new(rank = 50, x_max = 10)
main_vectors <- glove$fit_transform(tcm, n_iter = 8, convergence_tol = 0.01)
context_vectors <- glove$components
word_vectors <- main_vectors + t(context_vectors)

# Helper: find words most similar to a target word or theme.

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

# Example 1: analyse a spatial theme.

space_terms <- c("city", "town", "street", "house", "room")

nearest_words(space_terms, word_vectors, n = 20)

# Example 2: compare two theme vocabularies.
# The aim is not to prove that these words "mean" urban or rural space. It is to
# see which words the model associates with each cluster, then return to KWIC and
# close reading to interpret the pattern.

urban_terms <- c("city", "town", "street")
rural_terms <- c("country", "field", "garden")

theme_neighbours <- bind_rows(
  nearest_words(urban_terms, word_vectors, n = 15) %>%
    mutate(theme = "urban: city/town/street"),
  nearest_words(rural_terms, word_vectors, n = 15) %>%
    mutate(theme = "rural: country/field/garden")
)

theme_neighbours

theme_neighbours %>%
  group_by(theme) %>%
  mutate(word = reorder_within(word, similarity, theme)) %>%
  ungroup() %>%
  ggplot(aes(x = similarity, y = word, fill = theme)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ theme, scales = "free_y") +
  scale_y_reordered() +
  labs(
    title = "Words associated with two spatial themes",
    subtitle = "Use these results as prompts for interpretation, then check examples in context",
    x = "Cosine similarity",
    y = NULL
  ) +
  theme_minimal()

# YOUR TURN -----------

# Choose a theme relevant to your own research.
# For example:
# - gender: c("woman", "mother", "daughter") vs c("man", "father", "son")
# - emotion: c("joy", "happy", "pleasure") vs c("grief", "sad", "sorrow")
# - power: c("king", "lord", "master") vs c("servant", "poor", "child")
#
# Replace the theme terms above and re-run the nearest_words() and theme-axis
# sections. Then inspect a few examples in context using KWIC in 03_corpus_analysis.R.


# GUIDED EXERCISES ---------------------------------------------------------

# Exercise 1: build one thematic neighbourhood.

my_theme <- c("power", "king", "lord", "master")
number_of_words <- 20

my_theme_neighbours <- nearest_words(
  target_terms = my_theme,
  word_vectors = word_vectors,
  n = number_of_words
)

my_theme_neighbours

my_theme_neighbours %>%
  mutate(word = reorder(word, similarity)) %>%
  ggplot(aes(x = similarity, y = word)) +
  geom_col(fill = "steelblue") +
  labs(
    title = paste("Words near:", paste(my_theme, collapse = ", ")),
    x = "Cosine similarity",
    y = NULL
  ) +
  theme_minimal()


# Exercise 2: compare two thematic neighbourhoods.

theme_one <- c("joy", "happy", "pleasure")
theme_two <- c("grief", "sad", "sorrow")

two_theme_neighbours <- bind_rows(
  nearest_words(theme_one, word_vectors, n = 12) %>%
    mutate(theme = "theme one"),
  nearest_words(theme_two, word_vectors, n = 12) %>%
    mutate(theme = "theme two")
)

two_theme_neighbours

two_theme_neighbours %>%
  group_by(theme) %>%
  mutate(word = reorder_within(word, similarity, theme)) %>%
  ungroup() %>%
  ggplot(aes(x = similarity, y = word, fill = theme)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ theme, scales = "free_y") +
  scale_y_reordered() +
  labs(
    title = "Comparing two thematic neighbourhoods",
    x = "Cosine similarity",
    y = NULL
  ) +
  theme_minimal()


# Exercise 3: check whether your seed words are in the model.

candidate_terms <- c("city", "village", "empire", "freedom")

tibble(term = candidate_terms) %>%
  mutate(in_model = term %in% rownames(word_vectors))

corpus_for_vectors %>%
  unnest_tokens(output = "word", input = "text") %>%
  filter(word %in% candidate_terms) %>%
  count(word, sort = TRUE)
