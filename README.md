# Text Analysis with R

Material for the University of Manchester Digital Methods Summer School 2026.

This repository contains Quarto notebooks, R scripts, and prepared data for the
course sequence:

1. Introduction to R for text analysis
2. Corpus creation
3. Corpus analysis
4. Sentiment analysis
5. Word vectors and thematic neighbourhoods
6. Optional bridge exercise: word vectors and sentiment

The notebooks and scripts are designed to work in a local RStudio installation
and in Posit Cloud. For the smoothest setup, open
`UoM_Digital_Methods_TM.Rproj` first so that R uses the repository folder as the
working directory.

## Setup

The main teaching notebooks install missing packages when needed. If you prefer
to install everything before class, run:

```r
install.packages(c(
  "tidyverse",
  "tidytext",
  "readtext",
  "syuzhet",
  "quanteda",
  "plotly",
  "sentimentr",
  "text2vec"
))
```

On Posit Cloud, package installation can take a few minutes the first time. Some
large corpus objects and sentiment scores are already included as `.RData` files
so students can inspect the workflow without having to recreate the slowest
processing steps during class.

## Using the Notebooks

Open the `.qmd` files in RStudio or Posit Cloud and run the code chunks in
order. Rendering the notebooks is optional; during the workshop, the main aim is
to execute, inspect, and adapt the code step by step.

## Course Sequence

- `01_intro.qmd`: R basics, data structures, reading and writing text files.
- `02_corpus_creation.qmd`: creating a corpus from text and spreadsheet files,
  adding metadata, splitting texts into sentences and tokens.
- `03_corpus_analysis.qmd`: inspecting the corpus, checking frequencies,
  stopwords, collocations, concordances, and word patterns over time.
- `04_sentiment_analysis.qmd`: sentiment analysis using lexicons and
  precomputed corpus objects.
- `05_word_vectors.qmd`: word-vector analysis for exploring thematic
  neighbourhoods and comparing how selected themes are represented in the
  corpus.
- `06_word_vectors_sentiment_bridge.qmd`: optional guided exercise connecting
  word vectors, corpus retrieval, and sentence-level sentiment.

The `basic_R_scripts/` folder contains companion runnable `.R` scripts with the
same core workflow. The `.qmd` files are the most student-facing version because
they combine explanation, code, outputs, and short exercises.

Corpus creation and corpus analysis are central to the course. They shape what
later analyses can and cannot tell us: the files selected, metadata added,
tokenisation choices, stopword lists, sampling decisions, and precomputed data
all affect the results of sentiment analysis, word-vector analysis, and any
other computational method.

## Limits and Implications

The prepared `.RData` files reduce processing time, especially on Posit Cloud,
but they also hide earlier decisions in the workflow. Students should treat them
as inspectable teaching objects rather than neutral data. Before interpreting
results, check what texts are included, how they were sampled, which metadata is
available, and whether the corpus is appropriate for the research question.

Lexicon-based sentiment analysis is useful for exploring patterns, but it is not
a direct measure of meaning, emotion, or reader response. Results depend on the
lexicon, language, genre, historical period, negation handling, and the unit of
analysis. Corpus analysis should therefore be used before and alongside
sentiment analysis to identify biases, errors, absences, and interpretive limits.

Word vectors are useful for exploring semantic neighbourhoods: which words tend
to appear in similar contexts, and how a theme such as space, gender, power, or
emotion is distributed across a corpus. They are also shaped by corpus selection,
frequency thresholds, context windows, and model parameters. Treat word-vector
results as prompts for interpretation rather than as final evidence, and check
important patterns through concordances and close reading.

`basic_R_scripts/06_space.R` and `basic_R_scripts/09_word_contexts.R` are older
exploratory extension scripts.
