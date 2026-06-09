# Text Analysis with R

Material for the University of Manchester Digital Methods Summer School 2026.

This repository contains the scripts and prepared data for the course sequence:

1. Introduction to R for text analysis
2. Corpus creation
3. Corpus analysis
4. Sentiment analysis

The scripts are designed to work in a local RStudio installation and in Posit
Cloud. For the smoothest setup, open `UoM_Digital_Methods_TM.Rproj` first so
that R uses the repository folder as the working directory.

## Setup

The main teaching scripts install missing packages when needed. If you prefer to
install everything before class, run:

```r
install.packages(c(
  "tidyverse",
  "tidytext",
  "readtext",
  "readxl",
  "syuzhet",
  "quanteda",
  "plotly",
  "sentimentr"
))
```

On Posit Cloud, package installation can take a few minutes the first time. Some
large corpus objects and sentiment scores are already included as `.RData` files
so students can inspect the workflow without having to recreate the slowest
processing steps during class.

## Course Sequence

- `01_intro.R`: R basics, data structures, reading and writing text files.
- `02_corpus_creation.R`: creating a corpus from text and spreadsheet files,
  adding metadata, splitting texts into sentences and tokens.
- `03_corpus_analysis.R`: inspecting the corpus, checking frequencies,
  stopwords, collocations, concordances, and word patterns over time.
- `04_SA.R`: sentiment analysis using lexicons and precomputed corpus objects.

Corpus creation and corpus analysis are central to the course. They shape what
sentiment analysis can and cannot tell us: the files selected, metadata added,
tokenisation choices, stopword lists, sampling decisions, and precomputed data
all affect the results.

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

`05_space.R` and `09_word_contexts.R` are exploratory extension scripts rather
than core material for the renamed course.
