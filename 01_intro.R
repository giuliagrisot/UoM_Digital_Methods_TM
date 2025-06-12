### Welcome to "Text Mining with R"!

# This R script was created by Dr. Giulia Grisot
# Everything written after a hashtag (#) is a comment and will not be executed
# Everything else is R code
# To run code: place your cursor on a line (or highlight multiple lines)
# Then press Ctrl+Enter (Windows/Linux) or Cmd+Enter (Mac)
# This will send the code to the console for execution

### 1. Creating Variables

# Variables are like containers for data, and they can hold different types of information.
# In R, you can create variables using the assignment operator `<-` (or `=`)
# Here are some examples of different types of variables:

# Logical values (TRUE or FALSE)
my_logical <- TRUE
my_logical

# Numbers - numbers can be integers (whole numbers) or numerics (floating point numbers = they have decimals)
my_number <- 1
my_number

# Strings (text)
my_string <- "to be or not to be"
my_string

# Numeric vectors
my_first_vector <- c(1, 2, 3, 4, 5)
my_first_vector

# Shortcut to create a numeric sequence (1 to 5)
my_first_vector <- 1:5
my_first_vector

# Text (character) vectors
my_second_vector <- c("to", "be", "or", "not", "to", "be")
my_second_vector

# Lists (collections of different types of data)
my_list <- list(my_first_vector, my_second_vector)
my_list

# Dataframes (like spreadsheets)
my_df <- data.frame(
  author = c("Shakespeare", "Dante", "Cervantes", "Pynchon"),
  nationality = c("English", "Italian", "Spanish", "American")
)
View(my_df)

### Your Turn (1)

# Create a new dataframe with different authors and nationalities. Careful with the syntax!
my_new_df <- data.frame(
  author = 
  nationality = 
)

View(my_new_df)

### 2. Accessing Data
# Accessing elements in R variables is crucial for data manipulation and analysis.
# You can access elements in vectors, lists, and dataframes using indexing.

# Access elements in a logical variable
my_logical[1]         # First (and only) element

# Access elements in a number variable
my_number[1]         # First (and only) element

# Access elements in a character variable
my_string      # Whole string (not the first word)
# First 2 letters of the string
substr(my_string, 1, 5) # First five characters

# Access elements in a character vector
words[1]             # First word
words[1:4]          # First four words
words[c(1, 4)]      # First and fourth words
substr(words[1], 1, 2) # First two letters of the first word

# Access elements in a vector
my_first_vector[1]     # First element
my_second_vector[1:4]  # First four elements
my_second_vector[c(1, 4)] # First and fourth elements

# Access elements in a list
my_list[[1]]           # First item in the list
my_list[[2]][1:3]      # First 3 words from second item (vector of words)

# Access elements in a dataframe
my_df$author           # Column "author"
my_df[, 1]             # Same as above
my_df$author[1:2]      # First two authors
my_df[1, ]             # First row

# Find author by condition
my_df$author == "Dante"
which(my_df$author == "Dante")
my_df$nationality[which(my_df$author == "Dante")]

### Your Turn (2)

# Find the author who has "Spanish" nationality. Copy the code above and modify it to match the task!





### 3. Manipulating Variables

# Numeric operations
my_first_vector + 1
my_first_vector[2] + 1

# String operations
paste(my_string, "?")
strsplit(my_string, " ")
strsplit(my_string, " ")[[1]]
substr(my_string, 1, 5)

### Your Turn (3)

# Extract the second "be" from my_string
words <- strsplit(my_string, " ")[[1]]
words[which(words == "be")[2]]

### 4. Reading and Writing Text Files

# Working directory (where R reads and writes files)
getwd()
setwd("/cloud/project/scripts")
getwd()
setwd("/cloud/project")

# Read text file line by line
my_text <- readLines("scripts/samples/federer_pilatus_1912.txt")
head(my_text)

# Collapse text into a single string
my_text <- paste(my_text, collapse = "\n")

# Print and write text
cat("The cat is on the table")
cat("The cat is on the table", my_string)
cat("The cat is on the table", file = "scripts/samples/Cat.txt")

### Your Turn (4)

# Read another text file and split into words
my_text <- readLines("scripts/samples/another_sample.txt")
my_words <- strsplit(paste(my_text, collapse = " "), " ")[[1]]
head(my_words)

### 5. Loops and Conditionals

# Loop
for(i in 1:10){
  print(i)
}

# Conditional: if
for(i in 1:10){
  if(i == 1){
    print(i)
  }
}

# Conditional: if/else
for(i in 1:10){
  if(i <= 4){
    print(i)
  } else {
    print("more than four")
  }
}

### Your Turn (5)

# Modify the loop above to match the task (already done!)

### 6. Functions

# Basic function
my_function <- function(x){
  cat("Ciao", x, "\n")
}

my_function("Simone")
my_function("Giulia")

### Your Turn (6)

# Function with two inputs
teach_with <- function(name1, name2){
  cat(name1, "is teaching R with", name2, "\n")
}

teach_with("Simone", "Giulia")

### 7. Packages

# Install (only once)
# install.packages("tidyverse")

# Load (every time)
library(tidyverse)

# Base R vs tidyverse to filter data
my_df[which(my_df$nationality == "Italian"), ]  # Base R
my_df %>% filter(nationality == "Italian")      # tidyverse (dplyr)

### Congratulations!
# You've taken your first steps into text mining with R!
# Next: we'll start exploring and analysing real texts.
