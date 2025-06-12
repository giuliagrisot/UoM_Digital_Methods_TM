### Basic Text Mining with R ---
# Author: Dr. Giulia Grisot

# This script is for complete beginners and combines two elements:
# - How to import and prepare literary texts in R
# - Basic text mining tasks (tokenisation, word frequencies, concordances)

# Packages -----

# Before you begin you will need to load some packages. These allow you to execute specific operations.
# If you have not done so already, you have to install them first: it might take a few minutes and you only have to do it once. If R asks you whether you want to install dependencies for the packages, say yes.

if (!requireNamespace(c("tidyverse", "tidytext", "readtext", "readxl", "syuzhet"), quietly = TRUE)) {
  install.packages(c("tidyverse", "tidytext", "readtext", "readxl", "syuzhet"))
}

# After you have installed the packages, you can load them with the library function. You will need to do this every time you start a new R session.

library(tidyverse)
library(tidytext)
library(readtext)
library(readxl)
library(syuzhet)


# Importing data ----

## txt ----

# One easy way to import texts into R is to start from txt files.

# You might have more than one file, so it is important that you store them all together in one folder, and ideally with a consistent filename. Information in the filename can be used later on to add metadata to your dataset. The format "surname_title_year.txt" could be a good option, for example, where the surname and the title have to be one word.

# In order to import a txt file, you can use the "read.delim" function from base R (which means you do not need to install extra packages). 

# let's try it out. As you can see in the files panels, there is a folder called "samples", where some texts in different formats are stored.

# before you execute the code, make sure the working directory is set to your main repository folder (the one "above" the /samples folder)

getwd() # this should be the main folder of your repository, where the samples folder is located

pride <- read.delim("samples/austen_pride_1813.txt", # this is the url to your file
                              fileEncoding = "utf-8",  # we want to read it as unicode text
                              header = F) %>% # we do not want the first line as a header 
  rename(text = V1) # we can name the column text

# your file has been imported! in this case, it looks just fine.

# It could be that your texts has not the right structure. If so, you can manipilate it and fix it to your needs.
# For example, if you download pride and prejudice from Project Gutenberg, you will see that the raws represent the line breaks of the book layout, rather than the sentences or paragraphs.
# In such cases, we can merge the lines together, and then split them into sentences.

pride2 <- gutenbergr::gutenberg_download(1342)

pride2 <- paste(pride2$text, collapse = " ") %>% # this merges all lines together
  as.data.frame() %>% # this converts it into a dataframe
  rename(text = ".") %>% # and we rename the column
  mutate(text = str_squish(text)) %>% # this removes unwanted extra spaces
  mutate(text = str_trim(text, side = "both")) %>% # and this removes unwanted spaces before and after the text
  unnest_sentences(input = "text", # this splits the text into sentences
                   output = "sentence", 
                   to_lower = F) # and we do not want to convert it to lower case

# let's have a look at the first few sentences now
head(pride2)

# YOUR TURN 1 ---------

# can you create a corpus with another file in the samples folder?
# Try importing the melville.txt file, and then split it into sentences.








## Multiple txt files ----------

# Let's first empty our environment, so that we can start fresh
rm(list = ls()) # this removes all objects in the environment

# if you have more than one text, you probably won't want to repeat this operations manually several times.
# you can then proceed as follows:
# (this is just one way but there are many out there)

# run the "readtext" function from the "readtext" package, simply indicating the folder in which your texts are stored, and the format preceded by "*." (this means "all files that have this extension").

corpus <- readtext("samples/*.txt", encoding = "UTF-8")  %>%
  unnest_sentences(input = "text",
                   output = "sentence",
                   to_lower = F, drop = T) %>%
  as_tibble()

head(corpus)


# let's see which files are in our corpus:

corpus %>% 
  select(doc_id) %>%
  distinct()

# we can see that we have a double 'pride and prejudice' file, so we can remove the 'nospace' version

corpus <- corpus %>%
  filter(!grepl("nospace", doc_id)) # this means "filter out all rows that have 'nospace' in the doc_id column"

# let's have a look at the files again

corpus %>% 
  select(doc_id) %>%
  distinct()

# now, as we mentioned you might want to use the information in the filename to create more variables (that's how "columns" are called in R) in our corpus

corpus <- corpus %>%
  mutate(sentence = str_squish(sentence)) %>% # eliminate unwanted extra spaces
  separate(doc_id, into = c("author", "title", "year"), sep = "_", remove = T) %>% # and separate the metadata
  mutate(year = str_remove(str_trim(year, side = "both"), ".txt")) # and make sure there are no extra spaces before/after the words

corpus$year <- as.numeric(corpus$year)

# let's see how it looks

head(corpus)

# Neat, right?


# you might also want to add an identification number for the sentences, which can be useful for later analysis

corpus <- corpus %>%
  group_by(title) %>%
  mutate(sentence_id = seq_along(sentence)) %>% # this means "sequence along the column sentence"
  ungroup()

# and we might want then to split the text into tokens. we can easily use the unnest_tokens function of tidytext:

corpus_token <- unnest_tokens(corpus, input = "sentence", output = "token", to_lower = F, drop = F)

# as we did for sentences, we might want to preserve the position of the tokens insider sentences, and add a token_id index

corpus_token <- corpus_token %>%
  group_by(title, sentence) %>%
  mutate(token_id = seq_along(token)) %>% # this means "sequence along the column "token"
  ungroup()


# so let's see how does it look now

head(corpus_token, 10)

# splitting into tokens can be useful if we want to match our corpus to lists of 
# labelled data (for instance, locations or sentiment lexicons).
# We'll talk about this during the SA session.





## csv and xslx ----

# another common format for texts is csv or xlsx. Importing a this is very easy, because R understands the csv and xslx formats well. You can either use code, or click directly on the file you want to import in the files panel.
# R studio will ask if you want to import it, and you will be able to determine options with a friendly interface.

# navigate into the samples folder and click on the file small_corpus.xlsx. or 
# execute the following code

pride_excel <- read_excel("samples/pride.xlsx")

# have a look at it

head(pride_excel)


## multiple files 2.0 ----

# There are many ways to import files in R: this one is similar to the one we saw for the .txt files, except it has `readtext` as function, and it does not need any variable to be specified.

corpus_source <- readtext("samples/*.xlsx") # here we are looking for all xlsx files in the samples folder

head(corpus_source)


