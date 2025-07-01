
# SPACE ----------

library(tidyverse)
library(tidytext)

load("corpus_docs.RData")

corpus_docs <- corpus_docs %>%
  sample_n(10) # for the sake of this example, we will only use 5 books

# if we want to analyse space, we need to "find" spatial items and label them.
# that can take some time and effort (you might need manual annotations for example)

# To simplify the example, we will stick to location names from geonames that are only in the UK. 

# We can use the datasets provided by the Geonames website, which are available here: https://download.geonames.org/export/dump/. We have downloaded the UK dataset, which is now it the folder "GB"

colnames_geo <- c("geonameid", "name", "asciiname", "alternatenames", "latitude", "longitude", "feature_class", "feature_code", "country_code", "cc2", "admin1_code", "admin2_code", "admin3_code", "admin4_code", "population", "elevation", "dem", "timezone", "modification_date")
#
geoloc_UK <- read_tsv("geolocations/GB.txt", col_names = F, show_col_types = FALSE) %>%
  as_tibble()

colnames(geoloc_UK) <- colnames_geo


# we can now filter the geonames dataset for the places that have a single word as name

geoloc_UK <- geoloc_UK %>%
  filter(!is.na(name) & !is.na(latitude) & !is.na(longitude)) %>%
  filter(!grepl(" ", name)) %>% # only single word names
  filter(feature_class == "P") %>% # for the sake of this let's focus on cities and villages
  select(name, latitude, longitude) %>%
  distinct()

## remove double entities if any (some have same multiple lon/lat) ------------

geoloc_UK <- geoloc_UK %>%
  group_by(name) %>%
  # only keep the first occurrence of each name
  slice(1) %>%
  ungroup() %>%
  # remove duplicates
  distinct()
  
  
# now we can add that to our corpus, too

corpus_token_sample_loc <- corpus_docs %>%
  unnest_sentences(input = text,
                output = sentence, 
                to_lower = F,
                drop = T) %>%
  unnest_tokens(input = sentence,
                output = token, 
                to_lower = F,
                drop = F) %>%
  inner_join(geoloc_UK %>%
              rename(token = name),
            relationship = "many-to-many")


# # We can see however that a lot of the 'matched' locations correspond to proper names or common words ("How", "Well"). One way to go about this is to redo the tokenization with udpipe and preserve the locations only, removing names of people.
# 
# # Udpipe uses a trained model to identify the parts of speech, and we can use that to filter out names of people.
# 
# # Load the udpipe model for English
# 
# library(udpipe)
# 
# ud_model_en <- udpipe_download_model(language = "english", overwrite = FALSE)
# 
# # Load the model
# ud_model_en <- udpipe_load_model(ud_model_en$file_model)
# 
# # Tokenize the corpus with udpipe. This might take some time, depending on the size of your corpus, so execute this if you have enough time and memory available. Else, just load the precomputed corpus_udpipe.RData file.
# 
# # corpus_udpipe <- udpipe_annotate(ud_model_en, x = corpus_docs$text) %>%
# #   as_tibble()
# # 
# # save(corpus_udpipe, file = "corpus_udpipe.RData")
# 
# load("corpus_udpipe.RData")
# 
# # Let's see how the matching work with this split
# 
# corpus_udpipe_LOC <- corpus_udpipe %>%
#   inner_join(geoloc_UK %>%
#               rename(token = name),
#             relationship = "many-to-many") %>%
#   distinct()

# or with the janitor package


# with spatial information from geonames.org, we can also plot space

library(tmap)
library(sf)
library(leaflet)

# maps ----------------------

# we can now plot the locations of the spatial entities in our corpus

corpus_LOC <- corpus_token_sample_loc %>%
  group_by(doc_id, token, latitude, longitude) %>%
  count() %>%
  ungroup() %>%
  rename(place = token)

DT_sf = st_as_sf(corpus_LOC, coords = c("longitude", "latitude"), crs = 4326)

tmap_mode("view")

tm_shape(DT_sf) +
  tm_dots(fill = "doc_id", size = "n", popup.vars = c("Place" = "place", "Count" = "n")) +
  tm_text(text = "place") +
  # tm_facets(by = "doc_id") +
  tm_layout(legend.outside = F)


