# load libraries
library(httpgd)
# start the HTTP Graphics Device on default port
httpgd::hgd()
# direct base and ggplot graphics to httpgd
options(device = httpgd::hgd)

library(tidyverse)
library(ggplot2)
library(jsonlite)
library(readr) # for read_file
library(here) # for project-based file paths
library(dplyr) # for pipes and rename
library(tidyr) # for pivot_wider()
library(stringr) # for str_replace_all

# load and parse a JSON file into a tibble for tidyverse workflows
load_json_data <- function(file_path) {
  json_text <- read_file(file_path)
  json_data <- fromJSON(json_text)
}

# construct path via here()
json_path <- here("dataset.json")

# load the dataset
dataset <- load_json_data(json_path)

# clean up
# parse date and split comma-separated tags
dataset <- dataset %>%
  mutate(
    # parse date string to Date
    date = dmy(date)
  ) %>%
  # ensure user_generated_tags is always a list of strings
  mutate(
    user_generated_tags = map(user_generated_tags, ~ {
      # if already a character vector, leave as-is
      if (is.character(.x)) {
        return(.x)
      }
      # if empty or NULL, return empty character vector
      if (is.null(.x) || length(.x) == 0) {
        return(character())
      }
      # otherwise, flatten and keep as character
      unlist(.x)
    })
  )

#############################################################
### data analysis

# hypothesis 3: posts contain specific keywords
# Frequency and distribution analysis
tags_count <- dataset %>%
  select(id, user_generated_tags, keyword_coherence) %>%
  unnest(user_generated_tags) %>% # split comma-separated tags
  filter(!is.na(user_generated_tags) & user_generated_tags != "") %>% # drop NAs
  mutate(
    tag_clean = str_to_lower(user_generated_tags) %>%
      str_trim() %>%
      str_replace_all("[^\\p{L}\\d ]", "") # drop punctuation
  ) %>%
  count(tag_clean, sort = TRUE) %>%
  rename(freq = n)

# tag by topic
tags_long <- dataset %>%
  mutate(id = row_number()) %>%
  select(id, post_topic, user_generated_tags, keyword_coherence) %>%
  unnest(user_generated_tags) %>%
  filter(!is.na(user_generated_tags), user_generated_tags != "") %>%
  mutate(
    tag_clean = str_to_lower(user_generated_tags) %>%
      str_trim() %>%
      str_replace_all("[^\\p{L}\\d ]", "")
  )
# Now count tags per post_topic
tag_by_topic <- tags_long %>%
  # include keyword_coherence in the count grouping
  count(
    post_topic,
    tag_clean,
    keyword_coherence,
    name = "n"
  ) %>%
  group_by(post_topic) %>%
  ungroup() %>%
  arrange(desc(n))

tag_likeliness_by_topic <- dataset %>%
  # ensure user_generated_tags is always a list of strings
  mutate(
    has_tag = map_lgl(user_generated_tags, ~ length(.x) > 0)
  ) %>%
  # recode post_topic into human labels if not done already
  mutate(
    post_topic = fct_recode(
      as.factor(post_topic),
      "Politik" = "1",
      "Journalismus" = "2",
      "Wissenschaft/Technik" = "3",
      "Gesundheit/Medizin" = "4",
      "Gender/Race/Ethnien/Migration" = "5",
      "Sport/Kultur" = "6",
      "Wirtschaft" = "7",
      "Sonstige" = "8",
      "Nicht erkennbar" = "9"
    )
  ) %>%
  group_by(post_topic) %>%
  summarise(
    n_entries = n(),
    n_with_tags = sum(has_tag),
    prop_contains_tags = mean(has_tag) * 100 # percentage
  ) %>%
  ungroup()


#############################################################

# Viewers
View(dataset)
View(tags_count)
View(tag_by_topic)
View(tag_likeliness_by_topic)

#############################################################

# Results
