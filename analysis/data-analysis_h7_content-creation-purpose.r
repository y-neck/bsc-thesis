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
library(lubridate) # for date parsing
library(here) # for project-based file paths
library(dplyr) # for pipes and rename
library(tidyr) # for pivot_wider()
library(forcats) # for fct_recode

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

# hypothesis 7: Disinformative information is shared with a specific purpose
# frequency table
creation_purpose_lut <- c(
  "Teilen von Informationen" = "11",
  "Augenzeugenberichte" = "12",
  "Intimitätsaustausch" = "21",
  "Persönlicher Standpunkt" = "22",
  "Unterhaltungszweck" = "30",
  "Andere" = "40",
  "Nicht erkennbar" = "99"
)

creation_purpose_table <- dataset %>%
  count(content_creation_purpose) %>%
  mutate(content_creation_purpose = fct_recode(
    as.factor(content_creation_purpose),
    !!!creation_purpose_lut
  )) %>%
  mutate(relative_freq = n / nrow(dataset) * 100) %>%
  arrange(match(content_creation_purpose, names(creation_purpose_lut)))

creation_purpose_plot <- ggplot(creation_purpose_table, aes(x = content_creation_purpose, y = relative_freq)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(
    x = "Inhaltlicher Erstellungszweck",
    y = "Relative Häufigkeit (%)",
    title = "Inhaltlicher Erstellungszweck",
    subtitle = "Relative Häufigkeit in %"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )


#############################################################

# Viewers
View(dataset)
View(creation_purpose_table)
print(creation_purpose_plot)


#############################################################

# Results
