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

# hypothesis 5: Visual artefacts are primarily of low complexity
# frequency table
artefact_type_lut <- c(
  "Bild ohne Bearbeitung" = "11",
  "Bild tiefer Komplexität" = "12",
  "Bild hoher Komplexität" = "13",
  "Bild: Irreführende Datenvisualisierung" = "14",
  "Bild: Eigens erstellte Grafik" = "15",
  "Video ohne Bearbeitung" = "21",
  "Video tiefer Komplexität" = "22",
  "Video mittlerer Komplexität" = "23",
  "Video hoher Komplexität" = "24",
  "Nicht erkennbar" = "99"
)

artefact_type_table <- dataset %>%
  count(visual_disinformation_category) %>%
  mutate(visual_disinformation_category = fct_recode(
    as.factor(visual_disinformation_category),
    !!!artefact_type_lut
  )) %>%
  mutate(relative_freq = n / nrow(dataset) * 100) %>%
  arrange(match(visual_disinformation_category, names(artefact_type_lut)))

artefact_type_plot <- ggplot(artefact_type_table, aes(x = visual_disinformation_category, y = relative_freq)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(x = "Art der Visualisierung", y = "Relative Häufigkeit", title = "Art der Visualisierung", subtitle = "Relative Häufigkeit in %") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  ) 


#############################################################

# Viewers
View(dataset)
View(artefact_type_table)
print(artefact_type_plot)


#############################################################

# Results
