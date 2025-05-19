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

# hypothesis 8: disinformative videos contain much cuts
# frequency table
cut_tempo_table <- dataset %>%
  count(visual_cut_tempo) %>%
  filter(visual_cut_tempo > 0) %>%
  mutate(relative_freq = n / sum(n) * 100)
cut_tempo_plot <- ggplot(cut_tempo_table, aes(
  y = relative_freq,
  x = visual_cut_tempo
)) +
  geom_line() +
  annotate(
    "rect",
    xmin = 6.7, xmax = 28.1, ymin = 0.5, ymax = 4,
    fill = "red",
    alpha = 0.5
  ) +
  annotate(
    "text",
    x = 19.6, y = 5, label = "News-Video-Referenz",
    angle = 90,
    vjust = 0.5,
    check_overlap = TRUE,
    color = "red"
  ) +
  labs(
    x = "Anzahl Schnitte",
    y = "Relative Häufigkeit (%)",
    title = "Anzahl Schnitte pro Video",
    subtitle = "Relative Häufigkeit in %"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

auditive_specifications_table <- dataset %>%
  filter(fVar_artefact == 2) %>%
  count(auditive_specifications) %>%
  filter(auditive_specifications != 9) %>%
  mutate(auditive_specifications = fct_recode(
    as.factor(auditive_specifications),
    "Sprache" = "1",
    "Musik" = "2",
    "Umgebungsgeräusche" = "3",
    "Nicht erkennbar" = "9"
  )) %>%
  mutate(relative_freq = n / sum(n) * 100)
auditive_specifications_plot <- ggplot(auditive_specifications_table, aes(
  x = auditive_specifications,
  y = relative_freq
)) +
  geom_col(show.legend = FALSE) +
  labs(
    x = "Auditives Videomerkmal",
    y = "Relative Häufigkeit (%)",
    title = "Auditive Videomerkmale",
    subtitle = "Relative Häufigkeit in %"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# x^2 post_topic/auditive_specifications
topic_audio_chisq <- chisq.test(
  dataset$post_topic,
  dataset$auditive_specifications,
  simulate.p.value = TRUE,
  B = 10000
)

#############################################################

# Viewers
View(dataset)
View(cut_tempo_table)
print(cut_tempo_plot)

View(auditive_specifications_table)
print(auditive_specifications_plot)

print(topic_audio_chisq)

#############################################################

# Results
# Pearson's Chi-squared test with simulated p-value (based on 10000
#         replicates)
#   data:  dataset$post_topic and dataset$auditive_specifications
#   X-squared = 55.981, df = NA, p-value = 0.1121

# Reference videos for video length:
# SRF1: https://www.instagram.com/reel/DJyuZi7ItTw/: 7/38/60 -> 11.1
# SRF2: https://www.instagram.com/reel/DJyzGmlt5Ga/: 39/168/60 -> 13.9
# SRF3: https://www.instagram.com/reel/DJzJaiKNfaT/: 37/79/60 -> 28.1
# 20min1: https://www.instagram.com/reel/DJxgSInMc7j/: 6/21/60 -> 17.1
# 20min2: https://www.instagram.com/reel/DJynANvK-Pw/: 8/31/60 -> 15.5
# 20min3: https://www.instagram.com/reel/DJ0pgA7qpT9/: 7/31/60 -> 13.5
# nau1: https://www.instagram.com/reel/DJnw6vUK8aV/: 1/38/60 -> 1.6
# nau2: https://www.instagram.com/reel/DJodDaCta5i/: 4/28/60 -> 8.6
# nau3: https://www.instagram.com/reel/DJq4ufRtG6K/: 28/((4*60+11)/60-> 6.7
