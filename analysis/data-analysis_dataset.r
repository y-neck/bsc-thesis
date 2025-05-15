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

## metadata

# original_platform distribution
origin_platform_table <- dataset %>%
  count(original_platform) %>%
  # Relabel with forcats recode
  mutate(original_platform = fct_recode(
    as.factor(original_platform),
    "LinkedIn" = "11",
    "Instagram" = "12",
    "Faceboock/fbcdn" = "13",
    "SnapChat" = "14",
    "Pinterest" = "15",
    "TikTok" = "16",
    "X/Twitter" = "17",
    "YouTube" = "18",
    "Meta Threads" = "19",
    "Telegram" = "20",
    "Desinformative Website" = "30",
    "Sonstige" = "90"
  )) %>%
  # calculate relative frequency and add as new col
  mutate(relative_freq = n / nrow(dataset) * 100)
origin_platform_plot <- ggplot(
  origin_platform_table,
  aes(
    x = original_platform, # x axis assignment
    y = relative_freq,
    # fill = original_platform # bar fill color
  )
) +
  geom_bar(
    stat = "identity",
    show.legend = FALSE # do not show legend
  ) +
  labs(
    x = "Ursprungsplattform", # change x axis label
    y = "Relative Häufigkeit",
    title = "Ursprungsplattform der Beiträge",
    subtitle = "Relative Häufigkeit in %",
    fill = "Ursprungsplattform"
  ) +
  # scale_fill_brewer(palette = "Dark2") +
  theme_minimal() + # minimal graphics theme, remove grey bg
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

# post topics distribution
post_topic_table <- dataset %>%
  count(post_topic) %>%
  # Relabel with forcats recode
  mutate(post_topic = fct_recode(
    as.factor(post_topic),
    "Politik" = "1",
    "Journalismus" = "2",
    "Wissenschaft" = "3",
    "Gesundheit" = "4",
    "Identität" = "5",
    "Sport/Kultur" = "6",
    "Wirtschaft" = "7",
    "Sonstige" = "8",
    "Nicht erkennbar" = "9"
  )) %>%
  mutate(relative_freq = n / nrow(dataset) * 100)
post_topic_plot <- ggplot(post_topic_table, aes(x = post_topic, y = relative_freq)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(x = "Themengebiet", y = "Relative Häufigkeit", title = "Themengebiet der Beiträge", subtitle = "Relative Häufigkeit in %") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

# artefact type distribution
artefact_type_table <- dataset %>%
  count(fVar_artefact) %>%
  mutate(fVar_artefact = fct_recode(
    as.factor(fVar_artefact),
    "Bild" = "1",
    "Video" = "2"
  )) %>%
  mutate(relative_freq = n / nrow(dataset) * 100)
artefact_type_plot <- ggplot(artefact_type_table, aes(x = fVar_artefact, y = relative_freq)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(x = "Artefakt-Typ", y = "Relative Häufigkeit", title = "Mediales Artefakt", subtitle = "Relative Häufigkeit in %") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

# xTab origin country/reference country
# country labels
country_lut <- c(
  "1" = "Schweiz",
  "2" = "Deutschland",
  "3" = "Österreich",
  "4" = "USA",
  "5" = "Grossbritannien",
  "6" = "Israel",
  "7" = "Rumänien",
  "8" = "Frankreich",
  "9" = "Ukraine",
  "10" = "Taiwan",
  "11" = "Italien",
  "12" = "Israel",
  "13" = "Griechenland",
  "14" = "Türkei",
  "15" = "Dänemark",
  "16" = "Ägypten",
  "17" = "Katar",
  "18" = "Russland",
  "19" = "Kanada",
  "20" = "Indien",
  "21" = "Portugal",
  "22" = "Australien",
  "23" = "Polen",
  "24" = "Luxemburg",
  "25" = "Tschechien",
  "26" = "Iran",
  "27" = "Slowenien",
  "99" = "Nicht erkennbar"
)
# crosstab
country_xtabs <- xtabs(
  ~ source_country + content_reference_country,
  data = dataset
)
# turn crosstab into tibble
country_txtable <- as.data.frame(country_xtabs) %>%
  # rename columns
  rename(
    source_country = source_country,
    content_reference_country = content_reference_country,
    count = Freq
  ) %>%
  # recode both cols simultanously
  mutate(
    across(
      c(source_country, content_reference_country),
      ~ recode_factor(
        .x,
        !!!country_lut,
        .default = "Andere" # fallback label if a code is unexpected
      )
    )
  ) %>%
  # pivot wider for display as contingency table
  pivot_wider(
    id_cols = source_country,
    names_from = content_reference_country,
    values_from = count,
    values_fn = list(count = sum), # sum duplicates into one cell
    values_fill = 0 # fill missing combos with zero
  )
country_plot <- country_txtable %>%
  pivot_longer(
    cols      = -source_country,
    names_to  = "content_reference_country",
    values_to = "count"
  ) %>%
  filter(count > 0) %>%
  # 2) ensure ordering by numeric code, not alphabet
  mutate(
    content_reference_country = factor(
      content_reference_country,
      levels = names(country_txtable)[-1]
    ),
    # fix y‐levels to the exact row order in country_txtable
    source_country = factor(
      source_country,
      levels = country_txtable$source_country
    )
  ) %>%
  ggplot(aes(
    x = content_reference_country,
    y = source_country,
    color = count,
    size = count
  )) +
  geom_point(alpha = 0.5) +
  scale_size(range = c(1, 20)) +
  scale_fill_viridis_d() +
  labs(
    x = "Bezugsland",
    y = "Ursprungsland",
    title = "Verhältnis Ursprungs- vs. Bezugsland",
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(), # remove minor grid lines
    panel.grid.minor = element_blank(), # remove minor grid lines
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5),
    legend.position = "none" # hide legend
  )
country_txtable_relfreq <- country_xtabs %>%
  prop.table(1) # row proportions

#############################################################

# Viewers
View(dataset)

View(origin_platform_table)
print(origin_platform_plot)

View(post_topic_table)
print(post_topic_plot)

View(artefact_type_table)
print(artefact_type_plot)

View(country_txtable)
View(country_txtable_relfreq)
print(country_plot)
