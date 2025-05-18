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

# hypothesis 6: Usually, the disinformational part is the post, accompanied by a supporting visual artefact
# frequency table
visual_coherence_lut <- c(
  "Text sachlich, Artefakt sachlich" = "1",
  "Text sachlich, Artefakt desinformativ" = "2",
  "Text desinformativ, Artefakt sachlich" = "3",
  "Text und Artefakt desinformativ" = "4",
  "Nicht erkennbar" = "9"
)

visual_coherence_table <- dataset %>%
  count(visual_coherence) %>%
  mutate(visual_coherence = fct_recode(
    as.factor(visual_coherence),
    !!!visual_coherence_lut
  )) %>%
  mutate(relative_freq = n / nrow(dataset) * 100) %>%
  arrange(match(visual_coherence, names(visual_coherence_lut)))

visual_coherence_plot <- ggplot(visual_coherence_table, aes(x = visual_coherence, y = relative_freq)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(
    x = "Verhältnis zwischen Beitrag und Artefakt",
    y = "Relative Häufigkeit (%)",
    title = "Bildinhaltliche Koherenz",
    subtitle = "Relative Häufigkeit in %"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

# topic plot for visual coherence
topic_coherence_plot_table <- dataset %>%
  # extract code_str and derive super_topic before recoding
  mutate(
    code_str = as.character(visual_disinformation_topic),
    super_topic = case_when(
      code_str %in% c("10", "11", "12", "13", "14", "19") ~ "Politik",
      code_str %in% c("20", "21", "22", "23", "29") ~ "Journalismus",
      code_str %in% c("30", "31", "32") ~ "Wissenschaft",
      code_str %in% c("40", "41", "42", "49") ~ "Gesundheit",
      code_str %in% c("51", "52", "53") ~ "Gesellschaft",
      code_str %in% c("61", "62") ~ "Kultur",
      code_str %in% c("71", "72", "79") ~ "Wirtschaft",
      code_str == "80" ~ "Weitere",
      code_str == "99" ~ "Nicht erkennbar",
      TRUE ~ NA_character_
    ),
    # now recode both dimensions exactly once
    post_topic = fct_recode(
      as.factor(post_topic),
      "Politik" = "1",
      "Journalismus" = "2",
      "Wissenschaft" = "3",
      "Gesundheit" = "4",
      "Sport" = "5",
      "Kultur" = "6",
      "Wirtschaft" = "7",
      "Bildung" = "8",
      "Weitere" = "9",
      "Nicht erkennbar" = "99"
    ),
    visual_disinformation_topic = fct_recode(
      as.factor(visual_disinformation_topic),
      "Personen der Politik" = "11",
      "Eigenes Statement einer politischen Person/Partei/Organisation" = "12",
      "Politische Medienmitteilung (offiziell)" = "13",
      "Politisches/juristisches Dokument" = "14",
      "Weitere politische/politik-bezogene Abbildung" = "19",
      "Journalistischer Artikel" = "21",
      "Nachrichtenmoderation" = "22",
      "Weitere journalistisches Produkt" = "23",
      "Weitere Journalismus-bezogene Abbildung" = "29",
      "Wissenschaftliche/Technik-Komm." = "30",
      "Wissenschaftliche/Technik-Komm. mit Quelle" = "31",
      "Wissenschaftliche/Technik-Komm. ohne Quelle" = "32",
      "Gesundheit/Medizin" = "40",
      "Klinikpersonal" = "41",
      "Gesundheits-Expertin/Experte" = "42",
      "Weitere Gesundheitsthemen" = "49",
      "Gender" = "51",
      "Race/Ethnien" = "52",
      "Migration" = "53",
      "Sport" = "61",
      "Kultur" = "62",
      "Werbung" = "71",
      "Wirtschaftliche Kommunikation" = "72",
      "Weitere wirtschaftsbezogene Abbildung" = "79",
      "Sonstige" = "80",
      "Nicht bestimmbar" = "99"
    )
  ) %>%
  count(post_topic, visual_disinformation_topic, super_topic, name = "n") %>%
  group_by(post_topic) %>%
  mutate(
    total_type = sum(n),
    rel_type   = total_type / nrow(dataset) * 100,
    rel_freq   = n / total_type * rel_type
  ) %>%
  ungroup()
topic_coherence_plot <- ggplot(topic_coherence_plot_table, aes(
  x = post_topic,
  y = rel_freq,
  fill = super_topic
)) +
  geom_col() +
  coord_flip() +
  labs(
    x = "Thema des Beitrag",
    y = "Relative Häufigkeit",
    fill = "Thema des Artefakts",
    title = "Verhältnis zwischen Beitrags- und Artefaktthema",
    subtitle = "Relative Häufigkeit in %"
  ) +
  scale_fill_viridis_d(option = "B", direction = 1) + # use viridis color scale for color blindness optimization
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 11),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "bottom" # place legend under plot
  )

#############################################################

# Viewers
View(dataset)
View(visual_coherence_table)
print(visual_coherence_plot)

View(topic_coherence_plot_table)
print(topic_coherence_plot)


#############################################################

# Results
