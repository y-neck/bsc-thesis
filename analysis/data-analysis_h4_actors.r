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

# hypothesis 4: Disinformation is mostly shared by single actors
# frequency table
actors_table <- dataset %>%
  count(source_actor) %>%
  mutate(
    actor_type = fct_recode(
      as.factor(source_actor),
      "Privatperson/Individuum" = "1",
      "Kollektivakteur/Organisation" = "2",
      "Nicht erkennbar" = "9"
    ),
    relative_freq = n / nrow(dataset) * 100,
    source = "Akteurstyp"
  ) %>%
  select(source, actor_type, relative_freq)

actor_role_table <- dataset %>%
  count(source_role) %>%
  mutate(
    actor_role = fct_recode(
      as.factor(source_role),
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
    relative_freq = n / nrow(dataset) * 100,
    source = "Akteurrolle"
  ) %>%
  select(source, actor_role, relative_freq)
# combine
actor_combined <- bind_rows(actors_table, actor_role_table)

actor_plot_table <- dataset %>%
  mutate(
    actor_type = fct_recode(
      as.factor(source_actor),
      "Privatperson/Individuum" = "1",
      "Kollektivakteur/Organisation" = "2",
      "Nicht erkennbar" = "9"
    ),
    actor_role = fct_recode(
      as.factor(source_role),
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
  ) %>%
  count(actor_type, actor_role, name = "n") %>%
  group_by(actor_type) %>%
  mutate(
    total_type    = sum(n),
    rel_type      = total_type / nrow(dataset) * 100,
    rel_segment   = n / total_type * rel_type # segment’s share of overall %
  ) %>%
  ungroup()

actor_plot <- ggplot(actor_plot_table, aes(
  x    = actor_type,
  y    = rel_segment,
  fill = actor_role
)) +
  geom_col() +
  coord_flip() +
  labs(
    x = "Akteurstyp",
    y = "Relative Häufigkeit (%)",
    fill = "Akteurrolle",
    title = "Verteilung der Akteursrollen innerhalb der Akteurstypen",
    subtitle = "Relative Häufigkeit in %"
  ) +
  scale_fill_viridis_d(option = "B", direction = 1) + # use viridis color scale for color blindness optimization
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 11),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

# contingengy table
actor_txtable <- xtabs(
  n ~ source_actor + source_role,
  data = dataset %>%
    count(source_actor, source_role, name = "n")
)

# x^2
actor_txtable_chisq <- chisq.test(actor_txtable, simulate.p.value = TRUE, B = 10000)

#############################################################

# Viewers
View(dataset)
View(actors_table)
View(actor_plot_table)

print(actor_plot)

View(actor_txtable)
print(actor_txtable_chisq)
print(actor_txtable_stdres)

#############################################################

# Results
# Pearson's Chi-squared test with simulated p-value (based on 10000 replicates)
#     data:  actor_txtable
#     X-squared = 44.602, df = NA, p-value = 0.007699
