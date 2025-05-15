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

# hypothesis 1: amount of desinformation depends on external events
post_topic_lut <- c(
  "1" = "Politik",
  "2" = "Journalismus",
  "3" = "Wissenschaft",
  "4" = "Gesundheit",
  "5" = "Identität",
  "6" = "Sport/Kultur",
  "7" = "Wirtschaft",
  "8" = "Sonstige",
  "9" = "Nicht erkennbar"
)
date_dependency_xtabs <- xtabs(
  ~ date + post_topic,
  data = dataset
)
date_dependency_txtable <- as.data.frame(date_dependency_xtabs) %>%
  rename(
    date = date,
    post_topic = post_topic,
    count = Freq
  ) %>%
  mutate(
    #  factor→character→Date conversion
    date = as.Date(as.character(date)),
    post_topic = recode_factor(
      post_topic,
      !!!post_topic_lut,
      .default = "Nicht erkennbar"
    )
  ) %>%
  # filter by actual dates
  filter(date > as.Date("2020-01-01")) %>%
  pivot_wider(
    id_cols = date,
    names_from = post_topic,
    values_from = count,
    values_fn = list(count = sum), # sum duplicates into one cell
    values_fill = 0 # fill missing combos with zero
  )

# calculate overall Pearson x^2
date_dependency_matrix <- date_dependency_txtable %>%
  column_to_rownames("date") %>%
  as.matrix()
# summarize into one column of totals per date
date_totals <- date_dependency_matrix %>%
  # rowSums() gives total across all topic‐columns
  rowSums() %>%
  # turn into a tibble with a single column
  enframe(name = "date", value = "total_count") %>%
  # make date an actual Date (it was a character rowname)
  mutate(date = as.Date(date))
# balance the dates (fill any missing days with zeros)
balanced_dates <- date_totals %>%
  # define full daily sequence from min to max
  complete(
    date = seq(min(date), max(date), by = "day"),
    fill = list(total_count = 0)
  )
#   balanced_dates$date          — every calendar date
#   balanced_dates$total_count   — total posts that day (0 if none)
# turn back into a matrix for other analyses
balanced_matrix_sum <- balanced_dates %>%
  column_to_rownames("date") %>%
  as.matrix()

date_dependency_chisq <- chisq.test(balanced_matrix_sum)

# balanced matrix without sum
balanced_matrix <- date_dependency_txtable %>%
  # ensure 'date' is Date, not factor
  mutate(date = as.Date(as.character(date))) %>%
  # now join on matching types
  left_join(date_totals, by = "date") %>%
  # remove the total_count column to keep original data
  select(-total_count) %>%
  # restore date as rownames for matrix conversion
  column_to_rownames("date") %>%
  as.matrix()

date_dependency_plot <- balanced_matrix %>%
  as.data.frame() %>%
  rownames_to_column("date") %>%
  # convert factor ’date’ to Date first
  mutate(date = as.Date(as.character(date))) %>%
  # pivot to long, so you have (date, topic, count)
  pivot_longer(
    cols      = -date,
    names_to  = "post_topic",
    values_to = "count"
  ) %>%
  filter(count > 0) %>%
  filter(date > as.Date("2020-01-01")) %>%
  ggplot(aes(
    x = date, y = post_topic, color = post_topic, size = count * 3
  )) +
  geom_point(
    alpha = 0.5,
    show.legend = FALSE
  ) +
  scale_radius(range = c(2, 10)) +
  scale_fill_viridis_d() +
  labs(
    x     = "Zeitverlauf",
    y     = "Thema des Beitrags",
    color = "Topic",
    title = "Inhaltsverteilung der Beiträge über Zeit"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title  = element_text(hjust = 0.5)
  )

# stdresiduals
date_dependency_stdres <- tibble(
  date = rownames(balanced_matrix_sum),
  stdres = date_dependency_chisq$stdres
) %>%
  arrange(desc(abs(stdres)))
#############################################################

# Viewers
View(dataset)
View(date_dependency_txtable)
str(date_dependency_chisq)
print(date_dependency_plot)
View(date_dependency_stdres)

#############################################################

# Results
# List of 9
#  $ statistic:   2990
#  $ parameter:   1795
#  $ method   :  "Chi-squared test for given probabilities"
#  $ data.name:  "balanced_matrix_sum"
#  $ observed :  [1:1796] 1 0 0 0 0 0 0 0 0 0 ... -> observed values
#  $ expected :  [1:1796] 0.211 0.211 0.211 0.211 0.211 ... -> expected values

#  $ p.value  :  4.74e-63 -> p value << 0.05; reject H0 hypothesis
#  $ residuals:  [1:1796] 1.718 -0.459 -0.459 -0.459 -0.459 ... -> standardized deviation for each cell
#  $ stdres   :  [1:1796] 1.72 -0.46 -0.46 -0.46 -0.46 ...