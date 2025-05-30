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

# posts per year, line graph
postsPerYear <- dataset %>%
  mutate(year = lubridate::year(date)) %>%
  filter(year >= 2020 & year <= 2024) %>%
  count(year)
postsPerYearPlot <- ggplot(
  postsPerYear,
  aes(x = year, y = n)
) +
  geom_line() +
  geom_point(color = "black", size = 2) +
  labs(
    x = "Jahr",
    y = "Anzahl Beiträge",
    title = "Beiträge pro Jahr",
    subtitle = "Anzahl Beiträge pro Jahr"
  ) +
  theme_minimal() +
  theme(
    # axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

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

# x^2 -> cannot be used as it only calculates x^2 dependent on date and not on events
# date_dependency_chisq <- chisq.test(balanced_matrix_sum,
#   simulate.p.value = TRUE,
#   B = 10000 # number of replicates
# )

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
  mutate(post_topic = factor(
    post_topic,
    levels = post_topic_lut, # this uses the value order
    labels = post_topic_lut # these are the displayed names
  )) %>%
  ggplot(aes(
    x = date, y = post_topic, color = post_topic, size = count * 3
  )) +
  geom_point(
    alpha = 0.5,
    show.legend = FALSE
  ) +
  scale_radius(range = c(2, 10)) +
  scale_color_viridis_d(option = "B", begin = 0, end = 0.9) +
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

# z-score to calculate overall topic overrepresentation
daily_topic <- date_dependency_txtable %>%
  pivot_longer(
    cols      = -date,
    names_to  = "topic",
    values_to = "count"
  )
# Compute per‑topic mean & sd, then z‑score each day
daily_z <- daily_topic %>%
  group_by(topic) %>%
  mutate(
    mu    = mean(count),
    sigma = sd(count),
    z     = (count - mu) / sigma
  ) %>%
  ungroup()
# summarized mean z‑score per topic
mean_z_by_topic <- daily_z %>%
  group_by(topic) %>%
  summarize(
    mean_z = mean(z, na.rm = TRUE),
    sd_z = sd(z, na.rm = TRUE),
    days_observed = n()
  ) %>%
  ungroup()
# spike days to assess specific events
spike_days <- daily_z %>%
  filter(z > 1) %>% # threshold for “spike”
  arrange(topic, desc(z)) %>%
  select(topic, date, count, z)



#############################################################

# Viewers
View(dataset)

print(postsPerYearPlot)

View(date_dependency_txtable)
# str(date_dependency_chisq)
print(date_dependency_plot)
# View(date_dependency_stdres)
print(mean_z_by_topic)
View(mean_z_by_topic)
View(spike_days)
# print(date_dependency_cv)

#############################################################

# Results
# List of 9
#  $ statistic:   2990
#  $ parameter:   NA -> no degrees of freedom for Monte Carlo simulated p-value
#  $ method   :  "Chi-squared test for given probabilities with simulated p-value\n\t (based on 10000 replicates)"
#  $ data.name:  "balanced_matrix_sum"
#  $ observed :  [1:1796] 1 0 0 0 0 0 0 0 0 0 ... -> observed values
#  $ expected :  [1:1796] 0.211 0.211 0.211 0.211 0.211 ... -> expected values

#  $ p.value  :  1e-04 -> p value << 0.05; reject H0 hypothesis
#  $ residuals:  [1:1796] 1.718 -0.459 -0.459 -0.459 -0.459 ... -> Raw deviations of observed from expected.
#  $ stdres   :  [1:1796] 1.72 -0.46 -0.46 -0.46 -0.46 ... -> Standardized residuals, showing which dates deviate most in 𝜎 units

#  topic              mean_z  sd_z days_observed
# 1 Gesundheit      -2.63e-16  1.00           274
# 2 Identität       -7.96e-17  1.00           274
# 3 Journalismus     1.60e-16  1.00           274
# 4 Nicht erkennbar  1.12e-16  1.00           274
# 5 Politik         -3.08e-17  1.00           274
# 6 Sonstige        -2.68e-16  1.00           274
# 7 Sport/Kultur    -1.17e-16  1.00           274
# 8 Wirtschaft       2.57e-16  1              274
# 9 Wissenschaft    -2.89e-16  1.00           274
