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
library(irr) # for Cohen's Kappa
library(lpSolve) # for linear programming

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

# hypothesis 2: disinformation is emotionally connotated
# sentiment frequency table + plot
post_sentiment_table <- dataset %>%
  count(post_sentiment) %>%
  mutate(
    sentiment = fct_recode(
      as.factor(post_sentiment),
      "Freude/Humor"                    = "1",
      "Trauer"                          = "2",
      "Wut/Ärger"                       = "3",
      "Anwiderung"                      = "4",
      "Angst"                           = "5",
      "Überraschung/Erstaunen"          = "6",
      "Nicht erkennbar/entscheidbar"    = "9"
    ),
    relative_freq = n / nrow(dataset) * 100,
    source = "Beitrag"
  ) %>%
  select(source, sentiment, relative_freq)

visual_sentiment_table <- dataset %>%
  count(visual_sentiment) %>%
  mutate(
    sentiment = fct_recode(
      as.factor(visual_sentiment),
      "Freude/Humor"                    = "1",
      "Trauer"                          = "2",
      "Wut/Ärger"                       = "3",
      "Anwiderung"                      = "4",
      "Angst"                           = "5",
      "Überraschung/Erstaunen"          = "6",
      "Nicht erkennbar/entscheidbar"    = "9"
    ),
    relative_freq = n / nrow(dataset) * 100,
    source = "Visuelles Artefakt"
  ) %>%
  select(source, sentiment, relative_freq)
# combine
sentiment_combined <- bind_rows(post_sentiment_table, visual_sentiment_table)
sentiment_plot <- ggplot(
  sentiment_combined,
  aes(x = sentiment, y = relative_freq, fill = source)
) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6, color = "black") +
  scale_fill_grey(start = 0.3, end = 0.7, name = "Quelle") +
  labs(
    x        = "Emotionalität",
    y        = "Relative Häufigkeit (%)",
    title    = "Vergleich Post- vs. Artefakt-Emotionalität",
    subtitle = "Relative Häufigkeit in %"
  ) +
  theme_minimal() +
  theme(
    axis.text.x   = element_text(angle = 45, hjust = 1),
    plot.title    = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

# sentiment contingency
sentiment_lut <- c(
  "1" = "Freude/Humor",
  "2" = "Trauer",
  "3" = "Wut/Ärger",
  "4" = "Anwiderung",
  "5" = "Angst",
  "6" = "Überraschung/Erstaunen",
  "7" = "Nicht erkennbar/entscheidbar"
)
sentiment_coherence_xtabs <- xtabs(
  ~ post_sentiment + visual_sentiment,
  data = dataset
)
sentiment_coherence_txtable <- as.data.frame(sentiment_coherence_xtabs) %>%
  rename(
    post_sentiment = post_sentiment,
    visual_sentiment = visual_sentiment,
    count = Freq
  ) %>%
  mutate(
    post_sentiment = recode_factor(
      post_sentiment,
      !!!sentiment_lut,
      .default = "Nicht erkennbar/entscheidbar"
    ),
    visual_sentiment = recode_factor(
      visual_sentiment,
      !!!sentiment_lut,
      .default = "Nicht erkennbar/entscheidbar"
    )
  ) %>%
  pivot_wider(
    id_cols = post_sentiment,
    names_from = visual_sentiment,
    values_from = count,
    values_fn = list(count = sum), # sum duplicates into one cell
    values_fill = 0 # fill missing combos with zero
  )

# calculate overall x^2
sentiment_coherence_matrix <- sentiment_coherence_txtable %>%
  column_to_rownames("post_sentiment") %>%
  as.matrix()
sentiment_coherence_chisq <- chisq.test(sentiment_coherence_matrix,
  simulate.p.value = TRUE,
  B = 10000 # number of replicates
)

# calculate overall cohen's kappa
sentiment_coherence_ckappa_obs <- sentiment_txtable %>%
  pivot_longer(
    cols      = -post_sentiment,
    names_to  = "visual_sentiment",
    values_to = "freq"
  ) %>%
  filter(freq > 0) %>% # drop zero‐frequency cells
  uncount(weights = freq) %>% # replicate rows by count
  select(post_sentiment, visual_sentiment)
sentiment_coherence_ckappa <- kappa2(
  sentiment_coherence_ckappa_obs,
  weight = "unweighted"
)

# sentiment coherence plot
sentiment_coherence_plot <- sentiment_coherence_txtable %>%
  pivot_longer(
    cols      = -post_sentiment,
    names_to  = "visual_sentiment",
    values_to = "count"
  ) %>%
  filter(count > 0) %>%
  # lock in the same category order for both dimensions
  mutate(
    post_sentiment   = factor(post_sentiment, levels = unname(sentiment_lut)),
    visual_sentiment = factor(visual_sentiment, levels = unname(sentiment_lut))
  ) %>%
  ggplot(aes(
    x = post_sentiment,
    y = visual_sentiment,
    color = count,
    fill = count
  )) +
  geom_tile() +
  labs(
    x     = "Post-Emotionalität",
    y     = "Artefakt-Emotionalität",
    color = "Anzahl Beiträge",
    fill  = "Anzahl Beiträge",
    title = "Co-Vorkommen von Post- und Artefakt-Emotionalität"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title  = element_text(hjust = 0.5)
  )

#############################################################

# Viewers
View(dataset)
View(post_sentiment_table)
View(visual_sentiment_table)
print(sentiment_plot)

View(sentiment_coherence_txtable)
print(sentiment_coherence_chisq)
print(sentiment_coherence_plot)

print(sentiment_coherence_ckappa)

#############################################################

# Results

# Pearson's Chi-squared test
#   data:  sentiment_coherence_matrix
#   X-squared = 178.03, df = NA, p-value = 9.999e-05 -> observed co‑occurrence pattern is extremely unlikely under independence—strong evidence that post_sentiment and visual_sentiment are associated.

# Cohen's Kappa for 2 Raters (Weights: unweighted)
#   Subjects = 381
#   Raters = 2
#   Kappa = 0.0936 -> slight agreement
#   z = 3.59
#   p-value = 0.000331
# -> Although post and visual sentiments co‑occur more often than random, the strength of that correspondence is minimal.
