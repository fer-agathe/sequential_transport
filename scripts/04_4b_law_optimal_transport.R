# Law School dataset: Multivariate Optimal Transport for counterfactual inference

# Note: the file `04_4a_law_optimal_transport.py` must be evaluated
system("python3 04_4a_law_optimal_transport.py")

# Required packages----
library(tidyverse)
library(fairadapt)

# Graphs
font_main = font_title = 'Times New Roman'
extrafont::loadfonts(quiet = T)
face_text='plain'
face_title='plain'
size_title = 14
size_text = 11
legend_size = 11

global_theme <- function() {
  theme_minimal() %+replace%
    theme(
      text = element_text(family = font_main, size = size_text, face = face_text),
      legend.text = element_text(family = font_main, size = legend_size),
      axis.text = element_text(size = size_text, face = face_text),
      plot.title = element_text(
        family = font_title,
        size = size_title,
        hjust = 0.5
      ),
      plot.subtitle = element_text(hjust = 0.5)
    )
}

# Seed
set.seed(2025)

source("../functions/utils.R")

# Load Data and Classifier----

# Dataset
load("../data/df_race.rda")
load("../data/df_race_c.rda")

# Predictions on train/test sets
load("../data/pred_aware.rda")
load("../data/pred_unaware.rda")
# Predictions on the factuals, on the whole dataset
load("../data/pred_aware_all.rda")
load("../data/pred_unaware_all.rda")

# Counterfactuals with Multivariate Optimal Transport----

# Counterfactuals from `04_4a_law_optimal_transport.py`
counterfactuals_ot <- read_csv('../data/counterfactuals_ot.csv')

# Add the sensitive attribute
S_star <- df_race_c |>
  mutate(
    S_star = case_when(
      S == "Black" ~ "White",
      S == "White" ~ "Black",
      TRUE ~ "Error"
    )
  ) |>
  pull("S_star")

counterfactuals_ot <- counterfactuals_ot |>
  mutate(S = S_star)

## Unaware Model----
model_unaware <- pred_unaware$model
pred_unaware_ot <- predict(
  model_unaware, newdata = counterfactuals_ot, type = "response"
)
counterfactuals_unaware_ot <- counterfactuals_ot |>
  mutate(pred = pred_unaware_ot, type = "counterfactual")

factuals_unaware <- tibble(
  S = df_race$S,
  X1 = df_race$X1,
  X2 = df_race$X2,
  pred = pred_unaware_all,
  type = "factual"
)

unaware_ot <- bind_rows(
  # predicted values on factuals
  factuals_unaware |> mutate(type = "factual"),
  # predicted values on counterfactuals obtained with OT
  counterfactuals_unaware_ot
)

unaware_ot_white <- unaware_ot |>  filter(S == "White")
unaware_ot_black <- unaware_ot |>  filter(S == "Black")

# Ref: Black Individuals
ggplot(
  data = unaware_ot_black,
  mapping = aes(x = pred, fill = type)
) +
  geom_histogram(
    mapping = aes(y = after_stat(density)),
    alpha = 0.5, position = "identity", binwidth = 0.05
  ) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Unaware model, Sensitive: Race, Reference: Black individual",
    x = "Predictions for Y",
    y = "Density"
  ) +
  global_theme()

# Ref: White Individuals
ggplot(
  data = unaware_ot_white,
  mapping = aes(x = pred, fill = type)) +
  geom_histogram(
    mapping = aes(y = after_stat(density)),
    alpha = 0.5, position = "identity", binwidth = 0.05
  ) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Unaware model, Sensitive: Race, Reference: White",
    x = "Predictions for Y",
    y = "Density") +
  global_theme()

## Aware Model----

model_aware <- pred_aware$model
pred_aware_ot <- predict(
  model_aware, newdata = counterfactuals_ot, type = "response"
)
counterfactuals_aware_ot <- counterfactuals_ot |>
  mutate(pred = pred_aware_ot, type = "counterfactual")
counterfactuals_aware_ot

factuals_aware <- tibble(
  S = df_race$S,
  X1 = df_race$X1,
  X2 = df_race$X2,
  pred = pred_aware_all,
  type = "factual"
)

aware_ot <- bind_rows(
  factuals_aware,
  counterfactuals_aware_ot
)

aware_ot_white <- aware_ot |> filter(S == "White")
aware_ot_black <- aware_ot |>  filter(S == "Black")

# Ref: Black Individuals
ggplot(
  data = aware_ot_black,
  mapping = aes(x = pred, fill = type)
) +
  geom_histogram(
    mapping = aes(y = ..density..),
    alpha = 0.5, position = "identity", binwidth = 0.05
  ) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Aware model, Sensitive: Race, Reference: Black individual",
    x = "Predictions for Y",
    y = "Density"
  ) +
  global_theme()

# Ref: White Individuals
ggplot(
  data = aware_ot_white,
  mapping = aes(x = pred, fill = type)) +
  geom_histogram(
    mapping = aes(y = ..density..),
    alpha = 0.5, position = "identity", binwidth = 0.05
  ) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Aware model, Sensitive: Race, Reference: White",
    x = "Predictions for Y",
    y = "Density") +
  global_theme()


# Comparison for Two Individuals----

# The first Black individual in the dataset is in row 24. The individual in
# row 25 is White.

## Unaware----
# Prediction on the factuals with the unaware model
(indiv_factuals_unaware <- factuals_unaware[24:25, ])

# Counterfactual with unaware model:
(indiv_counterfactuals_unaware_ot <- counterfactuals_unaware_ot[24:25, ])

indiv_unaware_ot <- bind_rows(
  indiv_factuals_unaware |> mutate(id = c(24, 25)),
  indiv_counterfactuals_unaware_ot |> mutate(id = c(24, 25))
)
indiv_unaware_ot

# Difference between counterfactual and factual for these two individuals,
# with the unaware model
indiv_unaware_ot |> select(id , type, pred) |>
  pivot_wider(names_from = type, values_from = pred) |>
  mutate(diff = counterfactual - factual)

## Aware----
indiv_aware_ot <- bind_rows(
  factuals_aware[c(24, 25),] |> mutate(id = c(24, 25)),
  counterfactuals_aware_ot[c(24, 25),] |> mutate(id = c(24, 25))
)
indiv_aware_ot

# Difference between counterfactual and factual for these two individuals,
# with the aware model
indiv_aware_ot |> select(id , type, pred) |>
  pivot_wider(names_from = type, values_from = pred) |>
  mutate(diff = counterfactual - factual)


# Counterfactual Demographic Parity----

# With the unaware model
dp_unaware_pt <- mean(
  counterfactuals_unaware_ot |> filter(S == "White") |> pull("pred") -
    factuals_unaware |> filter(S == "Black") |> pull("pred")
)
dp_unaware_pt

# With the aware model
dp_aware_ot <- mean(
  counterfactuals_aware_ot |> filter(S == "White") |> pull("pred") -
    factuals_aware |> filter(S == "Black") |> pull("pred")
)
dp_aware_ot

# Saving Objects----
save(counterfactuals_unaware_ot, file = "../data/counterfactuals_unaware_ot.rda")
save(counterfactuals_aware_ot, file = "../data/counterfactuals_aware_ot.rda")
