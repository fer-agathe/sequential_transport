# Law School dataset: Fairadapt for counterfactual inference

# Required packages
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


# Data and Classifier----

# Dataset
load("../data/df_race.rda")
load("../data/df_race_c.rda")

# Predictions on train/test sets
load("../data/pred_aware.rda")
load("../data/pred_unaware.rda")
# Predictions on the factuals, on the whole dataset
load("../data/pred_aware_all.rda")
load("../data/pred_unaware_all.rda")

# Adjacency matrix
load("../data/adj.rda")

# Counterfactuals with fairadapt----

# Remove Y from adjacency matrix
adj_wo_Y <- adj[-4,-4]
adj_wo_Y

df_race_fpt <- df_race_c |> select(S, X1, X2)
levels(df_race_fpt$S)


# White (factuals) --> Black (counterfactuals)
fpt_model_white <- fairadapt(
  X2 ~ .,
  train.data = df_race_fpt,
  prot.attr = "S", adj.mat = adj_wo_Y,
  quant.method = linearQuants
)
adapt_df_white <- adaptedData(fpt_model_white)

# Black (factuals) --> White (counterfactuals)
df_race_fpt$S <- factor(df_race_fpt$S, levels = c("White", "Black"))
fpt_model_black <- fairadapt(
  X2 ~ .,
  train.data = df_race_fpt,
  prot.attr = "S", adj.mat = adj_wo_Y,
  quant.method = linearQuants
)
adapt_df_black <- adaptedData(fpt_model_black)

## Unaware Model----
factuals_unaware <- tibble(
  S = df_race$S,
  X1 = df_race$X1,
  X2 = df_race$X2,
  pred = pred_unaware_all,
  type = "factual"
)

write.csv(
  factuals_unaware,
  file = "../data/factuals_unaware.csv", row.names = FALSE
)

# Counterfactual characteristics
ind_white <- which(df_race_fpt$S == "White")
ind_black <- which(df_race_fpt$S == "Black")
df_counterfactuals_fpt <- factuals_unaware |> select(-pred, -type)
df_counterfactuals_fpt[ind_white, ] <-
  adapt_df_white[ind_white, ] |> select(S, X1, X2)
df_counterfactuals_fpt[ind_black, ] <-
  adapt_df_black[ind_black,] |> select(S, X1, X2)
# Predictions  using these characteristics
model_unaware <- pred_unaware$model
pred_unaware_fpt <- predict(
  model_unaware, newdata = df_counterfactuals_fpt, type = "response"
)

# Wrap this in a tibble
counterfactuals_unaware_fpt <- tibble(
  S = df_counterfactuals_fpt$S,
  X1 = df_counterfactuals_fpt$X1,
  X2 = df_counterfactuals_fpt$X2,
  pred = pred_unaware_fpt,
  type = "counterfactual"
)
# dataset with counterfactuals, for unaware model
unaware_fpt <- bind_rows(factuals_unaware, counterfactuals_unaware_fpt)

unaware_fpt_white <- unaware_fpt |> filter(S == "White")
unaware_fpt_black <- unaware_fpt |> filter(S == "Black")

# Looking at the distribution of predicted values by the model
ggplot(unaware_fpt_black, aes(x = pred, fill = type)) +
  geom_histogram(
    mapping = aes(
      y = after_stat(density)), alpha = 0.5,
    position = "identity", binwidth = 0.05
  ) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Unaware model, Sensitive: Race, Reference: Black individuals",
    x = "Predictions for Y",
    y = "Density"
  ) +
  global_theme()


ggplot(
  data = unaware_fpt_white,
  mapping = aes(x = pred, fill = type)
) +
  geom_histogram(
    mapping = aes(y = after_stat(density)),
    alpha = 0.5, position = "identity", binwidth = 0.05
  ) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Unaware model, Sensitive: Race, Reference: White individuals",
    x = "Predictions for Y",
    y = "Density"
  ) +
  global_theme()

## Aware Model----
factuals_aware <- tibble(
  S = df_race$S,
  X1 = df_race$X1,
  X2 = df_race$X2,
  pred = pred_aware_all,
  type = "factual"
)

write.csv(factuals_aware, file = "../data/factuals_aware.csv", row.names = FALSE)

# Predictions
model_aware <- pred_aware$model
pred_aware_fpt <- predict(
  model_aware, newdata = df_counterfactuals_fpt, type = "response"
)

# In a tibble:
counterfactuals_aware_fpt <- tibble(
  S = df_counterfactuals_fpt$S,
  X1 = df_counterfactuals_fpt$X1,
  X2 = df_counterfactuals_fpt$X2,
  pred = pred_aware_fpt,
  type = "counterfactual"
)

aware_fpt <- bind_rows(factuals_aware, counterfactuals_aware_fpt)

aware_fpt_white <- aware_fpt |> filter(S == "White")
aware_fpt_black <- aware_fpt |>  filter(S == "Black")

# Densities of predicted values

# Sensitive: Race, Reference: Black individuals
ggplot(
  data = aware_fpt_black,
  mapping = aes(x = pred, fill = type)) +
  geom_histogram(
    mapping = aes(y = after_stat(density)),
    alpha = 0.5, position = "identity", binwidth = 0.05
  ) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Aware model, Sensitive: Race, Reference: Black individuals",
    x = "Predictions for Y",
    y = "Density"
  ) +
  global_theme()

# Sensitive: Race, Reference: White individuals
ggplot(
  data = aware_fpt_white,
  mapping = aes(x = pred, fill = type)) +
  geom_histogram(
    mapping = aes(y = after_stat(density)),
    alpha = 0.5, position = "identity", binwidth = 0.05) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Aware model, Sensitive: Race, Reference: White individuals",
    x = "Predictions for Y",
    y = "Density"
  ) +
  global_theme()

# Comparison for Two Individuals----
# The first Black individual in the dataset is in row 24. The individual in
# row 25 is White.
(indiv_factuals_unaware <- factuals_unaware[24:25, ])

# The counterfactual values for these individuals
(indiv_counterfactuals_unaware_fpt <- counterfactuals_unaware_fpt[24:25, ])

# Aware model
indiv_unaware_fpt <- bind_rows(
  indiv_factuals_unaware |> mutate(id = c(24, 25)),
  indiv_counterfactuals_unaware_fpt |> mutate(id = c(24, 25))
)
indiv_unaware_fpt


# Difference between counterfactual and factual for these two individuals,
# with the unaware model
indiv_unaware_fpt |> select(id , type, pred) |>
  pivot_wider(names_from = type, values_from = pred) |>
  mutate(diff_fpt = counterfactual - factual)

# Unaware model
indiv_aware_fpt <- bind_rows(
  factuals_aware[c(24, 25),] |> mutate(id = c(24, 25)),
  counterfactuals_aware_fpt[c(24, 25),] |> mutate(id = c(24, 25))
)
indiv_aware_fpt

# Difference between counterfactual and factual for these two individuals,
# with the aware model
indiv_aware_fpt |> select(id , type, pred) |>
  pivot_wider(names_from = type, values_from = pred) |>
  mutate(diff = counterfactual - factual)


# Counterfactual Demographic Parity----

dp_unaware_fpt <- mean(
  counterfactuals_unaware_fpt |> filter(S == "White") |> pull("pred") -
    factuals_unaware |> filter(S == "Black") |> pull("pred")
)
dp_unaware_fpt

# Saving Objects----
save(factuals_unaware, file = "../data/factuals_unaware.rda")
save(factuals_aware, file = "../data/factuals_aware.rda")
save(
  counterfactuals_unaware_fpt,
  file = "../data/counterfactuals_unaware_fpt.rda"
)
save(counterfactuals_aware_fpt, file = "../data/counterfactuals_aware_fpt.rda")
