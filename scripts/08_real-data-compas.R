# Real Data Example: COMPAS dataset

# Source of the data:
# Larson, Surya, Jeff ans Mattu, Lauren Kirchner, and Julia Angwin. 2016.
# "How We Analyzed the COMPAS Recidivism Algorithm." Edited by ProPublica.
# https://www.propublica.org/article/how-we-analyzed-the-compas-recidivism-algorithm.
# https://github.com/propublica/compas-analysis
#
# We use a formatted version from:
# De Lara, Lucas, Alberto González-Sanz, Nicholas Asher, Laurent Risser,
# and Jean-Michel Loubes. 2024. “Transport-Based Counterfactual Models.”
# Journal of Machine Learning Research 25 (136): 1–59.
#
# Github URL: https://github.com/dplecko/fairadapt

# The COMPAS (Correctional Offender Management Profiling for Alternative
# Sanctions) dataset contains information used to predict whether criminal
# defendants are likely of recidivism (Y). The data contains real observations
# from Broward County, Florida. Each row gives information on individuals
# released on parole and whether they reoffended within two years (Y). Other
# characteristics such as the sex of the individual, the number of juvenile
# felonies, the number of juvenile misdemeanors, the number of other juvenile
# offenses, the number of prior offenses and the degree of charge (with two
# values F for felony, and M for misdemeanor). We will use the race of
# individuals as the sensitive attribute (S).

# Required packages----
library(tidyverse)
library(devtools)
# Our package
load_all("../seqtransfairness/")

# Graphs----
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

colours_all <- c(
  "factual" = "black",
  "source" = "#00A08A",
  "reference" = "#F2AD00",
  "naive" = "gray",
  "ot" = "#0072B2",
  "fairadapt" = '#D55E00',
  "seq" = "#CC79A7"
)

# Data----
vars <- c(
  "age", "sex", "juv_fel_count",
  "juv_misd_count", "juv_other_count", "priors_count",
  "c_charge_degree", "race", "two_year_recid"
)
s <- "race"
y <- "two_year_recid"

library(fairadapt)
# reading in the COMPAS data
data("compas", package = "fairadapt")
compas <-
  compas |>
  as_tibble() |>
  select(!!vars) |>
  mutate(race = fct_relevel(race, "Non-White", "White"))

compas |> count(two_year_recid, race) |>
  group_by(two_year_recid) |>
  mutate(pct_race = round(100* n / sum(n), 2))

# Causal Graph----

# As in Plečko, Drago, and Nicolai Meinshausen. 2020.
# 'Fair Data Adaptation with Quantile Preservation.' Journal of Machine
# Learning Research 21 (242): 1–44.
# URL: https://github.com/dplecko/fairadapt/blob/main/jmlr-paper/real-data/Figures-6-7-8-9.R

# Adjacency matrix
adj_mat <- matrix(
  0,
  ncol = ncol(compas), nrow = ncol(compas),
  dimnames = list(vars, vars)
)

# adding the edges to the matrix
adj_mat[
  c("race", "sex", "age"),
  c("juv_fel_count", "juv_misd_count", "juv_other_count", "priors_count",
    "c_charge_degree", "two_year_recid")
] <- 1
adj_mat[c("juv_fel_count", "juv_misd_count", "juv_other_count"),
        c("priors_count", "c_charge_degree", "two_year_recid")] <- 1
adj_mat["priors_count", c("c_charge_degree", "two_year_recid")] <- 1
adj_mat["c_charge_degree", "two_year_recid"] <- 1

adj_mat

causal_graph <- fairadapt::graphModel(adj_mat)
plot(causal_graph)

# Classifier----

seed <- 2025
sets <- split_dataset(compas, seed, train_ratio = 0.7)
data_train <- sets$data_train
data_test <- sets$data_test

# Two types of models:
# 1. aware (with the sensitive variable)
# 2. unaware (without the sensitive variable)

# Unaware logistic regression classifier (model without S)
pred_unaware <- log_reg_train(
  data_train, data_test, s = s, y = y, type = "unaware"
)
pred_unaware_train <- pred_unaware$pred_train
pred_unaware_test <- pred_unaware$pred_test

# Aware logistic regression classifier (model with S)
pred_aware <- log_reg_train(
  data_train, data_test, s = s, y = y, type = "aware"
)
pred_aware_train <- pred_aware$pred_train
pred_aware_test <- pred_aware$pred_test


df_test_unaware <- tibble(
  S = data_test |> pull(!!s),
  pred = pred_unaware_test
)

df_test_aware <- tibble(
  S = data_test |> pull(!!s),
  pred = pred_aware_test
)

## Predictions----

model_unaware <- pred_unaware$model
pred_unaware_all <- predict(
  model_unaware,
  newdata = compas,
  type = "response"
)

model_aware <- pred_aware$model
pred_aware_all <- predict(
  model_aware,
  newdata = compas,
  type = "response"
)

# Factuals----

factuals_unaware <-
  compas |>
  as_tibble() |>
  mutate(
    race_origin = race,
    pred = pred_unaware_all,
    type = "factual"
  ) |>
  mutate(id_indiv = row_number())

factuals_aware <-
  compas |>
  as_tibble() |>
  mutate(
    race_origin = race,
    pred = pred_aware_all,
    type = "factual"
  ) |>
  mutate(id_indiv = row_number())

# Counterfactuals----

## Naive approach: ceteris paribus----

# Source group: Non-White

pred_unaware_naive_nonwhite <- predict(
  model_unaware,
  newdata = compas |> filter(race == "Non-White") |> mutate(race = "White"),
  type = "response"
)
pred_aware_naive_nonwhite <- predict(
  model_aware,
  newdata = compas |> filter(race == "Non-White") |>  mutate(race = "White"),
  type = "response"
)

ind_nonwhite <- which(compas$race == "Non-White")
ind_white <- which(compas$race == "White")

counterfactuals_unaware_naive_nonwhite <-
  compas |> filter(race == "Non-White") |>
  mutate(
    race_origin = race,
    race = "White",
    pred = pred_unaware_naive_nonwhite,
    type = "counterfactual",
    id_indiv = ind_nonwhite
  )
counterfactuals_aware_naive_nonwhite <-
  compas |> filter(race == "Non-White") |>
  mutate(
    race_origin = race,
    race = "White",
    pred = pred_aware_naive_nonwhite,
    type = "counterfactual",
    id_indiv = ind_nonwhite
  )

### Unaware Model----

unaware_naive_nonwhite <-
  factuals_unaware |> mutate(race_origin = race) |>
  bind_rows(counterfactuals_unaware_naive_nonwhite)

# Unaware model, Sensitive: Race, Non-White -> White
ggplot(
  unaware_naive_nonwhite |> mutate(
    group = case_when(
      race_origin =="Non-White" & race == "Non-White" ~ "Non-White (Original)",
      race_origin =="Non-White" & race == "White" ~ "Non-White -> White (Counterfactual)",
      race_origin =="White" & race == "White" ~ "White (Original)"
    ),
    group = factor(
      group,
      levels = c(
        "Non-White (Original)", "Non-White -> White (Counterfactual)", "White (Original)"
      )
    )
  ),
  aes(x = pred, fill = group, colour = group)
) +
  geom_histogram(
    mapping = aes(
      y = after_stat(density)), alpha = 0.5, colour = NA,
    position = "identity", binwidth = 0.05
  ) +
  geom_density(alpha = 0.3, linewidth = 1) +
  facet_wrap(~race) +
  scale_fill_manual(
    NULL, values = c(
      "Non-White (Original)" = colours_all[["source"]],
      "Non-White -> White (Counterfactual)" = colours_all[["naive"]],
      "White (Original)" = colours_all[["reference"]]
    )
  ) +
  scale_colour_manual(
    NULL, values = c(
      "Non-White (Original)" = colours_all[["source"]],
      "Non-White -> White (Counterfactual)" = colours_all[["naive"]],
      "White (Original)" = colours_all[["reference"]]
    )
  ) +
  labs(
    x = "Predictions for Y",
    y = "Density"
  ) +
  global_theme() +
  theme(legend.position = "bottom")


# Distribution of Predicted Scores for Minority Class (Non-White),
# Unaware model, Sensitive: Race, Non-White -> White
ggplot(
  data = unaware_naive_nonwhite |>
    mutate(
      group = case_when(
        race_origin =="Non-White" & race == "Non-White" ~ "Non-White (Original)",
        race_origin =="Non-White" & race == "White" ~ "Non-White -> White (Counterfactual)",
        race_origin =="White" & race == "White" ~ "White (Original)"
      ),
      group = factor(
        group,
        levels = c(
          "Non-White (Original)", "Non-White -> White (Counterfactual)", "White (Original)"
        )
      )
    ) |>
    filter(race_origin =="Non-White"),
  mapping = aes(x = pred, fill = group, colour = group)
) +
  geom_histogram(
    mapping = aes(y = after_stat(density)),
    alpha = 0.5, position = "identity", binwidth = 0.05
  ) +
  geom_density(alpha = 0.5, linewidth = 1) +
  scale_fill_manual(
    NULL, values = c(
      "Non-White (Original)" = colours_all[["source"]],
      "Non-White -> White (Counterfactual)" = colours_all[["naive"]],
      "White (Original)" = colours_all[["reference"]]
    )
  ) +
  scale_colour_manual(
    NULL, values = c(
      "Non-White (Original)" = colours_all[["source"]],
      "Non-White -> White (Counterfactual)" = colours_all[["naive"]],
      "White (Original)" = colours_all[["reference"]]
    )
  ) +
  labs(
    x = "Predictions for Y",
    y = "Density"
  ) +
  global_theme() +
  theme(legend.position = "bottom")

### Aware Model----

aware_naive_nonwhite <-
  factuals_aware |> mutate(race_origin = race) |>
  bind_rows(counterfactuals_aware_naive_nonwhite)

# Aware model, Sensitive: Race, Non-White -> White
ggplot(
  aware_naive_nonwhite |> mutate(
    group = case_when(
      race_origin =="Non-White" & race == "Non-White" ~ "Non-White (Original)",
      race_origin =="Non-White" & race == "White" ~ "Non-White -> White (Counterfactual)",
      race_origin =="White" & race == "White" ~ "White (Original)"
    ),
    group = factor(
      group,
      levels = c(
        "Non-White (Original)", "Non-White -> White (Counterfactual)", "White (Original)"
      )
    )
  ),
  aes(x = pred, fill = group, colour = group)
) +
  geom_histogram(
    mapping = aes(
      y = after_stat(density)), alpha = 0.5, colour = NA,
    position = "identity", binwidth = 0.05
  ) +
  geom_density(alpha = 0.3, linewidth = 1) +
  facet_wrap(~race) +
  scale_fill_manual(
    NULL, values = c(
      "Non-White (Original)" = colours_all[["source"]],
      "Non-White -> White (Counterfactual)" = colours_all[["naive"]],
      "White (Original)" = colours_all[["reference"]]
    )
  ) +
  scale_colour_manual(
    NULL, values = c(
      "Non-White (Original)" = colours_all[["source"]],
      "Non-White -> White (Counterfactual)" = colours_all[["naive"]],
      "White (Original)" = colours_all[["reference"]]
    )
  ) +
  labs(
    x = "Predictions for Y",
    y = "Density"
  ) +
  global_theme() +
  theme(legend.position = "bottom")

# Distribution of Predicted Scores for Minority Class (Non-White), Aware model,
# Sensitive: Race, Non-White -> White
ggplot(
  data = aware_naive_nonwhite |>
    mutate(
      group = case_when(
        race_origin =="Non-White" & race == "Non-White" ~ "Non-White (Original)",
        race_origin =="Non-White" & race == "White" ~ "Non-White -> White (Counterfactual)",
        race_origin =="White" & race == "White" ~ "White (Original)"
      ),
      group = factor(
        group,
        levels = c(
          "Non-White (Original)", "Non-White -> White (Counterfactual)", "White (Original)"
        )
      )
    ) |>
    filter(race_origin =="Non-White"),
  mapping = aes(x = pred, fill = group, colour = group)
) +
  geom_histogram(
    mapping = aes(y = after_stat(density)),
    alpha = 0.5, position = "identity", binwidth = 0.05
  ) +
  geom_density(alpha = 0.5, linewidth = 1) +
  scale_fill_manual(
    NULL, values = c(
      "Non-White (Original)" = colours_all[["source"]],
      "Non-White -> White (Counterfactual)" = colours_all[["naive"]],
      "White (Original)" = colours_all[["reference"]]
    )
  ) +
  scale_colour_manual(
    NULL, values = c(
      "Non-White (Original)" = colours_all[["source"]],
      "Non-White -> White (Counterfactual)" = colours_all[["naive"]],
      "White (Original)" = colours_all[["reference"]]
    )
  ) +
  labs(
    x = "Predictions for Y",
    y = "Density"
  ) +
  global_theme() +
  theme(legend.position = "bottom")

## Multivariate Optimal Transport----

# Export factuals
write.csv(
  factuals_aware, file = "../data/factuals_aware-compas.csv",
  row.names = FALSE
)

# Estimation with python
library(reticulate)
use_virtualenv("~/quarto-python-env", required = TRUE)
# reticulate::install_miniconda(force = TRUE)
# py_install("POT")
# py_install("scikit-learn")
# py_install("pandas")
# py_install("numpy")
# py_install("matplotlib")
# py_install("scipy")
py_run_file("08_read-data-compas-ot.py")

# Load results in R
df_counterfactuals_ot_nonwhite <- read_csv('../data/counterfactuals-ot-nonwhite-compas.csv') |>
  mutate(
    id_indiv = ind_nonwhite,
    race_origin = "Non-White",
    race = "White"
  ) |>
  mutate(
    c_charge_degree = str_remove(c_charge_degree, "^charge_degree_")
  )

### Predictions----
pred_ot_unaware <- predict(
  model_unaware, newdata = df_counterfactuals_ot_nonwhite, type = "response"
)

pred_ot_aware <- predict(
  model_aware, newdata = df_counterfactuals_ot_nonwhite, type = "response"
)

counterfactuals_unaware_ot_nonwhite <-
  df_counterfactuals_ot_nonwhite |>
  mutate(pred = pred_ot_unaware, type = "counterfactual")
counterfactuals_aware_ot_nonwhite <-
  df_counterfactuals_ot_nonwhite |>
  mutate(pred = pred_ot_aware, type = "counterfactual")

aware_ot_nonwhite <- bind_rows(
  factuals_aware |> mutate(id_indiv = row_number(), race_origin = race),
  counterfactuals_aware_ot_nonwhite |> mutate(S_origin = "Non-White")
)
unaware_ot_nonwhite <- bind_rows(
  factuals_unaware |> mutate(id_indiv = row_number(), race_origin = race),
  counterfactuals_unaware_ot_nonwhite |> mutate(S_origin = "Non-White")
)

# Unaware model, Sensitive: Race, Non-White -> White
ggplot(
  data = unaware_ot_nonwhite |>
    mutate(
      group = case_when(
        race_origin == "Non-White" & race == "Non-White" ~ "Women (Original)",
        race_origin == "Non-White" & race == "White" ~ "Women -> Men (Counterfactual)",
        race_origin == "White" & race == "White" ~ "Men (Original)"
      ),
      group = factor(
        group,
        levels = c(
          "Women (Original)", "Women -> Men (Counterfactual)", "Men (Original)"
        )
      )
    ),
  aes(x = pred, fill = group, colour = group)
) +
  geom_histogram(
    mapping = aes(
      y = after_stat(density)), alpha = 0.5, colour = NA,
    position = "identity", binwidth = 0.05
  ) +
  geom_density(alpha = 0.3, linewidth = 1) +
  facet_wrap(~race) +
  scale_fill_manual(
    NULL, values = c(
      "Women (Original)" = colours_all[["source"]],
      "Women -> Men (Counterfactual)" = colours_all[["ot"]],
      "Men (Original)" = colours_all[["reference"]]
    )
  ) +
  scale_colour_manual(
    NULL, values = c(
      "Women (Original)" = colours_all[["source"]],
      "Women -> Men (Counterfactual)" = colours_all[["ot"]],
      "Men (Original)" = colours_all[["reference"]]
    )
  ) +
  labs(
    x = "Predictions for Y",
    y = "Density"
  ) +
  global_theme() +
  theme(legend.position = "bottom")

# Aware model, Sensitive: Race, Non-White -> White
ggplot(
  data = aware_ot_nonwhite |>
    mutate(
      group = case_when(
        race_origin == "Non-White" & race == "Non-White" ~ "Women (Original)",
        race_origin == "Non-White" & race == "White" ~ "Women -> Men (Counterfactual)",
        race_origin == "White" & race == "White" ~ "Men (Original)"
      ),
      group = factor(
        group,
        levels = c(
          "Women (Original)", "Women -> Men (Counterfactual)", "Men (Original)"
        )
      )
    ),
  aes(x = pred, fill = group, colour = group)
) +
  geom_histogram(
    mapping = aes(
      y = after_stat(density)), alpha = 0.5, colour = NA,
    position = "identity", binwidth = 0.05
  ) +
  geom_density(alpha = 0.3, linewidth = 1) +
  facet_wrap(~race) +
  scale_fill_manual(
    NULL, values = c(
      "Women (Original)" = colours_all[["source"]],
      "Women -> Men (Counterfactual)" = colours_all[["ot"]],
      "Men (Original)" = colours_all[["reference"]]
    )
  ) +
  scale_colour_manual(
    NULL, values = c(
      "Women (Original)" = colours_all[["source"]],
      "Women -> Men (Counterfactual)" = colours_all[["ot"]],
      "Men (Original)" = colours_all[["reference"]]
    )
  ) +
  labs(
    x = "Predictions for Y",
    y = "Density"
  ) +
  global_theme() +
  theme(legend.position = "bottom")

# Distribution of Predicted Scores for Minority Class (Women), Unaware model,
# Sensitive: Race, Non-White -> White
ggplot(
  data = unaware_ot_nonwhite |>
    mutate(
      group = case_when(
        race_origin == "Non-White" & race == "Non-White" ~ "Women (Original)",
        race_origin == "Non-White" & race == "White" ~ "Women -> Men (Counterfactual)",
        race_origin == "White" & race == "White" ~ "Men (Original)"
      ),
      group = factor(
        group,
        levels = c(
          "Women (Original)", "Women -> Men (Counterfactual)", "Men (Original)"
        )
      )
    ) |>
    filter(race_origin == "Non-White"),
  mapping = aes(x = pred, fill = group, colour = group)
) +
  geom_histogram(
    mapping = aes(y = after_stat(density)),
    alpha = 0.5, position = "identity", binwidth = 0.05
  ) +
  geom_density(alpha = 0.5, linewidth = 1) +
  scale_fill_manual(
    NULL, values = c(
      "Women (Original)" = colours_all[["source"]],
      "Women -> Men (Counterfactual)" = colours_all[["ot"]],
      "Men (Original)" = colours_all[["reference"]]
    )
  ) +
  scale_colour_manual(
    NULL, values = c(
      "Women (Original)" = colours_all[["source"]],
      "Women -> Men (Counterfactual)" = colours_all[["ot"]],
      "Men (Original)" = colours_all[["reference"]]
    )
  ) +
  labs(
    x = "Predictions for Y",
    y = "Density"
  ) +
  global_theme() +
  theme(legend.position = "bottom")

# Distribution of Predicted Scores for Minority Class (Women), Aware model,
# Sensitive: Race, Non-White -> White
ggplot(
  data = aware_ot_nonwhite |>
    mutate(
      group = case_when(
        race_origin == "Non-White" & race == "Non-White" ~ "Women (Original)",
        race_origin == "Non-White" & race == "White" ~ "Women -> Men (Counterfactual)",
        race_origin == "White" & race == "White" ~ "Men (Original)"
      ),
      group = factor(
        group,
        levels = c(
          "Women (Original)", "Women -> Men (Counterfactual)", "Men (Original)"
        )
      )
    ) |>
    filter(race_origin == "Non-White"),
  mapping = aes(x = pred, fill = group, colour = group)
) +
  geom_histogram(
    mapping = aes(y = after_stat(density)),
    alpha = 0.5, position = "identity", binwidth = 0.05
  ) +
  geom_density(alpha = 0.5, linewidth = 1) +
  scale_fill_manual(
    NULL, values = c(
      "Women (Original)" = colours_all[["source"]],
      "Women -> Men (Counterfactual)" = colours_all[["ot"]],
      "Men (Original)" = colours_all[["reference"]]
    )
  ) +
  scale_colour_manual(
    NULL, values = c(
      "Women (Original)" = colours_all[["source"]],
      "Women -> Men (Counterfactual)" = colours_all[["ot"]],
      "Men (Original)" = colours_all[["reference"]]
    )
  ) +
  labs(
    x = "Predictions for Y",
    y = "Density"
  ) +
  global_theme() +
  theme(legend.position = "bottom")

## Fairadapt----
levels(compas |> pull(!!s))

# Two configurations will be considered in turn:

# 1. The reference class consists of White individuals, and fairadapt will be
#    used to obtain the counterfactual values for Non-White individuals as if
#    they had been White individuals.
# 2. The reference class consists of Non-White individuals, and fairadapt will
#    be used to obtain the counterfactual values for White individuals as if
#    they had been Non-White individuals.

# Non-White (factuals) --> White (counterfactuals)
df_fpt <- compas |> mutate(race = fct_relevel(race, "Non-White", after = Inf))
fpt_model_nonwhite <- fairadapt(
  two_year_recid ~ .,
  train.data = df_fpt,
  prot.attr = "race", adj.mat = adj_mat,
  quant.method = rangerQuants
)
adapt_df_nonwhite <- adaptedData(fpt_model_nonwhite)

# White (factuals) --> Non-White (counterfactuals)
df_fpt <- df_fpt |> mutate(race = fct_relevel(race, "White", after = Inf))
fpt_model_white <- fairadapt(
  two_year_recid ~ .,
  train.data = df_fpt,
  prot.attr = "race", adj.mat = adj_mat,
  quant.method = rangerQuants
)
adapt_df_white <- adaptedData(fpt_model_white)

### Unaware Model----

pred_unaware_fpt_nonwhite <- predict(
  model_unaware,
  newdata = adapt_df_nonwhite[ind_nonwhite, ],
  type = "response"
)
pred_unaware_fpt_white <- predict(
  model_unaware,
  newdata = adapt_df_white[ind_white, ],
  type = "response"
)


counterfactuals_unaware_fpt_nonwhite <-
  as_tibble(adapt_df_nonwhite[ind_nonwhite, ]) |>
  mutate(
    race_origin = compas$race[ind_nonwhite],
    pred = pred_unaware_fpt_nonwhite,
    type = "counterfactual",
    id_indiv = ind_nonwhite
  )

counterfactuals_unaware_fpt_white <-
  as_tibble(adapt_df_white[ind_white, ]) |>
  mutate(
    race_origin = compas$race[ind_white],
    pred = pred_unaware_fpt_white,
    type = "counterfactual",
    id_indiv = ind_white
  )

unaware_fpt_nonwhite <-
  factuals_unaware |> mutate(race_origin = race) |>
  bind_rows(counterfactuals_unaware_fpt_nonwhite)

unaware_fpt_white <-
  factuals_unaware |> mutate(race_origin = race) |>
  bind_rows(counterfactuals_unaware_fpt_white)

# Unaware model, Sensitive: Race, Non-White -> White
ggplot(
  data = unaware_fpt_nonwhite |>
    mutate(
      group = case_when(
        race_origin =="Non-White" & race == "Non-White" ~ "Non-White (Original)",
        race_origin =="Non-White" & race == "White" ~ "Non-White -> White (Counterfactual)",
        race_origin =="White" & race == "White" ~ "White (Original)"
      ),
      group = factor(
        group,
        levels = c(
          "Non-White (Original)", "Non-White -> White (Counterfactual)", "White (Original)"
        )
      )
    ),
  aes(x = pred, fill = group, colour = group)
) +
  geom_histogram(
    mapping = aes(
      y = after_stat(density)), alpha = 0.5, colour = NA,
    position = "identity", binwidth = 0.05
  ) +
  geom_density(alpha = 0.3, linewidth = 1) +
  facet_wrap(~race) +
  scale_fill_manual(
    NULL, values = c(
      "Non-White (Original)" = colours_all[["source"]],
      "Non-White -> White (Counterfactual)" = colours_all[["fairadapt"]],
      "White (Original)" = colours_all[["reference"]]
    )
  ) +
  scale_colour_manual(
    NULL, values = c(
      "Non-White (Original)" = colours_all[["source"]],
      "Non-White -> White (Counterfactual)" = colours_all[["fairadapt"]],
      "White (Original)" = colours_all[["reference"]]
    )
  ) +
  labs(
    x = "Predictions for Y",
    y = "Density"
  ) +
  global_theme() +
  theme(legend.position = "bottom")

# Unaware model, Sensitive: Race, White -> Non-White
ggplot(
  data = unaware_fpt_white |>
    mutate(
      group = case_when(
        race_origin =="White" & race == "White" ~ "White (Original)",
        race_origin =="White" & race == "Non-White" ~ "White -> Non-White (Counterfactual)",
        race_origin =="Non-White" & race == "Non-White" ~ "Non-White (Original)"
      ),
      group = factor(
        group,
        levels = c(
          "White (Original)", "White -> Non-White (Counterfactual)", "Non-White (Original)"
        )
      )
    ),
  aes(x = pred, fill = group, colour = group)
) +
  geom_histogram(
    mapping = aes(
      y = after_stat(density)), alpha = 0.5, colour = NA,
    position = "identity", binwidth = 0.05
  ) +
  geom_density(alpha = 0.3, linewidth = 1) +
  facet_wrap(~race) +
  scale_fill_manual(
    NULL, values = c(
      "White (Original)" = colours_all[["source"]],
      "White -> Non-White (Counterfactual)" = colours_all[["fairadapt"]],
      "Non-White (Original)" = colours_all[["reference"]]
    )
  ) +
  scale_colour_manual(
    NULL, values = c(
      "White (Original)" = colours_all[["source"]],
      "White -> Non-White (Counterfactual)" = colours_all[["fairadapt"]],
      "Non-White (Original)" = colours_all[["reference"]]
    )
  ) +
  labs(
    x = "Predictions for Y",
    y = "Density"
  ) +
  global_theme() +
  theme(legend.position = "bottom")

# Distribution of Predicted Scores for Minority Class (Non-White), Unaware
# model, Sensitive: Race, Non-White -> White
ggplot(
  data = unaware_fpt_nonwhite |>
    mutate(
      group = case_when(
        race_origin =="Non-White" & race == "Non-White" ~ "Non-White (Original)",
        race_origin =="Non-White" & race == "White" ~ "Non-White -> White (Counterfactual)",
        race_origin =="White" & race == "White" ~ "White (Original)"
      ),
      group = factor(
        group,
        levels = c(
          "Non-White (Original)", "Non-White -> White (Counterfactual)", "White (Original)"
        )
      )
    ) |>
    filter(race_origin =="Non-White"),
  mapping = aes(x = pred, fill = group, colour = group)
) +
  geom_histogram(
    mapping = aes(y = after_stat(density)),
    alpha = 0.5, position = "identity", binwidth = 0.05
  ) +
  geom_density(alpha = 0.5, linewidth = 1) +
  scale_fill_manual(
    NULL, values = c(
      "Non-White (Original)" = colours_all[["source"]],
      "Non-White -> White (Counterfactual)" = colours_all[["fairadapt"]],
      "White (Original)" = colours_all[["reference"]]
    )
  ) +
  scale_colour_manual(
    NULL, values = c(
      "Non-White (Original)" = colours_all[["source"]],
      "Non-White -> White (Counterfactual)" = colours_all[["fairadapt"]],
      "White (Original)" = colours_all[["reference"]]
    )
  ) +
  labs(
    x = "Predictions for Y",
    y = "Density"
  ) +
  global_theme() +
  theme(legend.position = "bottom")


# Distribution of Predicted Scores for Minority Class (White), Unaware model,
# Sensitive: Race, White -> Non-White
ggplot(
  data = unaware_fpt_white |>
    mutate(
      group = case_when(
        race_origin =="White" & race == "White" ~ "White (Original)",
        race_origin =="White" & race == "Non-White" ~ "White -> Non-White (Counterfactual)",
        race_origin =="Non-White" & race == "Non-White" ~ "Non-White (Original)"
      ),
      group = factor(
        group,
        levels = c(
          "White (Original)", "White -> Non-White (Counterfactual)", "Non-White (Original)"
        )
      )
    ) |>
    filter(race_origin =="White"),
  mapping = aes(x = pred, fill = group, colour = group)) +
  geom_histogram(
    mapping = aes(y = after_stat(density)),
    alpha = 0.5, position = "identity", binwidth = 0.05
  ) +
  geom_density(alpha = 0.5, linewidth = 1) +
  scale_fill_manual(
    NULL, values = c(
      "White (Original)" = colours_all[["source"]],
      "White -> Non-White (Counterfactual)" = colours_all[["fairadapt"]]
    )
  ) +
  scale_colour_manual(
    NULL, values = c(
      "White (Original)" = colours_all[["source"]],
      "White -> Non-White (Counterfactual)" = colours_all[["fairadapt"]]
    )
  ) +
  labs(
    x = "Predictions for Y",
    y = "Density"
  ) +
  global_theme() +
  theme(legend.position = "bottom")

### Aware Model----

pred_aware_fpt_nonwhite <- predict(
  model_aware,
  newdata = adapt_df_nonwhite[ind_nonwhite, ],
  type = "response"
)
pred_aware_fpt_white <- predict(
  model_aware,
  newdata = adapt_df_white[ind_white, ],
  type = "response"
)

counterfactuals_aware_fpt_nonwhite <-
  as_tibble(adapt_df_nonwhite[ind_nonwhite, ]) |>
  mutate(
    race_origin = compas$race[ind_nonwhite],
    pred = pred_aware_fpt_nonwhite,
    type = "counterfactual",
    id_indiv = ind_nonwhite
  )

counterfactuals_aware_fpt_white <-
  as_tibble(adapt_df_white[ind_white, ]) |>
  mutate(
    race_origin = compas$race[ind_white],
    pred = pred_aware_fpt_white,
    type = "counterfactual",
    id_indiv = ind_white
  )

# dataset with counterfactuals, for aware model
aware_fpt_nonwhite <-
  factuals_aware |> mutate(rac_origin = race) |>
  bind_rows(counterfactuals_aware_fpt_nonwhite)

aware_fpt_white <-
  factuals_aware |> mutate(race_origin = race) |>
  bind_rows(counterfactuals_aware_fpt_white)

# Aware model, Sensitive: Race, Reference: White individuals
ggplot(
  data = aware_fpt_nonwhite |>
    mutate(
      group = case_when(
        race_origin =="Non-White" & race == "Non-White" ~ "Non-White (Original)",
        race_origin =="Non-White" & race == "White" ~ "Non-White -> White (Counterfactual)",
        race_origin =="White" & race == "White" ~ "White (Original)"
      ),
      group = factor(
        group,
        levels = c(
          "Non-White (Original)", "Non-White -> White (Counterfactual)", "White (Original)"
        )
      )
    ),
  aes(x = pred, fill = group, colour = group)
) +
  geom_histogram(
    mapping = aes(
      y = after_stat(density)), alpha = 0.5, colour = NA,
    position = "identity", binwidth = 0.05
  ) +
  geom_density(alpha = 0.3, linewidth = 1) +
  facet_wrap(~race) +
  scale_fill_manual(
    NULL, values = c(
      "Non-White (Original)" = colours_all[["source"]],
      "Non-White -> White (Counterfactual)" = colours_all[["fairadapt"]],
      "White (Original)" = colours_all[["reference"]]
    )
  ) +
  scale_colour_manual(
    NULL, values = c(
      "Non-White (Original)" = colours_all[["source"]],
      "Non-White -> White (Counterfactual)" = colours_all[["fairadapt"]],
      "White (Original)" = colours_all[["reference"]]
    )
  ) +
  labs(
    x = "Predictions for Y",
    y = "Density"
  ) +
  global_theme() +
  theme(legend.position = "bottom")

# Aware model, Sensitive: Race, Reference: Non-White individuals
ggplot(
  data = aware_fpt_white |>
    mutate(
      group = case_when(
        race_origin =="White" & race == "White" ~ "White (Original)",
        race_origin =="White" & race == "Non-White" ~ "White -> Non-White (Counterfactual)",
        race_origin =="Non-White" & race == "Non-White" ~ "Non-White (Original)"
      ),
      group = factor(
        group,
        levels = c(
          "White (Original)", "White -> Non-White (Counterfactual)", "Non-White (Original)"
        )
      )
    ),
  aes(x = pred, fill = group, colour = group)
) +
  geom_histogram(
    mapping = aes(
      y = after_stat(density)), alpha = 0.5, colour = NA,
    position = "identity", binwidth = 0.05
  ) +
  geom_density(alpha = 0.3, linewidth = 1) +
  facet_wrap(~race) +
  scale_fill_manual(
    NULL, values = c(
      "White (Original)" = colours_all[["source"]],
      "White -> Non-White (Counterfactual)" = colours_all[["fairadapt"]],
      "Non-White (Original)" = colours_all[["reference"]]
    )
  ) +
  scale_colour_manual(
    NULL, values = c(
      "White (Original)" = colours_all[["source"]],
      "White -> Non-White (Counterfactual)" = colours_all[["fairadapt"]],
      "Non-White (Original)" = colours_all[["reference"]]
    )
  ) +
  labs(
    x = "Predictions for Y",
    y = "Density"
  ) +
  global_theme() +
  theme(legend.position = "bottom")


# Distribution of Predicted Scores for Minority Class (Non-White), Aware model,
# Sensitive: Race, Reference: White individuals
ggplot(
  data = aware_fpt_nonwhite |>
    mutate(
      group = case_when(
        race_origin =="Non-White" & race == "Non-White" ~ "Non-White (Original)",
        race_origin =="Non-White" & race == "White" ~ "Non-White -> White (Counterfactual)",
        race_origin =="White" & race == "White" ~ "White (Original)"
      ),
      group = factor(
        group,
        levels = c(
          "Non-White (Original)", "Non-White -> White (Counterfactual)", "White (Original)"
        )
      )
    ) |>
    filter(race_origin =="Non-White"),
  mapping = aes(x = pred, fill = group, colour = group)
) +
  geom_histogram(
    mapping = aes(y = after_stat(density)),
    alpha = 0.5, position = "identity", binwidth = 0.05
  ) +
  geom_density(alpha = 0.5, linewidth = 1) +
  scale_fill_manual(
    NULL, values = c(
      "Non-White (Original)" = colours_all[["source"]],
      "Non-White -> White (Counterfactual)" = colours_all[["fairadapt"]],
      "White (Original)" = colours_all[["reference"]]
    )
  ) +
  scale_colour_manual(
    NULL, values = c(
      "Non-White (Original)" = colours_all[["source"]],
      "Non-White -> White (Counterfactual)" = colours_all[["fairadapt"]],
      "White (Original)" = colours_all[["reference"]]
    )
  ) +
  labs(
    x = "Predictions for Y",
    y = "Density"
  ) +
  global_theme() +
  theme(legend.position = "bottom")

# Distribution of Predicted Scores for Minority Class (White), Aware model,
# Sensitive: Race, Reference: Non-White individuals
ggplot(
  data = aware_fpt_white |>
    mutate(
      group = case_when(
        race_origin =="White" & race == "White" ~ "White (Original)",
        race_origin =="White" & race == "Non-White" ~ "White -> Non-White (Counterfactual)",
        race_origin =="Non-White" & race == "Non-White" ~ "Non-White (Original)"
      ),
      group = factor(
        group,
        levels = c(
          "White (Original)", "White -> Non-White (Counterfactual)", "Non-White (Original)"
        )
      )
    ) |>
    filter(race_origin =="White"),
  mapping = aes(x = pred, fill = group, colour = group)) +
  geom_histogram(
    mapping = aes(y = after_stat(density)),
    alpha = 0.5, position = "identity", binwidth = 0.05
  ) +
  geom_density(alpha = 0.5, linewidth = 1) +
  scale_fill_manual(
    NULL, values = c(
      "White (Original)" = colours_all[["source"]],
      "White -> Non-White (Counterfactual)" = colours_all[["fairadapt"]]
    )
  ) +
  scale_colour_manual(
    NULL, values = c(
      "White (Original)" = colours_all[["source"]],
      "White -> Non-White (Counterfactual)" = colours_all[["fairadapt"]]
    )
  ) +
  labs(
    x = "Predictions for Y",
    y = "Density"
  ) +
  global_theme() +
  theme(legend.position = "bottom")


## Sequential Transport----

sequential_transport <- seq_trans(
  data = compas, adj = adj_mat,
  s = "race", S_0 = "Non-White", y = "two_year_recid"
)

df_counterfactuals_seq_nonwhite <-
  as_tibble(sequential_transport$transported) |>
  mutate(
    id_indiv = ind_nonwhite,
    race_origin = "Non-White",
    race = "White"
  )

### Predictions----

pred_seq_unaware <- predict(
  model_unaware, newdata = df_counterfactuals_seq_nonwhite, type = "response"
)

pred_seq_aware <- predict(
  model_aware, newdata = df_counterfactuals_seq_nonwhite, type = "response"
)

counterfactuals_unaware_seq_nonwhite <-
  df_counterfactuals_seq_nonwhite |>
  mutate(pred = pred_seq_unaware, type = "counterfactual")

counterfactuals_aware_seq_nonwhite <-
  df_counterfactuals_seq_nonwhite |>
  mutate(pred = pred_seq_aware, type = "counterfactual")

aware_seq_nonwhite <- bind_rows(
  factuals_aware |> mutate(id_indiv = row_number(), race_origin = race),
  counterfactuals_aware_seq_nonwhite |> mutate(S_origin = "Non-White")
)

unaware_seq_nonwhite <- bind_rows(
  factuals_unaware |> mutate(id_indiv = row_number(), race_origin = race),
  counterfactuals_unaware_seq_nonwhite |> mutate(S_origin = "Non-White")
)

# Unaware model, Sensitive: Race, Non-White -> White
ggplot(
  data = unaware_seq_nonwhite |>
    mutate(
      group = case_when(
        race_origin =="Non-White" & race == "Non-White" ~ "Non-White (Original)",
        race_origin =="Non-White" & race == "White" ~ "Non-White -> White (Counterfactual)",
        race_origin =="White" & race == "White" ~ "White (Original)"
      ),
      group = factor(
        group,
        levels = c(
          "Non-White (Original)", "Non-White -> White (Counterfactual)", "White (Original)"
        )
      )
    ),
  aes(x = pred, fill = group, colour = group)
) +
  geom_histogram(
    mapping = aes(
      y = after_stat(density)), alpha = 0.5, colour = NA,
    position = "identity", binwidth = 0.05
  ) +
  geom_density(alpha = 0.3, linewidth = 1) +
  facet_wrap(~race) +
  scale_fill_manual(
    NULL, values = c(
      "Non-White (Original)" = colours_all[["source"]],
      "Non-White -> White (Counterfactual)" = colours_all[["seq"]],
      "White (Original)" = colours_all[["reference"]]
    )
  ) +
  scale_colour_manual(
    NULL, values = c(
      "Non-White (Original)" = colours_all[["source"]],
      "Non-White -> White (Counterfactual)" = colours_all[["seq"]],
      "White (Original)" = colours_all[["reference"]]
    )
  ) +
  labs(
    x = "Predictions for Y",
    y = "Density"
  ) +
  global_theme() +
  theme(legend.position = "bottom")

# Aware model, Sensitive: Race, Non-White -> White
ggplot(
  data = aware_seq_nonwhite |>
    mutate(
      group = case_when(
        race_origin =="Non-White" & race == "Non-White" ~ "Non-White (Original)",
        race_origin =="Non-White" & race == "White" ~ "Non-White -> White (Counterfactual)",
        race_origin =="White" & race == "White" ~ "White (Original)"
      ),
      group = factor(
        group,
        levels = c(
          "Non-White (Original)", "Non-White -> White (Counterfactual)", "White (Original)"
        )
      )
    ),
  aes(x = pred, fill = group, colour = group)
) +
  geom_histogram(
    mapping = aes(
      y = after_stat(density)), alpha = 0.5, colour = NA,
    position = "identity", binwidth = 0.05
  ) +
  geom_density(alpha = 0.3, linewidth = 1) +
  facet_wrap(~race) +
  scale_fill_manual(
    NULL, values = c(
      "Non-White (Original)" = colours_all[["source"]],
      "Non-White -> White (Counterfactual)" = colours_all[["seq"]],
      "White (Original)" = colours_all[["reference"]]
    )
  ) +
  scale_colour_manual(
    NULL, values = c(
      "Non-White (Original)" = colours_all[["source"]],
      "Non-White -> White (Counterfactual)" = colours_all[["seq"]],
      "White (Original)" = colours_all[["reference"]]
    )
  ) +
  labs(
    x = "Predictions for Y",
    y = "Density"
  ) +
  global_theme() +
  theme(legend.position = "bottom")

# Distribution of Predicted Scores for Minority Class (Non-White), Unaware
# model, Sensitive: Race, Non-White -> White
ggplot(
  data = unaware_seq_nonwhite |>
    mutate(
      group = case_when(
        race_origin =="Non-White" & race == "Non-White" ~ "Non-White (Original)",
        race_origin =="Non-White" & race == "White" ~ "Non-White -> White (Counterfactual)",
        race_origin =="White" & race == "White" ~ "White (Original)"
      ),
      group = factor(
        group,
        levels = c(
          "Non-White (Original)", "Non-White -> White (Counterfactual)", "White (Original)"
        )
      )
    ) |>
    filter(race_origin =="Non-White"),
  mapping = aes(x = pred, fill = group, colour = group)
) +
  geom_histogram(
    mapping = aes(y = after_stat(density)),
    alpha = 0.5, position = "identity", binwidth = 0.05
  ) +
  geom_density(alpha = 0.5, linewidth = 1) +
  scale_fill_manual(
    NULL, values = c(
      "Non-White (Original)" = colours_all[["source"]],
      "Non-White -> White (Counterfactual)" = colours_all[["seq"]],
      "White (Original)" = colours_all[["reference"]]
    )
  ) +
  scale_colour_manual(
    NULL, values = c(
      "Non-White (Original)" = colours_all[["source"]],
      "Non-White -> White (Counterfactual)" = colours_all[["seq"]],
      "White (Original)" = colours_all[["reference"]]
    )
  ) +
  labs(
    x = "Predictions for Y",
    y = "Density"
  ) +
  global_theme() +
  theme(legend.position = "bottom")

# Distribution of Predicted Scores for Minority Class (Non-White), Aware model,
# Sensitive: Race, Non-White -> White
ggplot(
  data = aware_seq_nonwhite |>
    mutate(
      group = case_when(
        race_origin =="Non-White" & race == "Non-White" ~ "Non-White (Original)",
        race_origin =="Non-White" & race == "White" ~ "Non-White -> White (Counterfactual)",
        race_origin =="White" & race == "White" ~ "White (Original)"
      ),
      group = factor(
        group,
        levels = c(
          "Non-White (Original)", "Non-White -> White (Counterfactual)", "White (Original)"
        )
      )
    ) |>
    filter(race_origin =="Non-White"),
  mapping = aes(x = pred, fill = group, colour = group)
) +
  geom_histogram(
    mapping = aes(y = after_stat(density)),
    alpha = 0.5, position = "identity", binwidth = 0.05
  ) +
  geom_density(alpha = 0.5, linewidth = 1) +
  scale_fill_manual(
    NULL, values = c(
      "Non-White (Original)" = colours_all[["source"]],
      "Non-White -> White (Counterfactual)" = colours_all[["seq"]],
      "White (Original)" = colours_all[["reference"]]
    )
  ) +
  scale_colour_manual(
    NULL, values = c(
      "Non-White (Original)" = colours_all[["source"]],
      "Non-White -> White (Counterfactual)" = colours_all[["seq"]],
      "White (Original)" = colours_all[["reference"]]
    )
  ) +
  labs(
    x = "Predictions for Y",
    y = "Density"
  ) +
  global_theme() +
  theme(legend.position = "bottom")

# Comparison----

tb_unaware <-
  factuals_unaware |> mutate(counterfactual = "none") |>
  # Naive
  bind_rows(
    counterfactuals_unaware_naive_nonwhite |> mutate(counterfactual = "naive")
  ) |>
  # Multivariate Optimal Transport
  bind_rows(
    counterfactuals_unaware_ot_nonwhite |> mutate(counterfactual = "ot")
  ) |>
  # Fairadapt
  bind_rows(
    counterfactuals_unaware_fpt_nonwhite |> mutate(counterfactual = "fpt")
  ) |>
  # Sequential transport
  bind_rows(
    counterfactuals_unaware_seq_nonwhite |> mutate(counterfactual = "seq")
  )

tb_aware <-
  factuals_aware |> mutate(counterfactual = "none") |>
  # Naive
  bind_rows(
    counterfactuals_aware_naive_nonwhite |> mutate(counterfactual = "naive")
  ) |>
  # Multivariate Optimal Transport
  bind_rows(
    counterfactuals_aware_ot_nonwhite |> mutate(counterfactual = "ot")
  ) |>
  # Fairadapt
  bind_rows(
    counterfactuals_aware_fpt_nonwhite |> mutate(counterfactual = "fpt")
  ) |>
  # Sequential transport
  bind_rows(
    counterfactuals_aware_seq_nonwhite |> mutate(counterfactual = "seq")
  )

## Unaware Model----

# Densities of predicted scores for Non-White individuals with factuals and
# White individuals counterfactuals. The yellow dashed line corresponds to the
# density of predicted scores for Non-White individuals, using factuals.

# Factuals
tb_unaware_factuals <- tb_unaware |> filter(counterfactual == "none")
# Predicted values
pred_unaware_factuals_nonwhite <- tb_unaware_factuals |> filter(race == "Non-White") |> pull("pred")
pred_unaware_factuals_white <- tb_unaware_factuals |> filter(race == "White") |> pull("pred")
# Estimated densities
d_unaware_factuals_nonwhite <- density(pred_unaware_factuals_nonwhite)
d_unaware_factuals_white <- density(pred_unaware_factuals_white)

par(mfrow = c(4, 1), mar = c(2, 2, 1, 0))
x_lim <- c(0, .8)
y_lim <- c(0, 4)

# OT
tb_unaware_ot <- tb_unaware |> filter(counterfactual == "ot")
# Predicted values, focusing on Non-White --> White
pred_unaware_ot_nonwhite_star <- tb_unaware_ot |> filter(race_origin == "Non-White") |> pull("pred")
# Estimated densities
d_unaware_ot_nonwhite_star <- density(pred_unaware_ot_nonwhite_star)

plot(
  d_unaware_factuals_nonwhite,
  main = "", xlab = "", ylab = "",
  axes = FALSE, col = NA,
  xlim = x_lim, ylim = y_lim
)
axis(1)
axis(2)
polygon(d_unaware_factuals_nonwhite, col = alpha(colours_all[["source"]], .5), border = NA)
lines(d_unaware_factuals_white, col = colours_all[["reference"]], lty = 2, lwd = 2)
polygon(d_unaware_ot_nonwhite_star, col = alpha(colours_all[["ot"]], .5), border = NA)
pos_arrow_ref <- .1
text(x = pos_arrow_ref, y = 3.5, "Factuals - White", col = colours_all[["reference"]])
ind_min_ref <- which.min(abs(d_unaware_factuals_white$x - pos_arrow_ref))
arrows(
  x1 = d_unaware_factuals_white$x[ind_min_ref],
  y1 = d_unaware_factuals_white$y[ind_min_ref],
  x0 = pos_arrow_ref,
  y0 = 3,
  length = 0.05, col = colours_all[["reference"]]
)

pos_arrow_ref <- .47
text(x = .4, y = 3.5, "Factuals - Non-White", col = colours_all[["source"]])
ind_min_ref <- which.min(abs(d_unaware_factuals_white$x - pos_arrow_ref))
arrows(
  x1 = d_unaware_factuals_nonwhite$x[ind_min_ref],
  y1 = d_unaware_factuals_nonwhite$y[ind_min_ref],
  x0 = .4,
  y0 = 3,
  length = 0.05, col = colours_all[["source"]]
)
text(x = .7, y = 3.5, "Multi. OT", col = colours_all[["ot"]])


# Naive
tb_unaware_naive <- tb_unaware |> filter(counterfactual == "naive")
# Predicted values, focusing on Non-White --> White
pred_unaware_naive_nonwhite_star <- tb_unaware_naive |> filter(race == "White") |> pull("pred")
# Estimated densities
d_unaware_naive_nonwhite_star <- density(pred_unaware_naive_nonwhite_star)

plot(
  d_unaware_factuals_nonwhite,
  main = "", xlab = "", ylab = "",
  axes = FALSE, col = NA,
  xlim = x_lim, ylim = y_lim
)
axis(1)
axis(2)
polygon(d_unaware_factuals_nonwhite, col = alpha(colours_all[["source"]], .5), border = NA)
lines(d_unaware_factuals_white, col = colours_all[["reference"]], lty = 2, lwd = 2)
polygon(d_unaware_naive_nonwhite_star, col = alpha(colours_all[["naive"]], .5), border = NA)
text(x = .7, y = 3.5, "Naive", col = colours_all[["naive"]])


# Fairadapt
tb_unaware_fpt <- tb_unaware |> filter(counterfactual == "fpt")
# Predicted values, focusing on Non-White --> White
pred_unaware_fpt_nonwhite_star <-
  tb_unaware_fpt |> filter(race == "White") |> pull("pred")
# Estimated densities
d_unaware_fpt_nonwhite_star <- density(pred_unaware_fpt_nonwhite_star)

plot(
  d_unaware_factuals_nonwhite,
  main = "", xlab = "", ylab = "",
  axes = FALSE, col = NA,
  xlim = x_lim, ylim = y_lim
)
axis(1)
axis(2)
polygon(d_unaware_factuals_nonwhite, col = alpha(colours_all[["source"]], .5), border = NA)
lines(d_unaware_factuals_white, col = colours_all[["reference"]], lty = 2, lwd = 2)
polygon(d_unaware_fpt_nonwhite_star, col = alpha(colours_all[["fairadapt"]], .5), border = NA)
text(x = .7, y = 3.5, "fairadapt", col = colours_all[["fairadapt"]])


# Sequential transport
tb_unaware_seq <- tb_unaware |> filter(counterfactual == "seq")
# Predicted values, focusing on Non-White --> White
pred_unaware_seq_nonwhite_star <- tb_unaware_seq |> filter(race == "White") |> pull("pred")
# Estimated densities
d_unaware_seq_nonwhite_star <- density(pred_unaware_seq_nonwhite_star)

plot(
  d_unaware_factuals_nonwhite,
  main = "", xlab = "", ylab = "",
  axes = FALSE, col = NA,
  xlim = x_lim, ylim = y_lim
)
axis(1)
axis(2)
polygon(d_unaware_factuals_nonwhite, col = alpha(colours_all[["source"]], .5), border = NA)
lines(d_unaware_factuals_white, col = colours_all[["reference"]], lty = 2, lwd = 2)
polygon(d_unaware_seq_nonwhite_star, col = alpha(colours_all[["seq"]], .5), border = NA)
text(x = .7, y = 3.5, "Seq. T.", col = colours_all[["seq"]])

## Figure 16 (right), Appendix D, Aware Model ----

# Densities of predicted scores for Non-White individuals with factuals and
# with counterfactuals. The yellow dashed line corresponds to the density of
# predicted scores for Non-White individuals, using factuals.
# Factuals
tb_aware_factuals <- tb_aware |> filter(counterfactual == "none")
# Predicted values
pred_aware_factuals_nonwhite <- tb_aware_factuals |> filter(race == "Non-White") |> pull("pred")
pred_aware_factuals_white <- tb_aware_factuals |> filter(race == "White") |> pull("pred")
# Estimated densities
d_aware_factuals_nonwhite <- density(pred_aware_factuals_nonwhite)
d_aware_factuals_white <- density(pred_aware_factuals_white)

par(mfrow = c(4, 1), mar = c(2, 2, 1, 0))
x_lim <- c(0, .8)
y_lim <- c(0, 4)


# OT
tb_aware_ot <- tb_aware |> filter(counterfactual == "ot")
# Predicted values, focusing on Non-White --> White
pred_aware_ot_nonwhite_star <- tb_aware_ot |> filter(race_origin == "Non-White") |> pull("pred")
# Estimated densities
d_aware_ot_nonwhite_star <- density(pred_aware_ot_nonwhite_star)

plot(
  d_aware_factuals_nonwhite,
  main = "", xlab = "", ylab = "",
  axes = FALSE, col = NA,
  xlim = x_lim, ylim = y_lim
)
axis(1)
axis(2)
polygon(d_aware_factuals_nonwhite, col = alpha(colours_all[["source"]], .5), border = NA)
lines(d_aware_factuals_white, col = colours_all[["reference"]], lty = 2, lwd = 2)
polygon(d_aware_ot_nonwhite_star, col = alpha(colours_all[["ot"]], .5), border = NA)

text(x = .4, y = 3.5, "Factuals - Non-White", col = colours_all[["source"]])
pos_arrow <- .45
ind_min <- which.min(abs(d_aware_factuals_nonwhite$x - pos_arrow))
arrows(
  x1 = d_aware_factuals_nonwhite$x[ind_min],
  y1 = d_aware_factuals_nonwhite$y[ind_min],
  x0 = .4,
  y0 = 3,
  length = 0.05, col = colours_all[["source"]]
)
pos_arrow_ref <- .2
text(x = .1, y = 3.5, "Factuals - White", col = colours_all[["reference"]])
ind_min_ref <- which.min(abs(d_aware_factuals_white$x - pos_arrow_ref))
arrows(
  x1 = d_aware_factuals_white$x[ind_min_ref],
  y1 = d_aware_factuals_white$y[ind_min_ref],
  x0 = .1,
  y0 = 3,
  length = 0.05, col = colours_all[["reference"]]
)
text(x = .7, y = 3.5, "Multi. OT", col = colours_all[["ot"]])

# Naive
tb_aware_naive <- tb_aware |> filter(counterfactual == "naive")
# Predicted values, focusing on Non-White --> White
pred_aware_naive_nonwhite_star <- tb_aware_naive |> filter(race == "White") |> pull("pred")
# Estimated densities
d_aware_naive_nonwhite_star <- density(pred_aware_naive_nonwhite_star)

plot(
  d_aware_factuals_nonwhite,
  main = "", xlab = "", ylab = "",
  axes = FALSE, col = NA,
  xlim = x_lim, ylim = y_lim
)
axis(1)
axis(2)
polygon(d_aware_factuals_nonwhite, col = alpha(colours_all[["source"]], .5), border = NA)
lines(d_aware_factuals_white, col = colours_all[["reference"]], lty = 2, lwd = 2)
polygon(d_aware_naive_nonwhite_star, col = alpha(colours_all[["naive"]], .5), border = NA)
text(x = .7, y = 3.5, "Naive", col = colours_all[["naive"]])


# Fairadapt
tb_aware_fpt <- tb_aware |> filter(counterfactual == "fpt")
# Predicted values, focusing on Non-White --> White
pred_aware_fpt_nonwhite_star <-
  tb_aware_fpt |> filter(race == "White") |> pull("pred")
# Estimated densities
d_aware_fpt_nonwhite_star <- density(pred_aware_fpt_nonwhite_star)

plot(
  d_aware_factuals_nonwhite,
  main = "", xlab = "", ylab = "",
  axes = FALSE, col = NA,
  xlim = x_lim, ylim = y_lim
)
axis(1)
axis(2)
polygon(d_aware_factuals_nonwhite, col = alpha(colours_all[["source"]], .5), border = NA)
lines(d_aware_factuals_white, col = colours_all[["reference"]], lty = 2, lwd = 2)
polygon(d_aware_fpt_nonwhite_star, col = alpha(colours_all[["fairadapt"]], .5), border = NA)
text(x = .7, y = 3.5, "fairadapt", col = colours_all[["fairadapt"]])


# Sequential transport
tb_aware_seq <- tb_aware |> filter(counterfactual == "seq")
# Predicted values, focusing on Non-White --> White
pred_aware_seq_nonwhite_star <- tb_aware_seq |> filter(race == "White") |> pull("pred")
# Estimated densities
d_aware_seq_nonwhite_star <- density(pred_aware_seq_nonwhite_star)

plot(
  d_aware_factuals_nonwhite,
  main = "", xlab = "", ylab = "",
  axes = FALSE, col = NA,
  xlim = x_lim, ylim = y_lim
)
axis(1)
axis(2)
polygon(d_aware_factuals_nonwhite, col = alpha(colours_all[["source"]], .5), border = NA)
lines(d_aware_factuals_white, col = colours_all[["reference"]], lty = 2, lwd = 2)
polygon(d_aware_seq_nonwhite_star, col = alpha(colours_all[["seq"]], .5), border = NA)
text(x = .7, y = 3.5, "Seq. T.", col = colours_all[["seq"]])

# Metrics (Table 2), Appendix D----

# Probability threshold for classifier
threshold <- .5

# Observed target variable
obs_0 <- factuals_aware |> filter(race_origin == "Non-White") |> pull("two_year_recid")
obs_1 <- factuals_aware |> filter(race_origin == "White") |> pull("two_year_recid")
# Scores using factuals
pred_0_aware <- factuals_aware |> filter(race_origin == "Non-White") |> pull("pred")
pred_1_aware <- factuals_aware |> filter(race_origin == "White") |> pull("pred")
pred_0_unaware <- factuals_unaware |> filter(race_origin == "Non-White") |> pull("pred")
pred_1_unaware <- factuals_unaware |> filter(race_origin == "White") |> pull("pred")
# Scores in groups S="Non-White" using naive counterfactuals
pred_0_naive_aware <- pred_aware_naive_nonwhite
pred_0_naive_unaware <- pred_unaware_naive_nonwhite
# Scores in groups S="Non-White" using OT counterfactuals
pred_0_ot_aware <- tb_aware_ot |> filter(race_origin == "Non-White") |> pull("pred")
pred_0_ot_unaware <- tb_unaware_ot |> filter(race_origin == "Non-White") |> pull("pred")
# Scores in groups S="Non-White" using fairadapt counterfactuals
pred_0_fpt_aware <- pred_aware_fpt_nonwhite
pred_0_fpt_unaware <- pred_unaware_fpt_nonwhite
# Scores in groups S="Non-White" using sequential transport counterfactuals
pred_0_seq_aware <- pred_seq_aware
pred_0_seq_unaware <- pred_seq_unaware

names_pred <- rep(c("naive", "ot", "fpt", "seq"), each = 2)
names_model <- rep(c("aware", "unaware"), length(names_pred)/2)
metrics_all <- map2(
  .x = list(
    pred_0_naive_aware, pred_0_naive_unaware,
    pred_0_ot_aware, pred_0_ot_unaware,
    pred_0_fpt_aware, pred_0_fpt_unaware,
    pred_0_seq_aware, pred_0_seq_unaware
  ),
  .y = names_model,
  .f = ~{
    if (.y == "aware") {
      pred_0 <- pred_0_aware
      pred_1 <- pred_1_aware
    } else {
      pred_0 <- pred_0_unaware
      pred_1 <- pred_1_unaware
    }
    counter_fair_metrics(
      obs_0 = obs_0, obs_1 = obs_1,
      pred_0 = pred_0,
      pred_0_t = .x,
      pred_1 = pred_1,
      threshold = threshold
    )
  }
)
names(metrics_all) <- str_c(names_pred, "_", names_model)

group_metrics <-
  map(metrics_all, "group_metrics") |>
  list_rbind(names_to = "model") |>
  mutate(
    cf_type = str_extract(model, "^(.*)_") |> str_remove("_$"),
    model = str_remove(model, "^(.*)_")
  ) |>
  mutate(
    model = factor(
      model, levels = c("aware", "unaware"), labels = c("Aware", "Unaware")
    )
  ) |>
  filter(
    metric %in% c("n_obs", "TPR", "FPR")
  ) |>
  pivot_wider(names_from = "cf_type", values_from = "group_0_t") |>
  relocate(group_1, .after = "seq")

counter_metrics <-
  map(metrics_all, ~enframe(.x[["counter_metrics_0"]])) |>
  list_rbind(names_to = "model") |>
  mutate(
    cf_type = str_extract(model, "^(.*)_") |> str_remove("_$"),
    model = str_remove(model, "^(.*)_")
  ) |>
  mutate(
    model = factor(
      model, levels = c("aware", "unaware"), labels = c("Aware", "Unaware")
    )
  ) |>
  pivot_wider(names_from = "cf_type", values_from = "value") |>
  rename(metric = name) |>
  filter(metric %in% c("c_demog_parity", "c_eq_op", "class_bal_fnr", "c_eq_treatment"))

tb_metrics <-
  group_metrics |> mutate(type = "groups") |>
  bind_rows(counter_metrics |> mutate(type = "cf")) |>
  mutate(
    metric = factor(
      metric,
      levels = c(
        "TPR", "FPR", "c_demog_parity", "c_eq_op", "class_bal_fnr",
        "c_eq_treatment", "n_obs"
      )
    ),
    type = factor(type, levels = c("groups", "cf"))
  ) |>
  arrange(model, type, metric) |>
  relocate(type, .after = "model")

options(knitr.kable.NA = '')

tb_metrics |>
  select(-model, -type) |>
  # select(-type) |>
  knitr::kable(
    col.names = c("", "S=0", "Naive", "OT", "Fairadapt", "Seq", "S=1"),
    digits = 2,
    booktabs = TRUE
  ) |>
  kableExtra::kable_styling() |>
  kableExtra::pack_rows(index = table(tb_metrics$model))
