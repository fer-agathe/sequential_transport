# Real Data Example: Adult dataset

# Source of the data:
# https://archive.ics.uci.edu/dataset/2/adult
#
# We use a formatted version from:
# De Lara, Lucas, Alberto González-Sanz, Nicholas Asher, Laurent Risser,
# and Jean-Michel Loubes. 2024. “Transport-Based Counterfactual Models.”
# Journal of Machine Learning Research 25 (136): 1–59.
#
# Github URL: https://github.com/dplecko/fairadapt


# The adult dataset contains information that allow to predict whether a
# person's income (Y) is over $50,000 a year. We will use gender as the
# protected binary variable here (S). Other characteristics such as the age,
# the native country, the marital status and so on will be used. Note that
# unlike previously, the set of covariates includes both numerical and
# categorical variables.


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
  "sex", "age", "native_country", "marital_status", "education_num",
  "workclass", "hours_per_week", "occupation", "income"
)
s <- "sex"
y <- "income"

library(fairadapt)
# reading in the UCI Adult data
adult <- readRDS(
  system.file("extdata", "uci_adult.rds", package = "fairadapt")
) |>
  as_tibble() |>
  select(!!vars)

adult |> count(income, sex) |>
  group_by(income) |>
  mutate(pct_gender = round(100* n / sum(n), 2))

# Causal Graph----

# Adjacency matrix
# As in Plečko, Drago, and Nicolai Meinshausen. 2020.
# 'Fair Data Adaptation with Quantile Preservation.' Journal of Machine
# Learning Research 21 (242): 1–44.
# URL: https://github.com/dplecko/fairadapt/blob/main/jmlr-paper/real-data/Figures-6-7-8-9.R

adj_mat <- c(
  0, 0, 0, 1, 1, 1, 1, 1, 1, # sex
  0, 0, 0, 1, 1, 1, 1, 1, 1, # age
  0, 0, 0, 1, 1, 1, 1, 1, 1, # native_country
  0, 0, 0, 0, 1, 1, 1, 1, 1, # marital_status
  0, 0, 0, 0, 0, 1, 1, 1, 1, # education_num
  0, 0, 0, 0, 0, 0, 0, 0, 1, # workclass
  0, 0, 0, 0, 0, 0, 0, 0, 1, # hours_per_week
  0, 0, 0, 0, 0, 0, 0, 0, 1, # occupation
  0, 0, 0, 0, 0, 0, 0, 0, 0  # income
) |> matrix(
  nrow = length(vars), ncol = length(vars),
  dimnames = list(vars, vars), byrow = TRUE
)

causal_graph <- fairadapt::graphModel(adj_mat)
plot(causal_graph)

# Classifier----

seed <- 2025
sets <- split_dataset(adult, seed, train_ratio = 0.7)
data_train <- sets$data_train
data_test <- sets$data_test

## Training the Model----

# Two types of models:
# 1. aware (with the sensitive variable)
# 2. unaware (without the sensitive variable)

# Unaware logistic regression classifier (model without S)
# Unaware logistic regression classifier (model without S)
pred_unaware <- log_reg_train(data_train, data_test, s = s, y = y, type = "unaware")
pred_unaware_train <- pred_unaware$pred_train
pred_unaware_test <- pred_unaware$pred_test

# Aware logistic regression classifier (model with S)
pred_aware <- log_reg_train(data_train, data_test, s = s, y = y, type = "aware")
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
  newdata = adult,
  type = "response"
)
model_aware <- pred_aware$model
pred_aware_all <- predict(
  model_aware,
  newdata = adult,
  type = "response"
)

# Factuals----
factuals_unaware <-
  adult |>
  as_tibble() |>
  mutate(
    sex_origin = sex,
    pred = pred_unaware_all,
    type = "factual"
  ) |>
  mutate(id_indiv = row_number())

factuals_aware <-
  adult |>
  as_tibble() |>
  mutate(
    sex_origin = sex,
    pred = pred_aware_all,
    type = "factual"
  ) |>
  mutate(id_indiv = row_number())

# Counterfactuals----

## Naive approach: ceteris paribus----

pred_unaware_naive_women <- predict(
  model_unaware,
  newdata = adult |> filter(sex == "Female") |> mutate(sex = "Male"),
  type = "response"
)
pred_aware_naive_women <- predict(
  model_aware,
  newdata = adult |> filter(sex == "Female") |>  mutate(sex = "Male"),
  type = "response"
)

ind_women <- which(adult$sex == "Female")
ind_men <- which(adult$sex == "Male")

counterfactuals_unaware_naive_women <-
  adult |> filter(sex == "Female") |>
  mutate(
    sex_origin = sex,
    sex = "Male",
    pred = pred_unaware_naive_women,
    type = "counterfactual",
    id_indiv = ind_women
  )
counterfactuals_aware_naive_women <-
  adult |> filter(sex == "Female") |>
  mutate(
    sex_origin = sex,
    sex = "Male",
    pred = pred_aware_naive_women,
    type = "counterfactual",
    id_indiv = ind_women
  )

#### Unaware Model----

unaware_naive_women <-
  factuals_unaware |> mutate(sex_origin = sex) |>
  bind_rows(counterfactuals_unaware_naive_women)

# Unaware model, Sensitive: Sex, Woman -> Man
ggplot(
  unaware_naive_women |> mutate(
    group = case_when(
      sex_origin == "Female" & sex == "Female" ~ "Women (Original)",
      sex_origin == "Female" & sex == "Male" ~ "Women -> Men (Counterfactual)",
      sex_origin == "Male" & sex == "Male" ~ "Men (Original)"
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
  facet_wrap(~sex) +
  scale_fill_manual(
    NULL, values = c(
      "Women (Original)" = colours_all[["source"]],
      "Women -> Men (Counterfactual)" = colours_all[["naive"]],
      "Men (Original)" = colours_all[["reference"]]
    )
  ) +
  scale_colour_manual(
    NULL, values = c(
      "Women (Original)" = colours_all[["source"]],
      "Women -> Men (Counterfactual)" = colours_all[["naive"]],
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
# Sensitive: Race, Woman -> Man
ggplot(
  data = unaware_naive_women |>
    mutate(
      group = case_when(
        sex_origin == "Female" & sex == "Female" ~ "Women (Original)",
        sex_origin == "Female" & sex == "Male" ~ "Women -> Men (Counterfactual)",
        sex_origin == "Male" & sex == "Male" ~ "Men (Original)"
      ),
      group = factor(
        group,
        levels = c(
          "Women (Original)", "Women -> Men (Counterfactual)", "Men (Original)"
        )
      )
    ) |>
    filter(sex_origin == "Female"),
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
      "Women -> Men (Counterfactual)" = colours_all[["naive"]],
      "Men (Original)" = colours_all[["reference"]]
    )
  ) +
  scale_colour_manual(
    NULL, values = c(
      "Women (Original)" = colours_all[["source"]],
      "Women -> Men (Counterfactual)" = colours_all[["naive"]],
      "Men (Original)" = colours_all[["reference"]]
    )
  ) +
  labs(
    x = "Predictions for Y",
    y = "Density"
  ) +
  global_theme() +
  theme(legend.position = "bottom")

#### Aware Model----

aware_naive_women <-
  factuals_aware |> mutate(sex_origin = sex) |>
  bind_rows(counterfactuals_aware_naive_women)

# Aware model, Sensitive: Sex, Woman -> Man
ggplot(
  aware_naive_women |> mutate(
    group = case_when(
      sex_origin == "Female" & sex == "Female" ~ "Women (Original)",
      sex_origin == "Female" & sex == "Male" ~ "Women -> Men (Counterfactual)",
      sex_origin == "Male" & sex == "Male" ~ "Men (Original)"
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
  facet_wrap(~sex) +
  scale_fill_manual(
    NULL, values = c(
      "Women (Original)" = colours_all[["source"]],
      "Women -> Men (Counterfactual)" = colours_all[["naive"]],
      "Men (Original)" = colours_all[["reference"]]
    )
  ) +
  scale_colour_manual(
    NULL, values = c(
      "Women (Original)" = colours_all[["source"]],
      "Women -> Men (Counterfactual)" = colours_all[["naive"]],
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
# Sensitive: Race, Woman -> Man
ggplot(
  data = aware_naive_women |>
    mutate(
      group = case_when(
        sex_origin == "Female" & sex == "Female" ~ "Women (Original)",
        sex_origin == "Female" & sex == "Male" ~ "Women -> Men (Counterfactual)",
        sex_origin == "Male" & sex == "Male" ~ "Men (Original)"
      ),
      group = factor(
        group,
        levels = c(
          "Women (Original)", "Women -> Men (Counterfactual)", "Men (Original)"
        )
      )
    ) |>
    filter(sex_origin == "Female"),
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
      "Women -> Men (Counterfactual)" = colours_all[["naive"]],
      "Men (Original)" = colours_all[["reference"]]
    )
  ) +
  scale_colour_manual(
    NULL, values = c(
      "Women (Original)" = colours_all[["source"]],
      "Women -> Men (Counterfactual)" = colours_all[["naive"]],
      "Men (Original)" = colours_all[["reference"]]
    )
  ) +
  labs(
    x = "Predictions for Y",
    y = "Density"
  ) +
  global_theme() +
  theme(legend.position = "bottom")

## Multivariate Optimal Transport----

# First, save CSV
write.csv(
  factuals_aware, file = "../data/factuals_aware-adult.csv",
  row.names = FALSE
)

# Then, run script in python
library(reticulate)
use_virtualenv("~/quarto-python-env", required = TRUE)
# reticulate::install_miniconda(force = TRUE)
# py_install("POT")
# py_install("scikit-learn")
# py_install("pandas")
# py_install("numpy")
# py_install("matplotlib")
# py_install("scipy")
py_run_file("07_real-data-adult-ot.py")

# Load results
df_counterfactuals_ot_women <- read_csv('../data/counterfactuals-ot-women-adult.csv') |>
  mutate(
    id_indiv = ind_women,
    sex_origin = "Female",
    sex = "Male"
  ) |>
  mutate(
    native_country = str_remove(native_country, "^country_"),
    marital_status = str_remove(marital_status, "^status_"),
  )

### Predictions----
pred_ot_unaware <- predict(
  model_unaware, newdata = df_counterfactuals_ot_women, type = "response"
)

pred_ot_aware <- predict(
  model_aware, newdata = df_counterfactuals_ot_women, type = "response"
)

counterfactuals_unaware_ot_women <-
  df_counterfactuals_ot_women |>
  mutate(pred = pred_ot_unaware, type = "counterfactual")
counterfactuals_aware_ot_women <-
  df_counterfactuals_ot_women |>
  mutate(pred = pred_ot_aware, type = "counterfactual")

aware_ot_women <- bind_rows(
  factuals_aware |> mutate(id_indiv = row_number(), sex_origin = sex),
  counterfactuals_aware_ot_women |> mutate(S_origin = "Female")
)
unaware_ot_women <- bind_rows(
  factuals_unaware |> mutate(id_indiv = row_number(), sex_origin = sex),
  counterfactuals_unaware_ot_women |> mutate(S_origin = "Female")
)


# Unaware model, Sensitive: Race, Woman -> Man
ggplot(
  data = unaware_ot_women |>
    mutate(
      group = case_when(
        sex_origin == "Female" & sex == "Female" ~ "Women (Original)",
        sex_origin == "Female" & sex == "Male" ~ "Women -> Men (Counterfactual)",
        sex_origin == "Male" & sex == "Male" ~ "Men (Original)"
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
  facet_wrap(~sex) +
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

# Aware model, Sensitive: Race, Woman -> Man
ggplot(
  data = aware_ot_women |>
    mutate(
      group = case_when(
        sex_origin == "Female" & sex == "Female" ~ "Women (Original)",
        sex_origin == "Female" & sex == "Male" ~ "Women -> Men (Counterfactual)",
        sex_origin == "Male" & sex == "Male" ~ "Men (Original)"
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
  facet_wrap(~sex) +
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

# Distribution of Predicted Scores for Minority Class (Women),
# Unaware model, Sensitive: Race, Woman -> Man
ggplot(
  data = unaware_ot_women |>
    mutate(
      group = case_when(
        sex_origin == "Female" & sex == "Female" ~ "Women (Original)",
        sex_origin == "Female" & sex == "Male" ~ "Women -> Men (Counterfactual)",
        sex_origin == "Male" & sex == "Male" ~ "Men (Original)"
      ),
      group = factor(
        group,
        levels = c(
          "Women (Original)", "Women -> Men (Counterfactual)", "Men (Original)"
        )
      )
    ) |>
    filter(sex_origin == "Female"),
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
# Sensitive: Race, Woman -> Man
ggplot(
  data = aware_ot_women |>
    mutate(
      group = case_when(
        sex_origin == "Female" & sex == "Female" ~ "Women (Original)",
        sex_origin == "Female" & sex == "Male" ~ "Women -> Men (Counterfactual)",
        sex_origin == "Male" & sex == "Male" ~ "Men (Original)"
      ),
      group = factor(
        group,
        levels = c(
          "Women (Original)", "Women -> Men (Counterfactual)", "Men (Original)"
        )
      )
    ) |>
    filter(sex_origin == "Female"),
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

levels(adult |> pull(!!s))

# Two configurations will be considered in turn:

# - The reference class consists of men, and fairadapt will be used to obtain
#   the counterfactual values for women as if they had been men.
# - The reference class consists of women, and fairadapt will be used to obtain
#   the counterfactual values for men as if they had been women.

# Women (factuals) --> Men (counterfactuals)
df_fpt <- adult |> mutate(sex = fct_relevel(sex, "Female", after = Inf))
fpt_model_women <- fairadapt(
  income ~ .,
  train.data = df_fpt,
  prot.attr = "sex", adj.mat = adj_mat,
  quant.method = rangerQuants
)
adapt_df_women <- adaptedData(fpt_model_women)

# Men (factuals) --> Women (counterfactuals)
df_fpt <- df_fpt |> mutate(sex = fct_relevel(sex, "Male", after = Inf))
fpt_model_men <- fairadapt(
  income ~ .,
  train.data = df_fpt,
  prot.attr = "sex", adj.mat = adj_mat,
  quant.method = rangerQuants
)
adapt_df_men <- adaptedData(fpt_model_men)

### Predictions----


#### Unaware Model----
pred_unaware_fpt_women <- predict(
  model_unaware,
  newdata = adapt_df_women[ind_women, ],
  type = "response"
)
pred_unaware_fpt_men <- predict(
  model_unaware,
  newdata = adapt_df_men[ind_men, ],
  type = "response"
)

counterfactuals_unaware_fpt_women <-
  as_tibble(adapt_df_women[ind_women, ]) |>
  mutate(
    sex_origin = adult$sex[ind_women],
    pred = pred_unaware_fpt_women,
    type = "counterfactual",
    id_indiv = ind_women
  )

counterfactuals_unaware_fpt_men <-
  as_tibble(adapt_df_men[ind_men, ]) |>
  mutate(
    sex_origin = adult$sex[ind_men],
    pred = pred_unaware_fpt_men,
    type = "counterfactual",
    id_indiv = ind_men
  )

unaware_fpt_women <-
  factuals_unaware |> mutate(sex_origin = sex) |>
  bind_rows(counterfactuals_unaware_fpt_women)

unaware_fpt_men <-
  factuals_unaware |> mutate(sex_origin = sex) |>
  bind_rows(counterfactuals_unaware_fpt_men)

# Unaware model, Sensitive: Sex, Woman -> Man
ggplot(
  data = unaware_fpt_women |>
    mutate(
      group = case_when(
        sex_origin == "Female" & sex == "Female" ~ "Women (Original)",
        sex_origin == "Female" & sex == "Male" ~ "Women -> Men (Counterfactual)",
        sex_origin == "Male" & sex == "Male" ~ "Men (Original)"
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
  facet_wrap(~sex) +
  scale_fill_manual(
    NULL, values = c(
      "Women (Original)" = colours_all[["source"]],
      "Women -> Men (Counterfactual)" = colours_all[["fairadapt"]],
      "Men (Original)" = colours_all[["reference"]]
    )
  ) +
  scale_colour_manual(
    NULL, values = c(
      "Women (Original)" = colours_all[["source"]],
      "Women -> Men (Counterfactual)" = colours_all[["fairadapt"]],
      "Men (Original)" = colours_all[["reference"]]
    )
  ) +
  labs(
    x = "Predictions for Y",
    y = "Density"
  ) +
  global_theme() +
  theme(legend.position = "bottom")

# Unaware model, Sensitive: Sex, Man -> Woman
ggplot(
  data = unaware_fpt_men |>
    mutate(
      group = case_when(
        sex_origin == "Male" & sex == "Male" ~ "Men (Original)",
        sex_origin == "Male" & sex == "Female" ~ "Men -> Women (Counterfactual)",
        sex_origin == "Female" & sex == "Female" ~ "Women (Original)"
      ),
      group = factor(
        group,
        levels = c(
          "Men (Original)", "Men -> Women (Counterfactual)", "Women (Original)"
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
  facet_wrap(~sex) +
  scale_fill_manual(
    NULL, values = c(
      "Men (Original)" = colours_all[["source"]],
      "Men -> Women (Counterfactual)" = colours_all[["fairadapt"]],
      "Women (Original)" = colours_all[["reference"]]
    )
  ) +
  scale_colour_manual(
    NULL, values = c(
      "Men (Original)" = colours_all[["source"]],
      "Men -> Women (Counterfactual)" = colours_all[["fairadapt"]],
      "Women (Original)" = colours_all[["reference"]]
    )
  ) +
  labs(
    x = "Predictions for Y",
    y = "Density"
  ) +
  global_theme() +
  theme(legend.position = "bottom")

# Distribution of Predicted Scores for Minority Class (Women), Unaware model,
# Sensitive: Sex, Woman -> Man
ggplot(
  data = unaware_fpt_women |>
    mutate(
      group = case_when(
        sex_origin == "Female" & sex == "Female" ~ "Women (Original)",
        sex_origin == "Female" & sex == "Male" ~ "Women -> Men (Counterfactual)",
        sex_origin == "Male" & sex == "Male" ~ "Men (Original)"
      ),
      group = factor(
        group,
        levels = c(
          "Women (Original)", "Women -> Men (Counterfactual)", "Men (Original)"
        )
      )
    ) |>
    filter(sex_origin == "Female"),
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
      "Women -> Men (Counterfactual)" = colours_all[["fairadapt"]],
      "Men (Original)" = colours_all[["reference"]]
    )
  ) +
  scale_colour_manual(
    NULL, values = c(
      "Women (Original)" = colours_all[["source"]],
      "Women -> Men (Counterfactual)" = colours_all[["fairadapt"]],
      "Men (Original)" = colours_all[["reference"]]
    )
  ) +
  labs(
    x = "Predictions for Y",
    y = "Density"
  ) +
  global_theme() +
  theme(legend.position = "bottom")

# Distribution of Predicted Scores for Minority Class (White), Unaware model,
# Sensitive: Race, Man -> Woman
ggplot(
  data = unaware_fpt_men |>
    mutate(
      group = case_when(
        sex_origin == "Male" & sex == "Male" ~ "Men (Original)",
        sex_origin == "Male" & sex == "Female" ~ "Men -> Women (Counterfactual)",
        sex_origin == "Female" & sex == "Female" ~ "Women (Original)"
      ),
      group = factor(
        group,
        levels = c(
          "Men (Original)", "Men -> Women (Counterfactual)", "Women (Original)"
        )
      )
    ) |>
    filter(sex_origin == "Male"),
  mapping = aes(x = pred, fill = group, colour = group)) +
  geom_histogram(
    mapping = aes(y = after_stat(density)),
    alpha = 0.5, position = "identity", binwidth = 0.05
  ) +
  geom_density(alpha = 0.5, linewidth = 1) +
  scale_fill_manual(
    NULL, values = c(
      "Men (Original)" = colours_all[["source"]],
      "Men -> Women (Counterfactual)" = colours_all[["fairadapt"]]
    )
  ) +
  scale_colour_manual(
    NULL, values = c(
      "Men (Original)" = colours_all[["source"]],
      "Men -> Women (Counterfactual)" = colours_all[["fairadapt"]]
    )
  ) +
  labs(
    x = "Predictions for Y",
    y = "Density"
  ) +
  global_theme() +
  theme(legend.position = "bottom")

#### Aware Model----

pred_aware_fpt_women <- predict(
  model_aware,
  newdata = adapt_df_women[ind_women, ],
  type = "response"
)
pred_aware_fpt_men <- predict(
  model_aware,
  newdata = adapt_df_men[ind_men, ],
  type = "response"
)

counterfactuals_aware_fpt_women <-
  as_tibble(adapt_df_women[ind_women, ]) |>
  mutate(
    sex_origin = adult$sex[ind_women],
    pred = pred_aware_fpt_women,
    type = "counterfactual",
    id_indiv = ind_women
  )

counterfactuals_aware_fpt_men <-
  as_tibble(adapt_df_men[ind_men, ]) |>
  mutate(
    sex_origin = adult$sex[ind_men],
    pred = pred_aware_fpt_men,
    type = "counterfactual",
    id_indiv = ind_men
  )

# dataset with counterfactuals, for aware model
aware_fpt_women <-
  factuals_aware |> mutate(sex_origin = sex) |>
  bind_rows(counterfactuals_aware_fpt_women)

aware_fpt_men <-
  factuals_aware |> mutate(sex_origin = sex) |>
  bind_rows(counterfactuals_aware_fpt_men)


# Aware model, Sensitive: Race, Reference: Men individuals
ggplot(
  data = aware_fpt_women |>
    mutate(
      group = case_when(
        sex_origin == "Female" & sex == "Female" ~ "Women (Original)",
        sex_origin == "Female" & sex == "Male" ~ "Women -> Men (Counterfactual)",
        sex_origin == "Male" & sex == "Male" ~ "Men (Original)"
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
  facet_wrap(~sex) +
  scale_fill_manual(
    NULL, values = c(
      "Women (Original)" = colours_all[["source"]],
      "Women -> Men (Counterfactual)" = colours_all[["fairadapt"]],
      "Men (Original)" = colours_all[["reference"]]
    )
  ) +
  scale_colour_manual(
    NULL, values = c(
      "Women (Original)" = colours_all[["source"]],
      "Women -> Men (Counterfactual)" = colours_all[["fairadapt"]],
      "Men (Original)" = colours_all[["reference"]]
    )
  ) +
  labs(
    x = "Predictions for Y",
    y = "Density"
  ) +
  global_theme() +
  theme(legend.position = "bottom")

# Aware model, Sensitive: Race, Reference: Women individuals
ggplot(
  data = aware_fpt_men |>
    mutate(
      group = case_when(
        sex_origin == "Male" & sex == "Male" ~ "Men (Original)",
        sex_origin == "Male" & sex == "Female" ~ "Men -> Women (Counterfactual)",
        sex_origin == "Female" & sex == "Female" ~ "Women (Original)"
      ),
      group = factor(
        group,
        levels = c(
          "Men (Original)", "Men -> Women (Counterfactual)", "Women (Original)"
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
  facet_wrap(~sex) +
  scale_fill_manual(
    NULL, values = c(
      "Men (Original)" = colours_all[["source"]],
      "Men -> Women (Counterfactual)" = colours_all[["fairadapt"]],
      "Women (Original)" = colours_all[["reference"]]
    )
  ) +
  scale_colour_manual(
    NULL, values = c(
      "Men (Original)" = colours_all[["source"]],
      "Men -> Women (Counterfactual)" = colours_all[["fairadapt"]],
      "Women (Original)" = colours_all[["reference"]]
    )
  ) +
  labs(
    x = "Predictions for Y",
    y = "Density"
  ) +
  global_theme() +
  theme(legend.position = "bottom")

# Distribution of Predicted Scores for Minority Class (Women), Aware model,
# Sensitive: Race, Reference: Men individuals
ggplot(
  data = aware_fpt_women |>
    mutate(
      group = case_when(
        sex_origin == "Female" & sex == "Female" ~ "Women (Original)",
        sex_origin == "Female" & sex == "Male" ~ "Women -> Men (Counterfactual)",
        sex_origin == "Male" & sex == "Male" ~ "Men (Original)"
      ),
      group = factor(
        group,
        levels = c(
          "Women (Original)", "Women -> Men (Counterfactual)", "Men (Original)"
        )
      )
    ) |>
    filter(sex_origin == "Female"),
  mapping = aes(x = pred, fill = group, colour = group)
) +
  geom_histogram(
    mapping = aes(y = after_stat(density)),
    alpha = 0.5, position = "identity", binwidth = 0.05
  ) +
  geom_density(alpha = 0.5, linewidth  =1) +
  scale_fill_manual(
    NULL, values = c(
      "Women (Original)" = colours_all[["source"]],
      "Women -> Men (Counterfactual)" = colours_all[["fairadapt"]],
      "Men (Original)" = colours_all[["reference"]]
    )
  ) +
  scale_colour_manual(
    NULL, values = c(
      "Women (Original)" = colours_all[["source"]],
      "Women -> Men (Counterfactual)" = colours_all[["fairadapt"]],
      "Men (Original)" = colours_all[["reference"]]
    )
  ) +
  labs(
    x = "Predictions for Y",
    y = "Density"
  ) +
  global_theme() +
  theme(legend.position = "bottom")

# Distribution of Predicted Scores for Minority Class (Men), Aware model,
# Sensitive: Race, Reference: Women individuals
ggplot(
  data = aware_fpt_men |>
    mutate(
      group = case_when(
        sex_origin == "Male" & sex == "Male" ~ "Men (Original)",
        sex_origin == "Male" & sex == "Female" ~ "Men -> Women (Counterfactual)",
        sex_origin == "Female" & sex == "Female" ~ "Women (Original)"
      ),
      group = factor(
        group,
        levels = c(
          "Men (Original)", "Men -> Women (Counterfactual)", "Women (Original)"
        )
      )
    ) |>
    filter(sex_origin == "Male"),
  mapping = aes(x = pred, fill = group, colour = group)) +
  geom_histogram(
    mapping = aes(y = after_stat(density)),
    alpha = 0.5, position = "identity", binwidth = 0.05
  ) +
  geom_density(alpha = 0.5, linewidth = 1) +
  scale_fill_manual(
    NULL, values = c(
      "Men (Original)" = colours_all[["source"]],
      "Men -> Women (Counterfactual)" = colours_all[["fairadapt"]]
    )
  ) +
  scale_colour_manual(
    NULL, values = c(
      "Men (Original)" = colours_all[["source"]],
      "Men -> Women (Counterfactual)" = colours_all[["fairadapt"]]
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
  data = adult, adj = adj_mat, s = "sex", S_0 = "Female", y = "income"
)

df_counterfactuals_seq_women <-
  as_tibble(sequential_transport$transported) |>
  mutate(
    id_indiv = ind_women,
    sex_origin = "Female",
    sex = "Male"
  )

### Predictions----
pred_seq_unaware <- predict(
  model_unaware, newdata = df_counterfactuals_seq_women, type = "response"
)

pred_seq_aware <- predict(
  model_aware, newdata = df_counterfactuals_seq_women, type = "response"
)

counterfactuals_unaware_seq_women <-
  df_counterfactuals_seq_women |>
  mutate(pred = pred_seq_unaware, type = "counterfactual")
counterfactuals_aware_seq_women <-
  df_counterfactuals_seq_women |>
  mutate(pred = pred_seq_aware, type = "counterfactual")

aware_seq_women <- bind_rows(
  factuals_aware |> mutate(id_indiv = row_number(), sex_origin = sex),
  counterfactuals_aware_seq_women |> mutate(S_origin = "Female")
)
unaware_seq_women <- bind_rows(
  factuals_unaware |> mutate(id_indiv = row_number(), sex_origin = sex),
  counterfactuals_unaware_seq_women |> mutate(S_origin = "Female")
)

# Unaware model, Sensitive: Race, Woman -> Man
ggplot(
  data = unaware_seq_women |>
    mutate(
      group = case_when(
        sex_origin == "Female" & sex == "Female" ~ "Women (Original)",
        sex_origin == "Female" & sex == "Male" ~ "Women -> Men (Counterfactual)",
        sex_origin == "Male" & sex == "Male" ~ "Men (Original)"
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
  facet_wrap(~sex) +
  scale_fill_manual(
    NULL, values = c(
      "Women (Original)" = colours_all[["source"]],
      "Women -> Men (Counterfactual)" = colours_all[["seq"]],
      "Men (Original)" = colours_all[["reference"]]
    )
  ) +
  scale_colour_manual(
    NULL, values = c(
      "Women (Original)" = colours_all[["source"]],
      "Women -> Men (Counterfactual)" = colours_all[["seq"]],
      "Men (Original)" = colours_all[["reference"]]
    )
  ) +
  labs(
    x = "Predictions for Y",
    y = "Density"
  ) +
  global_theme() +
  theme(legend.position = "bottom")

# Aware model, Sensitive: Race, Woman -> Man
ggplot(
  data = aware_seq_women |>
    mutate(
      group = case_when(
        sex_origin == "Female" & sex == "Female" ~ "Women (Original)",
        sex_origin == "Female" & sex == "Male" ~ "Women -> Men (Counterfactual)",
        sex_origin == "Male" & sex == "Male" ~ "Men (Original)"
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
  facet_wrap(~sex) +
  scale_fill_manual(
    NULL, values = c(
      "Women (Original)" = colours_all[["source"]],
      "Women -> Men (Counterfactual)" = colours_all[["seq"]],
      "Men (Original)" = colours_all[["reference"]]
    )
  ) +
  scale_colour_manual(
    NULL, values = c(
      "Women (Original)" = colours_all[["source"]],
      "Women -> Men (Counterfactual)" = colours_all[["seq"]],
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
# Sensitive: Race, Woman -> Man
ggplot(
  data = unaware_seq_women |>
    mutate(
      group = case_when(
        sex_origin == "Female" & sex == "Female" ~ "Women (Original)",
        sex_origin == "Female" & sex == "Male" ~ "Women -> Men (Counterfactual)",
        sex_origin == "Male" & sex == "Male" ~ "Men (Original)"
      ),
      group = factor(
        group,
        levels = c(
          "Women (Original)", "Women -> Men (Counterfactual)", "Men (Original)"
        )
      )
    ) |>
    filter(sex_origin == "Female"),
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
      "Women -> Men (Counterfactual)" = colours_all[["seq"]],
      "Men (Original)" = colours_all[["reference"]]
    )
  ) +
  scale_colour_manual(
    NULL, values = c(
      "Women (Original)" = colours_all[["source"]],
      "Women -> Men (Counterfactual)" = colours_all[["seq"]],
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
# Sensitive: Race, Woman -> Man
ggplot(
  data = aware_seq_women |>
    mutate(
      group = case_when(
        sex_origin == "Female" & sex == "Female" ~ "Women (Original)",
        sex_origin == "Female" & sex == "Male" ~ "Women -> Men (Counterfactual)",
        sex_origin == "Male" & sex == "Male" ~ "Men (Original)"
      ),
      group = factor(
        group,
        levels = c(
          "Women (Original)", "Women -> Men (Counterfactual)", "Men (Original)"
        )
      )
    ) |>
    filter(sex_origin == "Female"),
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
      "Women -> Men (Counterfactual)" = colours_all[["seq"]],
      "Men (Original)" = colours_all[["reference"]]
    )
  ) +
  scale_colour_manual(
    NULL, values = c(
      "Women (Original)" = colours_all[["source"]],
      "Women -> Men (Counterfactual)" = colours_all[["seq"]],
      "Men (Original)" = colours_all[["reference"]]
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
    counterfactuals_unaware_naive_women |> mutate(counterfactual = "naive")
  ) |>
  # OT
  bind_rows(
    counterfactuals_unaware_ot_women |> mutate(counterfactual = "ot")
  ) |>
  # Fairadapt
  bind_rows(
    counterfactuals_unaware_fpt_women |> mutate(counterfactual = "fpt")
  ) |>
  # Sequential transport
  bind_rows(
    counterfactuals_unaware_seq_women |> mutate(counterfactual = "seq")
  )

tb_aware <-
  factuals_aware |> mutate(counterfactual = "none") |>
  # Naive
  bind_rows(
    counterfactuals_aware_naive_women |> mutate(counterfactual = "naive")
  ) |>
  # OT
  bind_rows(
    counterfactuals_aware_ot_women |> mutate(counterfactual = "ot")
  ) |>
  # Fairadapt
  bind_rows(
    counterfactuals_aware_fpt_women |> mutate(counterfactual = "fpt")
  ) |>
  # Sequential transport
  bind_rows(
    counterfactuals_aware_seq_women |> mutate(counterfactual = "seq")
  )

## Densities----

### Unaware----

# Factuals
tb_unaware_factuals <- tb_unaware |> filter(counterfactual == "none")
# Predicted values
pred_unaware_factuals_women <- tb_unaware_factuals |> filter(sex == "Female") |> pull("pred")
pred_unaware_factuals_men <- tb_unaware_factuals |> filter(sex == "Male") |> pull("pred")
# Estimated densities
d_unaware_factuals_women <- density(pred_unaware_factuals_women)
d_unaware_factuals_men <- density(pred_unaware_factuals_men)

par(mfrow = c(4, 1), mar = c(2, 2, 0, 0))
x_lim <- c(0, .8)
y_lim <- c(0, 10)

# OT
tb_unaware_ot <- tb_unaware |> filter(counterfactual == "ot")
# Predicted values, focusing on Black --> White
pred_unaware_ot_women_star <- tb_unaware_ot |> filter(sex_origin == "Female") |> pull("pred")
# Estimated densities
d_unaware_ot_women_star <- density(pred_unaware_ot_women_star)

# Densities of predicted scores for Women with factuals and with
# counterfactuals. The yellow dashed line corresponds to the density of
# predicted scores for Women, using factuals.
plot(
  d_unaware_factuals_women,
  main = "", xlab = "", ylab = "",
  axes = FALSE, col = NA,
  xlim = x_lim, ylim = y_lim
)
axis(1)
axis(2)
polygon(d_unaware_factuals_women, col = alpha(colours_all[["source"]], .5), border = NA)
lines(d_unaware_factuals_men, col = colours_all[["reference"]], lty = 2, lwd = 2)
polygon(d_unaware_ot_women_star, col = alpha(colours_all[["ot"]], .5), border = NA)
text(x = .2, y = 6.5, "Factuals - Women", col = colours_all[["source"]])
pos_arrow <- .07
ind_min <- which.min(abs(d_unaware_factuals_women$x - pos_arrow))
arrows(
  x1 = d_unaware_factuals_women$x[ind_min],
  y1 = d_unaware_factuals_women$y[ind_min],
  x0 = .2,
  y0 = 5,
  length = 0.05, col = colours_all[["source"]]
)

pos_arrow_ref <- .5
text(x = pos_arrow_ref, y = 6.5, "Factuals - Men", col = colours_all[["reference"]])
ind_min_ref <- which.min(abs(d_unaware_factuals_men$x - pos_arrow_ref))
arrows(
  x1 = d_unaware_factuals_men$x[ind_min_ref],
  y1 = d_unaware_factuals_men$y[ind_min_ref],
  x0 = pos_arrow_ref,
  y0 = 5,
  length = 0.05, col = colours_all[["reference"]]
)
text(x = .7, y = 6.5, "Multi. OT", col = colours_all[["ot"]])


# Naive
tb_unaware_naive <- tb_unaware |> filter(counterfactual == "naive")
# Predicted values, focusing on Black --> White
pred_unaware_naive_women_star <- tb_unaware_naive |> filter(sex == "Male") |> pull("pred")
# Estimated densities
d_unaware_naive_women_star <- density(pred_unaware_naive_women_star)
plot(
  d_unaware_factuals_women,
  main = "", xlab = "", ylab = "",
  axes = FALSE, col = NA,
  xlim = x_lim, ylim = y_lim
)
axis(1)
axis(2)
polygon(d_unaware_factuals_women, col = alpha(colours_all[["source"]], .5), border = NA)
lines(d_unaware_factuals_men, col = colours_all[["reference"]], lty = 2, lwd = 2)
polygon(d_unaware_naive_women_star, col = alpha(colours_all[["naive"]], .5), border = NA)
text(x = .7, y = 6.5, "Naive", col = colours_all[["naive"]])


# Fairadapt
tb_unaware_fpt <- tb_unaware |> filter(counterfactual == "fpt")
# Predicted values, focusing on Black --> White
pred_unaware_fpt_women_star <-
  tb_unaware_fpt |> filter(sex == "Male") |> pull("pred")
# Estimated densities
d_unaware_fpt_women_star <- density(pred_unaware_fpt_women_star)

plot(
  d_unaware_factuals_women,
  main = "", xlab = "", ylab = "",
  axes = FALSE, col = NA,
  xlim = x_lim, ylim = y_lim
)
axis(1)
axis(2)
polygon(d_unaware_factuals_women, col = alpha(colours_all[["source"]], .5), border = NA)
lines(d_unaware_factuals_men, col = colours_all[["reference"]], lty = 2, lwd = 2)
polygon(d_unaware_fpt_women_star, col = alpha(colours_all[["fairadapt"]], .5), border = NA)
text(x = .7, y = 6.5, "fairadapt", col = colours_all[["fairadapt"]])


# Sequential transport
tb_unaware_seq <- tb_unaware |> filter(counterfactual == "seq")
# Predicted values, focusing on Black --> White
pred_unaware_seq_women_star <- tb_unaware_seq |> filter(sex == "Male") |> pull("pred")
# Estimated densities
d_unaware_seq_women_star <- density(pred_unaware_seq_women_star)

plot(
  d_unaware_factuals_women,
  main = "", xlab = "", ylab = "",
  axes = FALSE, col = NA,
  xlim = x_lim, ylim = y_lim
)
axis(1)
axis(2)
polygon(d_unaware_factuals_women, col = alpha(colours_all[["source"]], .5), border = NA)
lines(d_unaware_factuals_men, col = colours_all[["reference"]], lty = 2, lwd = 2)
polygon(d_unaware_seq_women_star, col = alpha(colours_all[["seq"]], .5), border = NA)
text(x = .7, y = 6.5, "Seq. T.", col = colours_all[["seq"]])

### Aware - Figure 16, Appendix D----

tb_aware_factuals <- tb_aware |> filter(counterfactual == "none")
# Predicted values
pred_aware_factuals_women <- tb_aware_factuals |> filter(sex == "Female") |> pull("pred")
pred_aware_factuals_men <- tb_aware_factuals |> filter(sex == "Male") |> pull("pred")
# Estimated densities
d_aware_factuals_women <- density(pred_aware_factuals_women)
d_aware_factuals_men <- density(pred_aware_factuals_men)

# Densities of predicted scores for Women with factuals and with
# counterfactuals. The yellow dashed line corresponds to the density of
# predicted scores for Women, using factuals.

par(mfrow = c(4, 1), mar = c(2, 2, 1, 0))
x_lim <- c(0, .8)
y_lim <- c(0, 15)


# OT
tb_aware_ot <- tb_aware |> filter(counterfactual == "ot")
# Predicted values, focusing on Black --> White
pred_aware_ot_women_star <- tb_aware_ot |> filter(sex_origin == "Female") |> pull("pred")
# Estimated densities
d_aware_ot_women_star <- density(pred_aware_ot_women_star)

plot(
  d_aware_factuals_women,
  main = "", xlab = "", ylab = "",
  axes = FALSE, col = NA,
  xlim = x_lim, ylim = y_lim
)
axis(1)
axis(2)
polygon(d_aware_factuals_women, col = alpha(colours_all[["source"]], .5), border = NA)
lines(d_aware_factuals_men, col = colours_all[["reference"]], lty = 2, lwd = 2)
polygon(d_aware_ot_women_star, col = alpha(colours_all[["ot"]], .5), border = NA)
text(x = .2, y = 12, "Factuals - Women", col = colours_all[["source"]])
pos_arrow <- .07
ind_min <- which.min(abs(d_aware_factuals_women$x - pos_arrow))
arrows(
  x1 = d_aware_factuals_women$x[ind_min],
  y1 = d_aware_factuals_women$y[ind_min],
  x0 = .2,
  y0 = 10,
  length = 0.05, col = colours_all[["source"]]
)

pos_arrow_ref <- .5
text(x = pos_arrow_ref, y = 12, "Factuals - Men", col = colours_all[["reference"]])
ind_min_ref <- which.min(abs(d_aware_factuals_men$x - pos_arrow_ref))
arrows(
  x1 = d_aware_factuals_men$x[ind_min_ref],
  y1 = d_aware_factuals_men$y[ind_min_ref],
  x0 = pos_arrow_ref,
  y0 = 10,
  length = 0.05, col = colours_all[["reference"]]
)
text(x = .7, y = 12, "Multi. OT", col = colours_all[["ot"]])

# Naive
tb_aware_naive <- tb_aware |> filter(counterfactual == "naive")
# Predicted values, focusing on Black --> White
pred_aware_naive_women_star <- tb_aware_naive |> filter(sex == "Male") |> pull("pred")
# Estimated densities
d_aware_naive_women_star <- density(pred_aware_naive_women_star)

plot(
  d_aware_factuals_women,
  main = "", xlab = "", ylab = "",
  axes = FALSE, col = NA,
  xlim = x_lim, ylim = y_lim
)
axis(1)
axis(2)
polygon(d_aware_factuals_women, col = alpha(colours_all[["source"]], .5), border = NA)
lines(d_aware_factuals_men, col = colours_all[["reference"]], lty = 2, lwd = 2)
polygon(d_aware_naive_women_star, col = alpha(colours_all[["naive"]], .5), border = NA)
text(x = .7, y = 12, "Naive", col = colours_all[["naive"]])


# Fairadapt
tb_aware_fpt <- tb_aware |> filter(counterfactual == "fpt")
# Predicted values, focusing on Black --> White
pred_aware_fpt_women_star <-
  tb_aware_fpt |> filter(sex == "Male") |> pull("pred")
# Estimated densities
d_aware_fpt_women_star <- density(pred_aware_fpt_women_star)

plot(
  d_aware_factuals_women,
  main = "", xlab = "", ylab = "",
  axes = FALSE, col = NA,
  xlim = x_lim, ylim = y_lim
)
axis(1)
axis(2)
polygon(d_aware_factuals_women, col = alpha(colours_all[["source"]], .5), border = NA)
lines(d_aware_factuals_men, col = colours_all[["reference"]], lty = 2, lwd = 2)
polygon(d_aware_fpt_women_star, col = alpha(colours_all[["fairadapt"]], .5), border = NA)
text(x = .7, y =12, "fairadapt", col = colours_all[["fairadapt"]])


# Sequential transport
tb_aware_seq <- tb_aware |> filter(counterfactual == "seq")
# Predicted values, focusing on Black --> White
pred_aware_seq_women_star <- tb_aware_seq |> filter(sex == "Male") |> pull("pred")
# Estimated densities
d_aware_seq_women_star <- density(pred_aware_seq_women_star)

plot(
  d_aware_factuals_women,
  main = "", xlab = "", ylab = "",
  axes = FALSE, col = NA,
  xlim = x_lim, ylim = y_lim
)
axis(1)
axis(2)
polygon(d_aware_factuals_women, col = alpha(colours_all[["source"]], .5), border = NA)
lines(d_aware_factuals_men, col = colours_all[["reference"]], lty = 2, lwd = 2)
polygon(d_aware_seq_women_star, col = alpha(colours_all[["seq"]], .5), border = NA)
text(x = .7, y = 12, "Seq. T.", col = colours_all[["seq"]])

# Metrics (Table 2, Appendix D)----

threshold <- .5

# Observed target variable
obs_0 <- factuals_aware |> filter(sex_origin == "Female") |> pull("income")
obs_1 <- factuals_aware |> filter(sex_origin == "Male") |> pull("income")
# Scores using factuals
pred_0_aware <- factuals_aware |> filter(sex_origin == "Female") |> pull("pred")
pred_1_aware <- factuals_aware |> filter(sex_origin == "Male") |> pull("pred")
pred_0_unaware <- factuals_unaware |> filter(sex_origin == "Female") |> pull("pred")
pred_1_unaware <- factuals_unaware |> filter(sex_origin == "Male") |> pull("pred")
# Scores in groups S="Female" using naive counterfactuals
pred_0_naive_aware <- pred_aware_naive_women
pred_0_naive_unaware <- pred_unaware_naive_women
# Scores in groups S="Female" using multivariate optimal transport counterfactuals
pred_0_ot_aware <- pred_ot_aware
pred_0_ot_unaware <- pred_ot_unaware
# Scores in groups S="Female" using fairadapt counterfactuals
pred_0_fpt_aware <- pred_aware_fpt_women
pred_0_fpt_unaware <- pred_unaware_fpt_women
# Scores in groups S="Female" using sequential transport counterfactuals
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
