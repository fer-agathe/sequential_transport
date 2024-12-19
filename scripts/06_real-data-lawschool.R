# Real Data Example: Law School Dataset

# Source of the data:
# Wightman, Linda F. 1998. “LSAC National Longitudinal Bar Passage Study.
# LSAC Research Report Series.”
# In. https://api.semanticscholar.org/CorpusID:151073942.
#
# We use a formatted version from:
# De Lara, Lucas, Alberto González-Sanz, Nicholas Asher, Laurent Risser,
# and Jean-Michel Loubes. 2024. “Transport-Based Counterfactual Models.”
# Journal of Machine Learning Research 25 (136): 1–59.
#
# Github URL: https://github.com/lucasdelara/PI-Fair.git

# Variables:
#
# race: Race of the student (character: Amerindian, Asian, Black, Hispanic,
#       Mexican, Other, Puertorican, White).
# sex: Sex of the student (numeric: 1 female, 2 male).
# LSAT: LSAT score received by the student (numeric).
# UGPA: Undergraduate GPA of the student (numeric).
# region_first: region in which the student took their first bar examination
#               (Far West, Great Lakes, Midsouth, Midwest, Mountain West,
#               Northeast, New England, Northwest, South Central, South East)
#               (character)
# ZFYA: standardized first-year law school grades (first year average grade,
#       FYA) (numeric).
# sander_index: Sander index of the student: weighted average of normalized
#               UGPA and LSAT scores (however, no details are given for this
#               specific dataset, see Sander (2004), p. 393) (numeric)
# first_pf: Probably a binary variable that indicates whether the student
#           passed on their first trial ? No information is given about this
#           variable... (numeric 0/1).



# Law school dataset
## Required packages
library(tidyverse)
library(fairadapt)
library(devtools)
load_all("../seqtransfairness/") # load the functions from our package


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
      plot.subtitle = element_text(hjust = 0.5),
      legend.position = "bottom"
    )
}

# Seed
set.seed(2025)

colours_all <- c(
  "Factual" = "black",
  "Source" = "#00A08A",
  "Reference" = "#F2AD00",
  "Naive" = "gray",
  "OT" = "#0072B2",
  "Fairadapt" = '#D55E00',
  "Seq. T." = "#CC79A7"
)


# Data----

# This law school dataset contains information collected through a survey
# conducted from 1991 through 1997 by the Law School Admission Council across
# 163 law schools in the United States of America (Wightman (1998)).
# In total, 21,790 law students were tracked through law school, graduation,
# and sittings for bar exams.

# We use the formatted data from De Lara et al. (2024), also used by
# Lara et al. (2021)
# (Github associated to the paper: <https://github.com/lucasdelara/PI-Fair.git>)

# Each row from the raw data gives information for a student.
# The following characteristics are available:

# - `race`: Race of the student (character: Amerindian, Asian, Black, Hispanic,
#    Mexican, Other, Puertorican, White).
# - `sex`: Sex of the student (numeric: 1 female, 2 male).
# - `LSAT`: LSAT score received by the student (numeric).
# - `UGPA`: Undergraduate GPA of the student (numeric).
# - `region_first`: region in which the student took their first bar examination
#    (Far West, Great Lakes, Midsouth, Midwest, Mountain West, Northeast,
#    New England, Northwest, South Central, South East) (character)
# - `ZFYA`: standardized first-year law school grades (first year average grade,
#    FYA) (numeric).
# - `sander_index`: Sander index of the student: weighted average of normalized
#    UGPA and LSAT scores (however, no details are given for this specific
#    dataset, see Sander 2OO4, p. 393) (numeric)
# - `first_pf`: Probably a binary variable that indicates whether the student
#    passed on their first trial ? No information is given about this
#    variable... (numeric 0/1).

# References
# - De Lara, Lucas, Alberto González-Sanz, Nicholas Asher, and Jean-Michel
#   Loubes. 2021. “Transport-Based Counterfactual Models.” arXiv 2108.13025.
# - De Lara, Lucas, Alberto González-Sanz, Nicholas Asher, Laurent Risser, and
#   Jean-Michel Loubes. 2024. “Transport-Based Counterfactual Models.”
#   Journal of Machine Learning Research 25 (136): 1–59.
# - Sander, Richard H. 2004. “A Systemic Analysis of Affirmative Action in
#   American Law Schools.” Stan. L. Rev. 57: 367.
# - Wightman, Linda F. 1998. “LSAC National Longitudinal Bar Passage Study.
#   LSAC Research Report Series.”
#   In. https://api.semanticscholar.org/CorpusID:151073942.


# Pre-Processing----
df <- read_csv('../data/law_data.csv')

df <- df |>
  select(
    race, # we can take S = race (white/black)
    sex,  # or S = gender
    LSAT,
    UGPA,
    ZFYA  # Y
  )

# Table for S = race
df_race <- df |>
  select(
    race,
    UGPA,
    LSAT,
    ZFYA
  ) |>
  filter(
    race %in% c("Black", "White")
  ) |>
  rename(
    S = race,
    X1 = UGPA,
    X2 = LSAT,
    Y = ZFYA
  ) |>  # no NA values
  mutate(
    S = as.factor(S)
  )

# Table for S = gender
df_gender <- df |>
  select(
    sex,
    UGPA,
    LSAT,
    ZFYA
  ) |>
  rename(
    S = sex,
    X1 = UGPA,
    X2 = LSAT,
    Y = ZFYA
  ) |>  # no NA values
  mutate(
    S = as.factor(S)
  )

# Distribution of the standardized first-year law school grades among the two
# groups, when S is the race
ggplot(
  data = df_race,
  mapping = aes(x = Y, fill = S)
) +
  geom_histogram(
    mapping = aes(y = after_stat(density)),
    alpha = 0.5, position = "identity", binwidth = 0.5
  ) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Race",
    x = "Y",
    y = "Density"
  ) +
  global_theme()


# Distribution of the standardized first-year law school grades among the two
# groups, when S is the gender
ggplot(
  data = df_gender,
  mapping = aes(x = Y, fill = S)) +
  geom_histogram(
    mapping = aes(y = after_stat(density)),
    alpha = 0.5, position = "identity", binwidth = 0.5
  ) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Gender",
    x = "Y",
    y = "Density"
  ) +
  global_theme()

# Causal Graph----
variables <- colnames(df_race)
# Adjacency matrix: upper triangular
adj <- matrix(
  c(0, 1, 1, 1,
    0, 0, 1, 1,
    0, 0, 0, 1,
    0, 0, 0, 0),
  ncol = length(variables),
  dimnames = rep(list(variables), 2),
  byrow = TRUE
)

causal_graph <- fairadapt::graphModel(adj)
plot(causal_graph)

# Topological order
top_order <- variables
top_order

## Pre-processing----

med <- median(df_race$Y)
df_race_c <- df_race |>
  mutate(
    Y_c = ifelse(Y > med, 1, 0)
  ) |>
  select(S, X1, X2, Y = Y_c)

df_race_c$Y <- as.factor(df_race_c$Y)
levels(df_race_c$Y)


# We turn the response variable to a factor:
df_race_c$Y <- as.factor(df_race_c$Y)
levels(df_race_c$Y)

seed <- 2025
sets <- split_dataset(df_race_c, seed)
data_train <- sets$data_train
data_test <- sets$data_test

# Classifier----

## Training the Model----

# Two types of models:
# 1. aware (with the sensitive variable)
# 2. unaware (without the sensitive variable)

# Unaware logistic regression classifier (model without S)
pred_unaware <- log_reg_train(
  data_train, data_test, type = "unaware", s = "S", y = "Y"
)
pred_unaware_train <- pred_unaware$pred_train
pred_unaware_test <- pred_unaware$pred_test

# Aware logistic regression classifier (model with S)
pred_aware <- log_reg_train(
  data_train, data_test, type = "aware", s = "S", y = "Y"
)
pred_aware_train <- pred_aware$pred_train
pred_aware_test <- pred_aware$pred_test

df_test_unaware <- tibble(
  S = data_test$S,
  pred = pred_unaware_test
)

df_test_aware <- tibble(
  S = data_test$S,
  pred = pred_aware_test
)

## Unaware
# Density of predictions on the test set, for the unaware model, when the
# sensitive attribute is the race
ggplot(
  data = df_test_unaware,
  mapping = aes(x = pred, fill = S)) +
  geom_histogram(
    mapping = aes(y = after_stat(density)),
    alpha = 0.5, position = "identity", binwidth = 0.05
  ) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(
    "S", values = c(
      "Black" = colours_all[["Source"]],
      "White" = colours_all[["Reference"]]),
  ) +
  labs(
    title = "Unaware Model, with S being Race",
    x = "Predictions for Y",
    y = "Density"
  ) +
  global_theme()

# Density of predictions on the test set, for the aware model, when the
# sensitive attribute is the race
ggplot(
  data = df_test_aware,
  mapping = aes(x = pred, fill = S)) +
  geom_histogram(
    mapping = aes(y = after_stat(density)),
    alpha = 0.5, position = "identity", binwidth = 0.05
  ) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(
    "S", values = c(
      "Black" = colours_all[["Source"]],
      "White" = colours_all[["Reference"]]),
  ) +
  labs(
    title = "Aware Model, with S being Race",
    x = "Predictions for Y",
    y = "Density"
  ) +
  global_theme()

## Predictions----

# On the factuals
model_unaware <- pred_unaware$model
pred_unaware_all <- predict(
  model_unaware,
  newdata = df_race_c |> select(S, X1, X2),
  type = "response"
)
model_aware <- pred_aware$model
pred_aware_all <- predict(
  model_aware,
  newdata = df_race_c |> select(S, X1, X2),
  type = "response"
)

## Factuals----

factuals_unaware <- tibble(
  S = df_race_c$S,
  S_origin = df_race_c$S,
  X1 = df_race_c$X1,
  X2 = df_race_c$X2,
  Y = df_race_c$Y,
  pred = pred_unaware_all,
  type = "factual"
) |>
  mutate(id_indiv = row_number())

write.csv(
  factuals_unaware,
  file = "../data/factuals_unaware.csv", row.names = FALSE
)

factuals_aware <- tibble(
  S = df_race_c$S,
  S_origin = df_race_c$S,
  X1 = df_race_c$X1,
  X2 = df_race_c$X2,
  Y = df_race_c$Y,
  pred = pred_aware_all,
  type = "factual"
) |>
  mutate(id_indiv = row_number())

# Counterfactuals----

## Naive Approach----

### Predictions----
pred_unaware_naive_black <- predict(
  model_unaware,
  newdata = df_race_c |> select(S, X1, X2) |> filter(S == "Black") |> mutate(S = "White"),
  type = "response"
)

pred_aware_naive_black <- predict(
  model_aware,
  newdata = df_race_c |> select(S, X1, X2) |> filter(S == "Black") |>  mutate(S = "White"),
  type = "response"
)

### Counterfactual table----
counterfactuals_unaware_naive_black <-
  df_race_c |>
  filter(S == "Black") |>
  mutate(
    S_origin = S,
    S = "White",
    pred = pred_unaware_naive_black,
    type = "counterfactual"
  ) |>
  mutate(id_indiv = row_number())

counterfactuals_aware_naive_black <-
  df_race_c |>
  filter(S == "Black") |>
  mutate(
    S_origin = S,
    S = "White",
    pred = pred_aware_naive_black,
    type = "counterfactual"
  ) |>
  mutate(id_indiv = row_number())

### Comparison of Predicted Values----

#### Unaware Model----

unaware_naive_black <-
  factuals_unaware |> mutate(S_origin = S) |>
  bind_rows(counterfactuals_unaware_naive_black)

# Unaware model, Sensitive: Race, Black -> White
ggplot(
  unaware_naive_black |> mutate(
    group = case_when(
      S_origin == "Black" & S == "Black" ~ "Black (Original)",
      S_origin == "Black" & S == "White" ~ "Black -> White (Counterfactual)",
      S_origin == "White" & S == "White" ~ "White (Original)"
    ),
    group = factor(
      group,
      levels = c(
        "Black (Original)", "Black -> White (Counterfactual)", "White (Original)"
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
  facet_wrap(~S) +
  scale_fill_manual(
    NULL, values = c(
      "Black (Original)" = colours_all[["Source"]],
      "Black -> White (Counterfactual)" = colours_all[["Naive"]],
      "White (Original)" = colours_all[["Reference"]]
    )
  ) +
  scale_colour_manual(
    NULL, values = c(
      "Black (Original)" = colours_all[["Source"]],
      "Black -> White (Counterfactual)" = colours_all[["Naive"]],
      "White (Original)" = colours_all[["Reference"]]
    )
  ) +
  labs(
    x = "Predictions for Y",
    y = "Density"
  ) +
  global_theme() +
  theme(legend.position = "bottom")

# Distribution of Predicted Scores for Minority Class (Black), Unaware model,
# Sensitive: Race, Reference: White individuals
ggplot(
  data = unaware_naive_black |>
    mutate(
      group = case_when(
        S_origin == "Black" & S == "Black" ~ "Black (Original)",
        S_origin == "Black" & S == "White" ~ "Black -> White (Counterfactual)",
        S_origin == "White" & S == "White" ~ "White (Original)"
      ),
      group = factor(
        group,
        levels = c(
          "Black (Original)", "Black -> White (Counterfactual)", "White (Original)"
        )
      )
    ) |>
    filter(S_origin == "Black"),
  mapping = aes(x = pred, fill = group, colour = group)
) +
  geom_histogram(
    mapping = aes(y = after_stat(density)),
    alpha = 0.5, position = "identity", binwidth = 0.05
  ) +
  geom_density(alpha = 0.5, linewidth = 1) +
  scale_fill_manual(
    NULL, values = c(
      "Black (Original)" = colours_all[["Source"]],
      "Black -> White (Counterfactual)" = colours_all[["Naive"]],
      "White (Original)" = colours_all[["Reference"]]
    )
  ) +
  scale_colour_manual(
    NULL, values = c(
      "Black (Original)" = colours_all[["Source"]],
      "Black -> White (Counterfactual)" = colours_all[["Naive"]],
      "White (Original)" = colours_all[["Reference"]]
    )
  ) +
  labs(
    x = "Predictions for Y",
    y = "Density"
  ) +
  global_theme() +
  theme(legend.position = "bottom")

#### Aware Model----

aware_naive_black <-
  factuals_aware |> mutate(S_origin = S) |>
  bind_rows(counterfactuals_aware_naive_black)

# Aware model, Sensitive: Race, Black -> White
ggplot(
  aware_naive_black |> mutate(
    group = case_when(
      S_origin == "Black" & S == "Black" ~ "Black (Original)",
      S_origin == "Black" & S == "White" ~ "Black -> White (Counterfactual)",
      S_origin == "White" & S == "White" ~ "White (Original)"
    ),
    group = factor(
      group,
      levels = c(
        "Black (Original)", "Black -> White (Counterfactual)", "White (Original)"
      )
    )
  ),
  aes(x = pred, fill = group, colour = group)
) +
  geom_histogram(
    mapping = aes(y = after_stat(density)), alpha = 0.5,
    position = "identity", binwidth = 0.05
  ) +
  geom_density(alpha = 0.5, linewidth = 1) +
  scale_fill_manual(
    NULL, values = c(
      "Black (Original)" = colours_all[["Source"]],
      "Black -> White (Counterfactual)" = colours_all[["Naive"]],
      "White (Original)" = colours_all[["Reference"]]
    )
  ) +
  scale_colour_manual(
    NULL, values = c(
      "Black (Original)" = colours_all[["Source"]],
      "Black -> White (Counterfactual)" = colours_all[["Naive"]],
      "White (Original)" = colours_all[["Reference"]]
    )
  ) +
  facet_wrap(~S) +
  labs(
    x = "Predictions for Y",
    y = "Density"
  ) +
  global_theme() +
  theme(legend.position = "bottom")

# Distribution of Predicted Scores for Minority Class (Black), Aware model,
# Sensitive: Race, Black -> White

ggplot(
  data = aware_naive_black |>
    mutate(
      group = case_when(
        S_origin == "Black" & S == "Black" ~ "Black (Original)",
        S_origin == "Black" & S == "White" ~ "Black -> White (Counterfactual)",
        S_origin == "White" & S == "White" ~ "White (Original)"
      ),
      group = factor(
        group,
        levels = c(
          "Black (Original)", "Black -> White (Counterfactual)", "White (Original)"
        )
      )
    ) |>
    filter(S_origin == "Black"),
  mapping = aes(x = pred, fill = group, colour = group)
) +
  geom_histogram(
    mapping = aes(y = after_stat(density)),
    alpha = 0.5, position = "identity", binwidth = 0.05
  ) +
  geom_density(alpha = 0.5, linewidth = 1) +
  scale_fill_manual(
    NULL, values = c(
      "Black (Original)" = colours_all[["Source"]],
      "Black -> White (Counterfactual)" = colours_all[["Naive"]],
      "White (Original)" = colours_all[["Reference"]]
    )
  ) +
  scale_colour_manual(
    NULL, values = c(
      "Black (Original)" = colours_all[["Source"]],
      "Black -> White (Counterfactual)" = colours_all[["Naive"]],
      "White (Original)" = colours_all[["Reference"]]
    )
  ) +
  labs(
    x = "Predictions for Y",
    y = "Density"
  ) +
  global_theme() +
  theme(legend.position = "bottom")

## Fairadapt----

adj_wo_Y <- adj[-4,-4]
adj_wo_Y
df_race_fpt <- df_race_c |> select(S, X1, X2)
levels(df_race_fpt$S)

# Two configurations will be considered in turn:

# 1. The reference class consists of Black individuals, and FairAdapt will be
# used to obtain the counterfactual UGPA and LSAT scores for White individuals
# as if they had been Black.
#
# 2. The reference class consists of White individuals, and FairAdapt will be
# used to obtain the counterfactual UGPA and LSAT scores for Black individuals
# as if they had been White.

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

### Predictions----

ind_black <- which(df_race_c$S == "Black")
ind_white <- which(df_race_c$S == "White")

model_unaware <- pred_unaware$model
pred_unaware_fpt_black <- predict(
  model_unaware,
  newdata = adapt_df_black[ind_black, ],
  type = "response"
)
pred_unaware_fpt_white <- predict(
  model_unaware,
  newdata = adapt_df_white[ind_white, ],
  type = "response"
)

pred_aware_fpt_black <- predict(
  model_aware,
  newdata = adapt_df_black[ind_black, ],
  type = "response"
)
pred_aware_fpt_white <- predict(
  model_aware,
  newdata = adapt_df_white[ind_white, ],
  type = "response"
)

### Counterfactuals----

# Unaware model
counterfactuals_unaware_fpt_black <-
  as_tibble(adapt_df_black[ind_black, ]) |>
  mutate(
    S_origin = df_race_c$S[ind_black],
    pred = pred_unaware_fpt_black,
    type = "counterfactual",
    id_indiv = ind_black
  )

counterfactuals_unaware_fpt_white <-
  as_tibble(adapt_df_white[ind_white, ]) |>
  mutate(
    S_origin = df_race_c$S[ind_white],
    pred = pred_unaware_fpt_white,
    type = "counterfactual",
    id_indiv = ind_white
  )

# dataset with counterfactuals, for unaware model
unaware_fpt_black <-
  factuals_unaware |> mutate(S_origin = S) |>
  bind_rows(counterfactuals_unaware_fpt_black)

unaware_fpt_white <-
  factuals_unaware |> mutate(S_origin = S) |>
  bind_rows(counterfactuals_unaware_fpt_white)


# Aware model
counterfactuals_aware_fpt_black <-
  as_tibble(adapt_df_black[ind_black, ]) |>
  mutate(
    S_origin = df_race_c$S[ind_black],
    pred = pred_aware_fpt_black,
    type = "counterfactual",
    id_indiv = ind_black
  )

counterfactuals_aware_fpt_white <-
  as_tibble(adapt_df_white[ind_white, ]) |>
  mutate(
    S_origin = df_race_c$S[ind_white],
    pred = pred_aware_fpt_white,
    type = "counterfactual",
    id_indiv = ind_white
  )

# dataset with counterfactuals, for aware model
aware_fpt_black <-
  factuals_aware |> mutate(S_origin = S) |>
  bind_rows(counterfactuals_aware_fpt_black)

aware_fpt_white <-
  factuals_aware |> mutate(S_origin = S) |>
  bind_rows(counterfactuals_aware_fpt_white)

### Comparison of Predicted Values----

#### Unaware Model----
# Unaware model, Sensitive: Race, Reference: White individuals
ggplot(
  data = unaware_fpt_black |>
    mutate(
      group = case_when(
        S_origin == "Black" & S == "Black" ~ "Black (Original)",
        S_origin == "Black" & S == "White" ~ "Black -> White (Counterfactual)",
        S_origin == "White" & S == "White" ~ "White (Original)"
      ),
      group = factor(
        group,
        levels = c(
          "Black (Original)", "Black -> White (Counterfactual)", "White (Original)"
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
  facet_wrap(~S) +
  scale_fill_manual(
    NULL, values = c(
      "Black (Original)" = colours_all[["Source"]],
      "Black -> White (Counterfactual)" = colours_all[["Fairadapt"]],
      "White (Original)" = colours_all[["Reference"]]
    )
  ) +
  scale_colour_manual(
    NULL, values = c(
      "Black (Original)" = colours_all[["Source"]],
      "Black -> White (Counterfactual)" = colours_all[["Fairadapt"]],
      "White (Original)" = colours_all[["Reference"]]
    )
  ) +
  labs(
    x = "Predictions for Y",
    y = "Density"
  ) +
  global_theme() +
  theme(legend.position = "bottom")

# Unaware model, Sensitive: Race, Reference: Black individuals
ggplot(
  data = unaware_fpt_white |>
    mutate(
      group = case_when(
        S_origin == "White" & S == "White" ~ "White (Original)",
        S_origin == "White" & S == "Black" ~ "White -> Black (Counterfactual)",
        S_origin == "Black" & S == "Black" ~ "Black (Original)"
      ),
      group = factor(
        group,
        levels = c(
          "White (Original)", "White -> Black (Counterfactual)", "Black (Original)"
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
  facet_wrap(~S) +
  scale_fill_manual(
    NULL, values = c(
      "White (Original)" = colours_all[["Source"]],
      "White -> Black (Counterfactual)" = colours_all[["Fairadapt"]],
      "Black (Original)" = colours_all[["Reference"]]
    )
  ) +
  scale_colour_manual(
    NULL, values = c(
      "White (Original)" = colours_all[["Source"]],
      "White -> Black (Counterfactual)" = colours_all[["Fairadapt"]],
      "Black (Original)" = colours_all[["Reference"]]
    )
  ) +
  labs(
    x = "Predictions for Y",
    y = "Density"
  ) +
  global_theme() +
  theme(legend.position = "bottom")

# Distribution of Predicted Scores for Minority Class (Black), Unaware model,
# Sensitive: Race, Reference: White individuals

ggplot(
  data = unaware_fpt_black |>
    mutate(
      group = case_when(
        S_origin == "Black" & S == "Black" ~ "Black (Original)",
        S_origin == "Black" & S == "White" ~ "Black -> White (Counterfactual)",
        S_origin == "White" & S == "White" ~ "White (Original)"
      ),
      group = factor(
        group,
        levels = c(
          "Black (Original)", "Black -> White (Counterfactual)", "White (Original)"
        )
      )
    ) |>
    filter(S_origin == "Black"),
  mapping = aes(x = pred, fill = group, colour = group)
) +
  geom_histogram(
    mapping = aes(y = after_stat(density)),
    alpha = 0.5, position = "identity", binwidth = 0.05
  ) +
  geom_density(alpha = 0.5, linewidth = 1) +
  scale_fill_manual(
    NULL, values = c(
      "Black (Original)" = colours_all[["Source"]],
      "Black -> White (Counterfactual)" = colours_all[["Fairadapt"]],
      "White (Original)" = colours_all[["Reference"]]
    )
  ) +
  scale_colour_manual(
    NULL, values = c(
      "Black (Original)" = colours_all[["Source"]],
      "Black -> White (Counterfactual)" = colours_all[["Fairadapt"]],
      "White (Original)" = colours_all[["Reference"]]
    )
  ) +
  labs(
    x = "Predictions for Y",
    y = "Density"
  ) +
  global_theme() +
  theme(legend.position = "bottom")

# Distribution of Predicted Scores for Minority Class (White), Unaware model,
# Sensitive: Race, Reference: Black individuals

ggplot(
  data = unaware_fpt_white |>
    mutate(
      group = case_when(
        S_origin == "White" & S == "White" ~ "White (Original)",
        S_origin == "White" & S == "Black" ~ "White -> Black (Counterfactual)",
        S_origin == "Black" & S == "Black" ~ "Black (Original)"
      ),
      group = factor(
        group,
        levels = c(
          "White (Original)", "White -> Black (Counterfactual)", "Black (Original)"
        )
      )
    ) |>
    filter(S_origin == "White"),
  mapping = aes(x = pred, fill = group, colour = group)) +
  geom_histogram(
    mapping = aes(y = after_stat(density)),
    alpha = 0.5, position = "identity", binwidth = 0.05
  ) +
  geom_density(alpha = 0.5, linewidth = 1) +
  scale_fill_manual(
    NULL, values = c(
      "White (Original)" = colours_all[["Source"]],
      "White -> Black (Counterfactual)" = colours_all[["Fairadapt"]]
    )
  ) +
  scale_colour_manual(
    NULL, values = c(
      "White (Original)" = colours_all[["Source"]],
      "White -> Black (Counterfactual)" = colours_all[["Fairadapt"]]
    )
  ) +
  labs(
    x = "Predictions for Y",
    y = "Density"
  ) +
  global_theme() +
  theme(legend.position = "bottom")

#### Aware Model----

# Aware model, Sensitive: Race, Reference: White individuals
ggplot(
  data = aware_fpt_black |>
    mutate(
      group = case_when(
        S_origin == "Black" & S == "Black" ~ "Black (Original)",
        S_origin == "Black" & S == "White" ~ "Black -> White (Counterfactual)",
        S_origin == "White" & S == "White" ~ "White (Original)"
      ),
      group = factor(
        group,
        levels = c(
          "Black (Original)", "Black -> White (Counterfactual)", "White (Original)"
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
  facet_wrap(~S) +
  scale_fill_manual(
    NULL, values = c(
      "Black (Original)" = colours_all[["Source"]],
      "Black -> White (Counterfactual)" = colours_all[["Fairadapt"]],
      "White (Original)" = colours_all[["Reference"]]
    )
  ) +
  scale_colour_manual(
    NULL, values = c(
      "Black (Original)" = colours_all[["Source"]],
      "Black -> White (Counterfactual)" = colours_all[["Fairadapt"]],
      "White (Original)" = colours_all[["Reference"]]
    )
  ) +
  labs(
    x = "Predictions for Y",
    y = "Density"
  ) +
  global_theme() +
  theme(legend.position = "bottom")

# Aware model, Sensitive: Race, Reference: Black individuals
ggplot(
  data = aware_fpt_white |>
    mutate(
      group = case_when(
        S_origin == "White" & S == "White" ~ "White (Original)",
        S_origin == "White" & S == "Black" ~ "White -> Black (Counterfactual)",
        S_origin == "Black" & S == "Black" ~ "Black (Original)"
      ),
      group = factor(
        group,
        levels = c(
          "White (Original)", "White -> Black (Counterfactual)", "Black (Original)"
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
  facet_wrap(~S) +
  scale_fill_manual(
    NULL, values = c(
      "White (Original)" = colours_all[["Source"]],
      "White -> Black (Counterfactual)" = colours_all[["Fairadapt"]],
      "Black (Original)" = colours_all[["Reference"]]
    )
  ) +
  scale_colour_manual(
    NULL, values = c(
      "White (Original)" = colours_all[["Source"]],
      "White -> Black (Counterfactual)" = colours_all[["Fairadapt"]],
      "Black (Original)" = colours_all[["Reference"]]
    )
  ) +
  labs(
    x = "Predictions for Y",
    y = "Density"
  ) +
  global_theme() +
  theme(legend.position = "bottom")

# Distribution of Predicted Scores for Minority Class (Black), Aware model,
# Sensitive: Race, Reference: White individuals
ggplot(
  data = aware_fpt_black |>
    mutate(
      group = case_when(
        S_origin == "Black" & S == "Black" ~ "Black (Original)",
        S_origin == "Black" & S == "White" ~ "Black -> White (Counterfactual)",
        S_origin == "White" & S == "White" ~ "White (Original)"
      ),
      group = factor(
        group,
        levels = c(
          "Black (Original)", "Black -> White (Counterfactual)", "White (Original)"
        )
      )
    ) |>
    filter(S_origin == "Black"),
  mapping = aes(x = pred, fill = group, colour = group)
) +
  geom_histogram(
    mapping = aes(y = after_stat(density)),
    alpha = 0.5, position = "identity", binwidth = 0.05
  ) +
  geom_density(alpha = 0.5, linewidth = 1) +
  scale_fill_manual(
    NULL, values = c(
      "Black (Original)" = colours_all[["Source"]],
      "Black -> White (Counterfactual)" = colours_all[["Fairadapt"]],
      "White (Original)" = colours_all[["Reference"]]
    )
  ) +
  scale_colour_manual(
    NULL, values = c(
      "Black (Original)" = colours_all[["Source"]],
      "Black -> White (Counterfactual)" = colours_all[["Fairadapt"]],
      "White (Original)" = colours_all[["Reference"]]
    )
  ) +
  labs(
    x = "Predictions for Y",
    y = "Density"
  ) +
  global_theme() +
  theme(legend.position = "bottom")

# Distribution of Predicted Scores for Minority Class (White), Aware model,
# Sensitive: Race, Reference: Black individuals
ggplot(
  data = aware_fpt_white |>
    mutate(
      group = case_when(
        S_origin == "White" & S == "White" ~ "White (Original)",
        S_origin == "White" & S == "Black" ~ "White -> Black (Counterfactual)",
        S_origin == "Black" & S == "Black" ~ "Black (Original)"
      ),
      group = factor(
        group,
        levels = c(
          "White (Original)", "White -> Black (Counterfactual)", "Black (Original)"
        )
      )
    ) |>
    filter(S_origin == "White"),
  mapping = aes(x = pred, fill = group, colour = group)) +
  geom_histogram(
    mapping = aes(y = after_stat(density)),
    alpha = 0.5, position = "identity", binwidth = 0.05
  ) +
  geom_density(alpha = 0.5, linewidth = 1) +
  scale_fill_manual(
    NULL, values = c(
      "White (Original)" = colours_all[["Source"]],
      "White -> Black (Counterfactual)" = colours_all[["Fairadapt"]]
    )
  ) +
  scale_colour_manual(
    NULL, values = c(
      "White (Original)" = colours_all[["Source"]],
      "White -> Black (Counterfactual)" = colours_all[["Fairadapt"]]
    )
  ) +
  labs(
    x = "Predictions for Y",
    y = "Density"
  ) +
  global_theme() +
  theme(legend.position = "bottom")

## Multivariate OT----

library(reticulate)
use_virtualenv("~/quarto-python-env", required = TRUE)
# reticulate::install_miniconda(force = TRUE)
# py_install("POT")
# py_install("pandas")
# py_install("numpy")
# py_install("matplotlib")

# Run python script
py_run_file("06_real-data-lawschool-ot.py")

# Load results
counterfactuals_ot <- read_csv('../data/counterfactuals_ot.csv') |>
  mutate(id_indiv = row_number())

# We add the sensitive attribute to the dataset (Black individuals become
# White, and conversely):
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
  mutate(
    S_origin = df_race_c$S,
    S = S_star
  )

counterfactuals_ot_black <-
  counterfactuals_ot |> filter(S_origin == "Black") |>
  bind_rows(
    df_race_c |> select(-Y) |>
      mutate(
        id_indiv = row_number(),
        S_origin = S,
      ) |>
      filter(S == "White")
  ) |>
  arrange(id_indiv)

counterfactuals_ot_white <-
  counterfactuals_ot |> filter(S_origin == "White") |>
  bind_rows(
    df_race_c |> select(-Y) |>
      mutate(
        id_indiv = row_number(),
        S_origin = S,
      ) |>
      filter(S == "Black")
  ) |>
  arrange(id_indiv)

### Predictions----

# With source group Black, unaware model
pred_unaware_ot_black <- predict(
  model_unaware, newdata = counterfactuals_ot_black, type = "response"
)
counterfactuals_unaware_ot_black <- counterfactuals_ot_black |>
  mutate(pred = pred_unaware_ot_black, type = "counterfactual")

# With source group White, unaware model
pred_unaware_ot_white <- predict(
  model_unaware, newdata = counterfactuals_ot_white, type = "response"
)
counterfactuals_unaware_ot_white <- counterfactuals_ot_white |>
  mutate(pred = pred_unaware_ot_white, type = "counterfactual")

# With source group Black, aware model
model_aware <- pred_aware$model
pred_aware_ot_black <- predict(
  model_aware, newdata = counterfactuals_ot_black, type = "response"
)
counterfactuals_aware_ot_black <- counterfactuals_ot_black |>
  mutate(pred = pred_aware_ot_black, type = "counterfactual")

# With source group White, aware model
pred_aware_ot_white <- predict(
  model_aware, newdata = counterfactuals_ot_white, type = "response"
)
counterfactuals_aware_ot_white <- counterfactuals_ot_white |>
  mutate(pred = pred_aware_ot_white, type = "counterfactual")

### Counterfactual table----

#### Unaware Model----

unaware_ot_black <-
  factuals_unaware |> mutate(S_origin = S) |>
  bind_rows(counterfactuals_unaware_ot_black)

unaware_ot_white <-
  factuals_unaware |> mutate(S_origin = S) |>
  bind_rows(counterfactuals_unaware_ot_white)

# Unaware model, Sensitive: Race, Reference: White individuals
ggplot(
  data = unaware_ot_black |>
    mutate(
      group = case_when(
        S_origin == "Black" & S == "Black" ~ "Black (Original)",
        S_origin == "Black" & S == "White" ~ "Black -> White (Counterfactual)",
        S_origin == "White" & S == "White" ~ "White (Original)"
      ),
      group = factor(
        group,
        levels = c(
          "Black (Original)", "Black -> White (Counterfactual)", "White (Original)"
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
  facet_wrap(~S) +
  scale_fill_manual(
    NULL, values = c(
      "Black (Original)" = colours_all[["Source"]],
      "Black -> White (Counterfactual)" = colours_all[["OT"]],
      "White (Original)" = colours_all[["Reference"]]
    )
  ) +
  scale_colour_manual(
    NULL, values = c(
      "Black (Original)" = colours_all[["Source"]],
      "Black -> White (Counterfactual)" = colours_all[["OT"]],
      "White (Original)" = colours_all[["Reference"]]
    )
  ) +
  labs(
    title = "Unaware model, Sensitive: Race, Reference: White individuals",
    x = "Predictions for Y",
    y = "Density"
  ) +
  global_theme() +
  theme(legend.position = "bottom")

# Unaware model, Sensitive: Race, Reference: Black individuals
ggplot(
  data = unaware_ot_white |>
    mutate(
      group = case_when(
        S_origin == "White" & S == "White" ~ "White (Original)",
        S_origin == "White" & S == "Black" ~ "White -> Black (Counterfactual)",
        S_origin == "Black" & S == "Black" ~ "Black (Original)"
      ),
      group = factor(
        group,
        levels = c(
          "White (Original)", "White -> Black (Counterfactual)", "Black (Original)"
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
  facet_wrap(~S) +
  scale_fill_manual(
    NULL, values = c(
      "White (Original)" = colours_all[["Source"]],
      "White -> Black (Counterfactual)" = colours_all[["OT"]],
      "Black (Original)" = colours_all[["Reference"]]
    )
  ) +
  scale_colour_manual(
    NULL, values = c(
      "White (Original)" = colours_all[["Source"]],
      "White -> Black (Counterfactual)" = colours_all[["OT"]],
      "Black (Original)" = colours_all[["Reference"]]
    )
  ) +
  labs(
    title = "Unaware model, Sensitive: Race, Reference: Black individuals",
    x = "Predictions for Y",
    y = "Density"
  ) +
  global_theme() +
  theme(legend.position = "bottom")


# Distribution of Predicted Scores for Minority Class (Black), Unaware model,
# Sensitive: Race, Reference: White individuals
ggplot(
  data = unaware_ot_black |>
    mutate(
      group = case_when(
        S_origin == "Black" & S == "Black" ~ "Black (Original)",
        S_origin == "Black" & S == "White" ~ "Black -> White (Counterfactual)",
        S_origin == "White" & S == "White" ~ "White (Original)"
      ),
      group = factor(
        group,
        levels = c(
          "Black (Original)", "Black -> White (Counterfactual)", "White (Original)"
        )
      )
    ) |>
    filter(S_origin == "Black"),
  mapping = aes(x = pred, fill = group, colour = group)
) +
  geom_histogram(
    mapping = aes(y = after_stat(density)),
    alpha = 0.5, position = "identity", binwidth = 0.05
  ) +
  geom_density(alpha = 0.5, linewidth = 1) +
  scale_fill_manual(
    NULL, values = c(
      "Black (Original)" = colours_all[["Source"]],
      "Black -> White (Counterfactual)" = colours_all[["OT"]],
      "White (Original)" = colours_all[["Reference"]]
    )
  ) +
  scale_colour_manual(
    NULL, values = c(
      "Black (Original)" = colours_all[["Source"]],
      "Black -> White (Counterfactual)" = colours_all[["OT"]],
      "White (Original)" = colours_all[["Reference"]]
    )
  ) +
  labs(
    title = "Predicted Scores for Minority Class\n Unware model, Sensitive: Race, Reference: White individuals",
    x = "Predictions for Y",
    y = "Density"
  ) +
  global_theme()

# Distribution of Predicted Scores for Minority Class (White), Unaware model,
# Sensitive: Race, Reference: Black individuals
ggplot(
  data = unaware_ot_white |>
    mutate(
      group = case_when(
        S_origin == "White" & S == "White" ~ "White (Original)",
        S_origin == "White" & S == "Black" ~ "White -> Black (Counterfactual)",
        S_origin == "Black" & S == "Black" ~ "Black (Original)"
      ),
      group = factor(
        group,
        levels = c(
          "White (Original)", "White -> Black (Counterfactual)", "Black (Original)"
        )
      )
    ) |>
    filter(S_origin == "White"),
  mapping = aes(x = pred, fill = group, colour = group)) +
  geom_histogram(
    mapping = aes(y = after_stat(density)),
    alpha = 0.5, position = "identity", binwidth = 0.05
  ) +
  geom_density(alpha = 0.5, linewidth = 1) +
  scale_fill_manual(
    NULL, values = c(
      "White (Original)" = colours_all[["Source"]],
      "White -> Black (Counterfactual)" = colours_all[["OT"]]
    )
  ) +
  scale_colour_manual(
    NULL, values = c(
      "White (Original)" = colours_all[["Source"]],
      "White -> Black (Counterfactual)" = colours_all[["OT"]]
    )
  ) +
  labs(
    title = "Predicted Scores for Minority Class\n Unware model, Sensitive: Race, Reference: Black individuals",
    x = "Predictions for Y",
    y = "Density"
  ) +
  global_theme()

#### Aware Model----

aware_ot_black <-
  factuals_unaware |> mutate(S_origin = S) |>
  bind_rows(counterfactuals_aware_ot_black)

aware_ot_white <-
  factuals_unaware |> mutate(S_origin = S) |>
  bind_rows(counterfactuals_aware_ot_white)

# Aware model, Sensitive: Race, Reference: White individuals
ggplot(
  data = aware_ot_black |>
    mutate(
      group = case_when(
        S_origin == "Black" & S == "Black" ~ "Black (Original)",
        S_origin == "Black" & S == "White" ~ "Black -> White (Counterfactual)",
        S_origin == "White" & S == "White" ~ "White (Original)"
      ),
      group = factor(
        group,
        levels = c(
          "Black (Original)", "Black -> White (Counterfactual)", "White (Original)"
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
  facet_wrap(~S) +
  scale_fill_manual(
    NULL, values = c(
      "Black (Original)" = colours_all[["Source"]],
      "Black -> White (Counterfactual)" = colours_all[["OT"]],
      "White (Original)" = colours_all[["Reference"]]
    )
  ) +
  scale_colour_manual(
    NULL, values = c(
      "Black (Original)" = colours_all[["Source"]],
      "Black -> White (Counterfactual)" = colours_all[["OT"]],
      "White (Original)" = colours_all[["Reference"]]
    )
  ) +
  labs(
    title = "Aware model, Sensitive: Race, Reference: White individuals",
    x = "Predictions for Y",
    y = "Density"
  ) +
  global_theme() +
  theme(legend.position = "bottom")

# Aware model, Sensitive: Race, Reference: Black individuals
ggplot(
  data = aware_ot_white |>
    mutate(
      group = case_when(
        S_origin == "White" & S == "White" ~ "White (Original)",
        S_origin == "White" & S == "Black" ~ "White -> Black (Counterfactual)",
        S_origin == "Black" & S == "Black" ~ "Black (Original)"
      ),
      group = factor(
        group,
        levels = c(
          "White (Original)", "White -> Black (Counterfactual)", "Black (Original)"
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
  facet_wrap(~S) +
  scale_fill_manual(
    NULL, values = c(
      "White (Original)" = colours_all[["Source"]],
      "White -> Black (Counterfactual)" = colours_all[["OT"]],
      "Black (Original)" = colours_all[["Reference"]]
    )
  ) +
  scale_colour_manual(
    NULL, values = c(
      "White (Original)" = colours_all[["Source"]],
      "White -> Black (Counterfactual)" = colours_all[["OT"]],
      "Black (Original)" = colours_all[["Reference"]]
    )
  ) +
  labs(
    title = "Aware model, Sensitive: Race, Reference: Black individuals",
    x = "Predictions for Y",
    y = "Density"
  ) +
  global_theme() +
  theme(legend.position = "bottom")

# Distribution of Predicted Scores for Minority Class (Black), Aware model,
# Sensitive: Race, Reference: White individuals
ggplot(
  data = aware_ot_black |>
    mutate(
      group = case_when(
        S_origin == "Black" & S == "Black" ~ "Black (Original)",
        S_origin == "Black" & S == "White" ~ "Black -> White (Counterfactual)",
        S_origin == "White" & S == "White" ~ "White (Original)"
      ),
      group = factor(
        group,
        levels = c(
          "Black (Original)", "Black -> White (Counterfactual)", "White (Original)"
        )
      )
    ) |>
    filter(S_origin == "Black"),
  mapping = aes(x = pred, fill = group, colour = group)
) +
  geom_histogram(
    mapping = aes(y = after_stat(density)),
    alpha = 0.5, position = "identity", binwidth = 0.05
  ) +
  geom_density(alpha = 0.5, linewidth = 1) +
  scale_fill_manual(
    NULL, values = c(
      "Black (Original)" = colours_all[["Source"]],
      "Black -> White (Counterfactual)" = colours_all[["OT"]],
      "White (Original)" = colours_all[["Reference"]]
    )
  ) +
  scale_colour_manual(
    NULL, values = c(
      "Black (Original)" = colours_all[["Source"]],
      "Black -> White (Counterfactual)" = colours_all[["OT"]],
      "White (Original)" = colours_all[["Reference"]]
    )
  ) +
  labs(
    title = "Predicted Scores for Minority Class\n Unware model, Sensitive: Race, Reference: White individuals",
    x = "Predictions for Y",
    y = "Density"
  ) +
  global_theme()

# Distribution of Predicted Scores for Minority Class (White), Aware model,
# Sensitive: Race, Reference: Black individuals
ggplot(
  data = aware_ot_white |>
    mutate(
      group = case_when(
        S_origin == "White" & S == "White" ~ "White (Original)",
        S_origin == "White" & S == "Black" ~ "White -> Black (Counterfactual)",
        S_origin == "Black" & S == "Black" ~ "Black (Original)"
      ),
      group = factor(
        group,
        levels = c(
          "White (Original)", "White -> Black (Counterfactual)", "Black (Original)"
        )
      )
    ) |>
    filter(S_origin == "White"),
  mapping = aes(x = pred, fill = group, colour = group)) +
  geom_histogram(
    mapping = aes(y = after_stat(density)),
    alpha = 0.5, position = "identity", binwidth = 0.05
  ) +
  geom_density(alpha = 0.5, linewidth = 1) +
  scale_fill_manual(
    NULL, values = c(
      "White (Original)" = colours_all[["Source"]],
      "White -> Black (Counterfactual)" = colours_all[["OT"]]
    )
  ) +
  scale_colour_manual(
    NULL, values = c(
      "White (Original)" = colours_all[["Source"]],
      "White -> Black (Counterfactual)" = colours_all[["OT"]]
    )
  ) +
  labs(
    title = "Predicted Scores for Minority Class\n Unware model, Sensitive: Race, Reference: Black individuals",
    x = "Predictions for Y",
    y = "Density"
  ) +
  global_theme()

## Sequential Transport----

df_race_c_light <- df_race_c |> select(S, X1, X2)
ind_white <- which(df_race_c_light$S == "White")
ind_black <- which(df_race_c_light$S == "Black")

variables <- c("S", "X1", "X2", "Y")
# X1, then X2
adj_1 <- matrix(
  # S  X1 X2 Y
  c(0, 1, 1, 1,# S
    0, 0, 1, 1,# X1
    0, 0, 0, 1,# X2
    0, 0, 0, 0  # Y
  ),
  ncol = length(variables),
  dimnames = rep(list(variables), 2),
  byrow = TRUE
)

trans_x1_then_x2 <- seq_trans(
  data = df_race_c, adj = adj_1, s = "S", S_0 = "Black", y = "Y"
)

a10 <- df_race_c_light$X1[ind_black]
a20 <- df_race_c_light$X2[ind_black]
# Transported values:
x1_star <- trans_x1_then_x2$transported$X1 # Transport X1 to group S=White
x2_star <- trans_x1_then_x2$transported$X2 # Transport X2|X1 to group S=White

### Counterfactuals----

df_counterfactuals_seq_black <-
  df_race_c_light |> mutate(id_indiv= row_number()) |>
  filter(S == "Black") |>
  mutate(
    S_origin = "Black",
    S = "White",
    X1 = x1_star,
    X2 = x2_star
  )

### Predictions----

pred_seq_unaware <- predict(
  model_unaware, newdata = df_counterfactuals_seq_black,type = "response"
)

pred_seq_aware <- predict(
  model_aware, newdata = df_counterfactuals_seq_black,type = "response"
)

counterfactuals_unaware_seq_black <-
  df_counterfactuals_seq_black |>
  mutate(pred = pred_seq_unaware, type = "counterfactual")

counterfactuals_aware_seq_black <-
  df_counterfactuals_seq_black |>
  mutate(pred = pred_seq_aware, type = "counterfactual")

### Comparison of Predicted Values----

aware_seq_black <- bind_rows(
  factuals_aware |> mutate(id_indiv = row_number(), S_origin = S),
  counterfactuals_aware_seq_black |> mutate(S_origin = "Black")
)
unaware_seq_black <- bind_rows(
  factuals_unaware |> mutate(id_indiv = row_number(), S_origin = S),
  counterfactuals_unaware_seq_black |> mutate(S_origin = "Black")
)

# Unaware model, Sensitive: Race, Black -> White
ggplot(
  data = unaware_seq_black |>
    mutate(
      group = case_when(
        S_origin == "Black" & S == "Black" ~ "Black (Original)",
        S_origin == "Black" & S == "White" ~ "Black -> White (Counterfactual)",
        S_origin == "White" & S == "White" ~ "White (Original)"
      ),
      group = factor(
        group,
        levels = c(
          "Black (Original)", "Black -> White (Counterfactual)", "White (Original)"
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
  facet_wrap(~S) +
  scale_fill_manual(
    NULL, values = c(
      "Black (Original)" = colours_all[["Source"]],
      "Black -> White (Counterfactual)" = colours_all[["Seq. T."]],
      "White (Original)" = colours_all[["Reference"]]
    )
  ) +
  scale_colour_manual(
    NULL, values = c(
      "Black (Original)" = colours_all[["Source"]],
      "Black -> White (Counterfactual)" = colours_all[["Seq. T."]],
      "White (Original)" = colours_all[["Reference"]]
    )
  ) +
  labs(
    x = "Predictions for Y",
    y = "Density"
  ) +
  global_theme() +
  theme(legend.position = "bottom")


# Aware model, Sensitive: Race, Black -> White
ggplot(
  data = aware_seq_black |>
    mutate(
      group = case_when(
        S_origin == "Black" & S == "Black" ~ "Black (Original)",
        S_origin == "Black" & S == "White" ~ "Black -> White (Counterfactual)",
        S_origin == "White" & S == "White" ~ "White (Original)"
      ),
      group = factor(
        group,
        levels = c(
          "Black (Original)", "Black -> White (Counterfactual)", "White (Original)"
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
  facet_wrap(~S) +
  scale_fill_manual(
    NULL, values = c(
      "Black (Original)" = colours_all[["Source"]],
      "Black -> White (Counterfactual)" = colours_all[["Seq. T."]],
      "White (Original)" = colours_all[["Reference"]]
    )
  ) +
  scale_colour_manual(
    NULL, values = c(
      "Black (Original)" = colours_all[["Source"]],
      "Black -> White (Counterfactual)" = colours_all[["Seq. T."]],
      "White (Original)" = colours_all[["Reference"]]
    )
  ) +
  labs(
    x = "Predictions for Y",
    y = "Density"
  ) +
  global_theme() +
  theme(legend.position = "bottom")


# Distribution of Predicted Scores for Minority Class (Black), Unaware model,
# Sensitive: Race, Black -> White
ggplot(
  data = unaware_seq_black |>
    mutate(
      group = case_when(
        S_origin == "Black" & S == "Black" ~ "Black (Original)",
        S_origin == "Black" & S == "White" ~ "Black -> White (Counterfactual)",
        S_origin == "White" & S == "White" ~ "White (Original)"
      ),
      group = factor(
        group,
        levels = c(
          "Black (Original)", "Black -> White (Counterfactual)", "White (Original)"
        )
      )
    ) |>
    filter(S_origin == "Black"),
  mapping = aes(x = pred, fill = group, colour = group)
) +
  geom_histogram(
    mapping = aes(y = after_stat(density)),
    alpha = 0.5, position = "identity", binwidth = 0.05
  ) +
  geom_density(alpha = 0.5, linewidth = 1) +
  scale_fill_manual(
    NULL, values = c(
      "Black (Original)" = colours_all[["Source"]],
      "Black -> White (Counterfactual)" = colours_all[["Seq. T."]],
      "White (Original)" = colours_all[["Reference"]]
    )
  ) +
  scale_colour_manual(
    NULL, values = c(
      "Black (Original)" = colours_all[["Source"]],
      "Black -> White (Counterfactual)" = colours_all[["Seq. T."]],
      "White (Original)" = colours_all[["Reference"]]
    )
  ) +
  labs(
    x = "Predictions for Y",
    y = "Density"
  ) +
  global_theme() +
  theme(legend.position = "bottom")

# Distribution of Predicted Scores for Minority Class (Black), Aware model,
# Sensitive: Race, Black -> White
ggplot(
  data = aware_seq_black |>
    mutate(
      group = case_when(
        S_origin == "Black" & S == "Black" ~ "Black (Original)",
        S_origin == "Black" & S == "White" ~ "Black -> White (Counterfactual)",
        S_origin == "White" & S == "White" ~ "White (Original)"
      ),
      group = factor(
        group,
        levels = c(
          "Black (Original)", "Black -> White (Counterfactual)", "White (Original)"
        )
      )
    ) |>
    filter(S_origin == "Black"),
  mapping = aes(x = pred, fill = group, colour = group)
) +
  geom_histogram(
    mapping = aes(y = after_stat(density)),
    alpha = 0.5, position = "identity", binwidth = 0.05
  ) +
  geom_density(alpha = 0.5, linewidth = 1) +
  scale_fill_manual(
    NULL, values = c(
      "Black (Original)" = colours_all[["Source"]],
      "Black -> White (Counterfactual)" = colours_all[["Seq. T."]],
      "White (Original)" = colours_all[["Reference"]]
    )
  ) +
  scale_colour_manual(
    NULL, values = c(
      "Black (Original)" = colours_all[["Source"]],
      "Black -> White (Counterfactual)" = colours_all[["Seq. T."]],
      "White (Original)" = colours_all[["Reference"]]
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
  bind_rows(counterfactuals_unaware_naive_black |> mutate(counterfactual = "naive")) |>
  # Multivariate optimal transport
  bind_rows(counterfactuals_unaware_ot_black |> mutate(counterfactual = "ot")) |>
  # Fairadapt
  bind_rows(counterfactuals_unaware_fpt_black |> mutate(counterfactual = "fpt")) |>
  # Sequential transport
  bind_rows(counterfactuals_unaware_seq_black |> mutate(counterfactual = "seq"))

tb_indiv_unaware <-
  tb_unaware |>
  filter(id_indiv %in% counterfactuals_unaware_seq_black$id_indiv[1:3])

tb_indiv_unaware


tb_aware <-
  factuals_aware |> mutate(counterfactual = "none") |>
  # Naive
  bind_rows(counterfactuals_aware_naive_black |> mutate(counterfactual = "naive")) |>
  # Multivariate optimal transport
  bind_rows(counterfactuals_aware_ot_black |> mutate(counterfactual = "ot")) |>
  # Fairadapt
  bind_rows(counterfactuals_aware_fpt_black |> mutate(counterfactual = "fpt")) |>
  # Sequential transport
  bind_rows(counterfactuals_aware_seq_black |> mutate(counterfactual = "seq"))

tb_indiv_aware <-
  tb_aware |>
  filter(id_indiv %in% counterfactuals_aware_seq_black$id_indiv[1:3])

tb_indiv_aware

## Three Individuals----

### Figure with unaware model----
# Predictions by the unaware model for three Black individuals.

par(mar = c(2, 2, 0, 0))
# Initial characteristics with the unaware model
tb_indiv_unaware_factual <-
  tb_indiv_unaware |> filter(type == "factual")

range_x1 <- range(tb_indiv_unaware$X1)
expansion_amount_x1 <- .1*range_x1
range_x2 <- range(tb_indiv_unaware$X2)
expansion_amount_x2 <- .05*range_x2



plot(
  x = tb_indiv_unaware_factual$X1,
  y = tb_indiv_unaware_factual$X2,
  col = colours_all[["Factual"]],
  # xlab = "X1 (UGPA)", ylab = "X2 (LSAT)",
  xlab = "", ylab = "",
  xlim = c(range_x1[1] - expansion_amount_x1[1], range_x1[2] + expansion_amount_x1[2]),
  ylim = c(range_x2[1] - expansion_amount_x2[1], range_x2[2] + expansion_amount_x2[2]),
  pch = 19,
  axes = FALSE
)
axis(1)
mtext(expression(X[1]~(UGCA)), side = 1, padj = .5)
axis(2)
mtext(expression(X[2]~(LSAT)), side = 2, padj = 0)
text(
  x = tb_indiv_unaware_factual$X1,
  y = tb_indiv_unaware_factual$X2 + 1,
  paste0(round(100*tb_indiv_unaware_factual$pred, 2), "%"),
  col = colours_all[["Factual"]]
)
# Transported characteristics with fairadapt
tb_indiv_unaware_fpt <-
  tb_indiv_unaware |> filter(counterfactual == "fpt")
points(
  x = tb_indiv_unaware_fpt$X1,
  y = tb_indiv_unaware_fpt$X2,
  col = colours_all[["Fairadapt"]],
  xlab = "X1", ylab = "X2",
  pch = 19
)
# x1 then x2
segments(
  x0 = tb_indiv_unaware_factual$X1,
  y0 = tb_indiv_unaware_factual$X2,
  x1 = tb_indiv_unaware_fpt$X1,
  y1 = tb_indiv_unaware_factual$X2,
  col = colours_all[["Fairadapt"]],
  lty = 2
)
segments(
  x0 = tb_indiv_unaware_fpt$X1,
  y0 = tb_indiv_unaware_factual$X2,
  x1 = tb_indiv_unaware_fpt$X1,
  y1 = tb_indiv_unaware_fpt$X2,
  col = colours_all[["Fairadapt"]],
  lty = 2
)
text(
  x = tb_indiv_unaware_fpt$X1,
  y = tb_indiv_unaware_fpt$X2 + 1,
  paste0(round(100*tb_indiv_unaware_fpt$pred, 2), "%"),
  col = colours_all[["Fairadapt"]]
)

# Naive
tb_indiv_unaware_naive <-
  tb_indiv_unaware |> filter(counterfactual == "naive")
points(
  x = tb_indiv_unaware_naive$X1,
  y = tb_indiv_unaware_naive$X2,
  col = colours_all[["Naive"]],
  xlab = "X1", ylab = "X2",
  pch = 19
)
segments(
  x0 = tb_indiv_unaware_factual$X1,
  y0 = tb_indiv_unaware_factual$X2,
  x1 = tb_indiv_unaware_naive$X1,
  y1 = tb_indiv_unaware_naive$X2,
  col = colours_all[["Naive"]],
  lty = 2
)
text(
  x = tb_indiv_unaware_naive$X1 + .1,
  y = tb_indiv_unaware_naive$X2,
  paste0(round(100*tb_indiv_unaware_naive$pred, 2), "%"),
  col = colours_all[["Naive"]]
)

# Transported characteristics with OT
tb_indiv_unaware_ot <-
  tb_indiv_unaware |> filter(counterfactual == "ot")
points(
  x = tb_indiv_unaware_ot$X1,
  y = tb_indiv_unaware_ot$X2,
  col = colours_all[["OT"]],
  xlab = "X1", ylab = "X2",
  pch = 19
)
segments(
  x0 = tb_indiv_unaware_factual$X1,
  y0 = tb_indiv_unaware_factual$X2,
  x1 = tb_indiv_unaware_ot$X1,
  y1 = tb_indiv_unaware_ot$X2,
  col = colours_all[["OT"]],
  lty = 2
)
text(
  x = tb_indiv_unaware_ot$X1 - .15,
  y = tb_indiv_unaware_ot$X2,
  paste0(round(100*tb_indiv_unaware_ot$pred, 2), "%"),
  col = colours_all[["OT"]]
)

# Transported characteristics with Sequential transport
tb_indiv_unaware_seq <-
  tb_indiv_unaware |> filter(counterfactual == "seq")
points(
  x = tb_indiv_unaware_seq$X1,
  y = tb_indiv_unaware_seq$X2,
  col = colours_all[["Seq. T."]],
  xlab = "X1", ylab = "X2",
  pch = 19
)
# x1 then x2
segments(
  x0 = tb_indiv_unaware_factual$X1,
  y0 = tb_indiv_unaware_factual$X2,
  x1 = tb_indiv_unaware_seq$X1,
  y1 = tb_indiv_unaware_factual$X2,
  col = colours_all[["Seq. T."]],
  lty = 2
)
segments(
  x0 = tb_indiv_unaware_seq$X1,
  y0 = tb_indiv_unaware_factual$X2,
  x1 = tb_indiv_unaware_seq$X1,
  y1 = tb_indiv_unaware_seq$X2,
  col = colours_all[["Seq. T."]],
  lty = 2
)
text(
  x = tb_indiv_unaware_seq$X1 + .11,
  y = tb_indiv_unaware_seq$X2 - .5,
  paste0(round(100*tb_indiv_unaware_seq$pred, 2), "%"),
  col = colours_all[["Seq. T."]]
)
legend(
  "topleft",
  pch = 19, col = colours_all[c("Factual", "Naive", "OT", "Fairadapt", "Seq. T.")],
  legend = names(colours_all[c("Factual", "Naive", "OT", "Fairadapt", "Seq. T.")]),
  box.lty=0
)

### Figure 8 (left), with aware model----

par(mar = c(2, 2, 0, 0))
# Initial characteristics with the aware model
tb_indiv_aware_factual <-
  tb_indiv_aware |> filter(type == "factual")

range_x1 <- range(tb_indiv_aware$X1)
expansion_amount_x1 <- .1*range_x1
range_x2 <- range(tb_indiv_aware$X2)
expansion_amount_x2 <- .05*range_x2

plot(
  x = tb_indiv_aware_factual$X1,
  y = tb_indiv_aware_factual$X2,
  col = colours_all[["Factual"]],
  xlab = "", ylab = "",
  # xlab = "X1 (UGPA)", ylab = "X2 (LSAT)",
  xlim = c(range_x1[1] - expansion_amount_x1[1], range_x1[2] + expansion_amount_x1[2]),
  ylim = c(range_x2[1] - expansion_amount_x2[1], range_x2[2] + expansion_amount_x2[2]),
  pch = 19,
  axes = FALSE
)
axis(1)
mtext(expression(X[1]~(UGCA)), side = 1, padj = .5)
axis(2)
mtext(expression(X[2]~(LSAT)), side = 2, padj = 0)
text(
  x = tb_indiv_aware_factual$X1,
  y = tb_indiv_aware_factual$X2 + 1,
  paste0(round(100*tb_indiv_aware_factual$pred, 2), "%"),
  col = colours_all[["Factual"]]
)

# Naive
tb_indiv_aware_naive <-
  tb_indiv_aware |> filter(counterfactual == "naive")
points(
  x = tb_indiv_aware_naive$X1,
  y = tb_indiv_aware_naive$X2,
  col = colours_all[["Naive"]],
  xlab = "X1", ylab = "X2",
  pch = 19
)
segments(
  x0 = tb_indiv_aware_factual$X1,
  y0 = tb_indiv_aware_factual$X2,
  x1 = tb_indiv_aware_naive$X1,
  y1 = tb_indiv_aware_naive$X2,
  col = colours_all[["Naive"]],
  lty = 2
)
text(
  x = tb_indiv_aware_naive$X1 - .1,
  y = tb_indiv_aware_naive$X2,
  paste0(round(100*tb_indiv_aware_naive$pred, 2), "%"),
  col = colours_all[["Naive"]]
)

# Transported characteristics with fairadapt
tb_indiv_aware_fpt <-
  tb_indiv_aware |> filter(counterfactual == "fpt")
points(
  x = tb_indiv_aware_fpt$X1,
  y = tb_indiv_aware_fpt$X2,
  col = colours_all[["Fairadapt"]],
  xlab = "X1", ylab = "X2",
  pch = 19
)
# x1 then x2
segments(
  x0 = tb_indiv_aware_factual$X1,
  y0 = tb_indiv_aware_factual$X2,
  x1 = tb_indiv_aware_fpt$X1,
  y1 = tb_indiv_aware_factual$X2,
  col = colours_all[["Fairadapt"]],
  lty = 2
)
segments(
  x0 = tb_indiv_aware_fpt$X1,
  y0 = tb_indiv_aware_factual$X2,
  x1 = tb_indiv_aware_fpt$X1,
  y1 = tb_indiv_aware_fpt$X2,
  col = colours_all[["Fairadapt"]],
  lty = 2
)
text(
  x = tb_indiv_aware_fpt$X1,
  y = tb_indiv_aware_fpt$X2 + 1,
  paste0(round(100*tb_indiv_aware_fpt$pred, 2), "%"),
  col = colours_all[["Fairadapt"]]
)
# Transported characteristics with OT
tb_indiv_aware_ot <-
  tb_indiv_aware |> filter(counterfactual == "ot")
points(
  x = tb_indiv_aware_ot$X1,
  y = tb_indiv_aware_ot$X2,
  col = colours_all[["OT"]],
  xlab = "X1", ylab = "X2",
  pch = 19
)
# x1 then x2
segments(
  x0 = tb_indiv_aware_factual$X1,
  y0 = tb_indiv_aware_factual$X2,
  x1 = tb_indiv_aware_ot$X1,
  y1 = tb_indiv_aware_ot$X2,
  col = colours_all[["OT"]],
  lty = 2
)
text(
  x = tb_indiv_aware_ot$X1 - .15,
  y = tb_indiv_aware_ot$X2,
  paste0(round(100*tb_indiv_aware_ot$pred, 2), "%"),
  col = colours_all[["OT"]]
)

# Transported characteristics with Sequential transport
tb_indiv_aware_seq <-
  tb_indiv_aware |> filter(counterfactual == "seq")
points(
  x = tb_indiv_aware_seq$X1,
  y = tb_indiv_aware_seq$X2,
  col = colours_all[["Seq. T."]],
  xlab = "X1", ylab = "X2",
  pch = 19
)
# x1 then x2
segments(
  x0 = tb_indiv_aware_factual$X1,
  y0 = tb_indiv_aware_factual$X2,
  x1 = tb_indiv_aware_seq$X1,
  y1 = tb_indiv_aware_factual$X2,
  col = colours_all[["Seq. T."]],
  lty = 2
)
segments(
  x0 = tb_indiv_aware_seq$X1,
  y0 = tb_indiv_aware_factual$X2,
  x1 = tb_indiv_aware_seq$X1,
  y1 = tb_indiv_aware_seq$X2,
  col = colours_all[["Seq. T."]],
  lty = 2
)
text(
  x = tb_indiv_aware_seq$X1 - .11,
  y = tb_indiv_aware_seq$X2 - 1,
  paste0(round(100*tb_indiv_aware_seq$pred, 2), "%"),
  col = colours_all[["Seq. T."]]
)
legend(
  "topleft",
  pch = 19, col = colours_all[c("Factual", "Naive", "OT", "Fairadapt", "Seq. T.")],
  legend = names(colours_all[c("Factual", "Naive", "OT", "Fairadapt", "Seq. T.")]),
  box.lty=0, bg = "transparent"
)
## Densities----

### Figure with unaware model----
# Densities of predicted scores for Black individuals with factuals and with
# counterfactuals. The yellow dashed line corresponds to the density of
# predicted scores for White individuals, using factuals.
# Factuals
tb_unaware_factuals <- tb_unaware |>
  filter(counterfactual == "none")
# Predicted values
pred_unaware_factuals_black <- tb_unaware_factuals |> filter(S == "Black") |> pull("pred")
pred_unaware_factuals_white <- tb_unaware_factuals |> filter(S == "White") |> pull("pred")
# Estimated densities
d_unaware_factuals_black <- density(pred_unaware_factuals_black)
d_unaware_factuals_white <- density(pred_unaware_factuals_white)

par(mfrow = c(4, 1), mar = c(2, 2, 0, 0))
x_lim <- c(0, .8)
y_lim <- c(0, 8)

# OT
tb_unaware_ot <- tb_unaware |> filter(counterfactual == "ot")
# Predicted values, focusing on Black --> White
pred_unaware_ot_black_star <- tb_unaware_ot |> filter(S == "White") |> pull("pred")
# Estimated densities
d_unaware_ot_black_star <- density(pred_unaware_ot_black_star)

plot(
  d_unaware_factuals_black,
  main = "", xlab = "", ylab = "",
  axes = FALSE, col = NA,
  xlim = x_lim, ylim = y_lim
)
axis(1)
axis(2)
polygon(d_unaware_factuals_black, col = alpha(colours_all[["Source"]], .5), border = NA)
lines(d_unaware_factuals_white, col = colours_all[["Reference"]], lty = 2, lwd = 2)
polygon(d_unaware_ot_black_star, col = alpha(colours_all[["OT"]], .5), border = NA)
text(x = .15, y = 6, "Factuals - Black", col = colours_all[["Source"]])
pos_arrow <- .2
ind_min <- which.min(abs(d_unaware_factuals_black$x - pos_arrow))
arrows(
  x1 = d_unaware_factuals_black$x[ind_min],
  y1 = d_unaware_factuals_black$y[ind_min],
  x0 = .15,
  y0 = 5,
  length = 0.05, col = colours_all[["Source"]]
)
text(x = .53, y = 6, "Multi. OT", col = colours_all[["OT"]])
pos_arrow_ref <- .7
text(x = pos_arrow_ref, y = 6, "Factuals - White", col = colours_all[["Reference"]])
ind_min_ref <- which.min(abs(d_unaware_factuals_white$x - pos_arrow_ref))
arrows(
  x1 = d_unaware_factuals_white$x[ind_min_ref],
  y1 = d_unaware_factuals_white$y[ind_min_ref],
  x0 = pos_arrow_ref,
  y0 = 5,
  length = 0.05, col = colours_all[["Reference"]]
)

# Naive
tb_unaware_naive <- tb_unaware |> filter(counterfactual == "naive")
# Predicted values, focusing on Black --> White
pred_unaware_naive_black_star <- tb_unaware_naive |> filter(S == "White") |> pull("pred")
# Estimated densities
d_unaware_naive_black_star <- density(pred_unaware_naive_black_star)

plot(
  d_unaware_factuals_black,
  main = "", xlab = "", ylab = "",
  axes = FALSE, col = NA,
  xlim = x_lim, ylim = y_lim
)
axis(1)
axis(2)
polygon(d_unaware_factuals_black, col = alpha(colours_all[["Source"]], .5), border = NA)
lines(d_unaware_factuals_white, col = colours_all[["Reference"]], lty = 2, lwd = 2)
polygon(d_unaware_naive_black_star, col = alpha(colours_all[["Naive"]], .5), border = NA)
text(x = .28, y = 6, "Naive", col = colours_all[["Naive"]])


# Fairadapt
tb_unaware_fpt <- tb_unaware |> filter(counterfactual == "fpt")
# Predicted values, focusing on Black --> White
pred_unaware_fpt_black_star <-
  tb_unaware_fpt |> filter(S == "White") |> pull("pred")
# Estimated densities
d_unaware_fpt_black_star <- density(pred_unaware_fpt_black_star)

plot(
  d_unaware_factuals_black,
  main = "", xlab = "", ylab = "",
  axes = FALSE, col = NA,
  xlim = x_lim, ylim = y_lim
)
axis(1)
axis(2)
polygon(d_unaware_factuals_black, col = alpha(colours_all[["Source"]], .5), border = NA)
lines(d_unaware_factuals_white, col = colours_all[["Reference"]], lty = 2, lwd = 2)
polygon(d_unaware_fpt_black_star, col = alpha(colours_all[["Fairadapt"]], .5), border = NA)
text(x = .53, y = 6, "fairadapt", col = colours_all[["Fairadapt"]])


# Sequential transport
tb_unaware_seq <- tb_unaware |> filter(counterfactual == "seq")
# Predicted values, focusing on Black --> White
pred_unaware_seq_black_star <- tb_unaware_seq |> filter(S == "White") |> pull("pred")
# Estimated densities
d_unaware_seq_black_star <- density(pred_unaware_seq_black_star)

plot(
  d_unaware_factuals_black,
  main = "", xlab = "", ylab = "",
  axes = FALSE, col = NA,
  xlim = x_lim, ylim = y_lim
)
axis(1)
axis(2)
polygon(d_unaware_factuals_black, col = alpha(colours_all[["Source"]], .5), border = NA)
lines(d_unaware_factuals_white, col = colours_all[["Reference"]], lty = 2, lwd = 2)
polygon(d_unaware_seq_black_star, col = alpha(colours_all[["Seq. T."]], .5), border = NA)
text(x = .53, y = 6, "Seq. T.", col = colours_all[["Seq. T."]])

### Figure 8 (right), with aware model----

# Densities of predicted scores for Black individuals with factuals and with
# counterfactuals. The yellow dashed line corresponds to the density of
# predicted scores for White individuals, using factuals.
# Factuals
tb_aware_factuals <- tb_aware |>
  filter(counterfactual == "none")
# Predicted values
pred_aware_factuals_black <- tb_aware_factuals |> filter(S == "Black") |> pull("pred")
pred_aware_factuals_white <- tb_aware_factuals |> filter(S == "White") |> pull("pred")
# Estimated densities
d_aware_factuals_black <- density(pred_aware_factuals_black)
d_aware_factuals_white <- density(pred_aware_factuals_white)

par(mfrow = c(4, 1), mar = c(2, 2, 0, 0))
x_lim <- c(0, .8)
y_lim <- c(0, 8)

# OT
tb_aware_ot <- tb_aware |> filter(counterfactual == "ot")
# Predicted values, focusing on Black --> White
pred_aware_ot_black_star <- tb_aware_ot |> filter(S == "White") |> pull("pred")
# Estimated densities
d_aware_ot_black_star <- density(pred_aware_ot_black_star)

plot(
  d_aware_factuals_black,
  main = "", xlab = "", ylab = "",
  axes = FALSE, col = NA,
  xlim = x_lim, ylim = y_lim
)
axis(1)
axis(2)
polygon(d_aware_factuals_black, col = alpha(colours_all[["Source"]], .5), border = NA)
lines(d_aware_factuals_white, col = colours_all[["Reference"]], lty = 2, lwd = 2)
polygon(d_aware_ot_black_star, col = alpha(colours_all[["OT"]], .5), border = NA)
text(x = .25, y = 6, "Factuals - Black", col = colours_all[["Source"]])
pos_arrow <- .2
ind_min <- which.min(abs(d_aware_factuals_black$x - .2))
arrows(
  x1 = d_aware_factuals_black$x[ind_min],
  y1 = d_aware_factuals_black$y[ind_min],
  x0 = .25,
  y0 = 5,
  length = 0.05, col = colours_all[["Source"]]
)
pos_arrow_ref <- .7
text(x = pos_arrow_ref, y = 6, "Factuals - White", col = colours_all[["Reference"]])
ind_min_ref <- which.min(abs(d_aware_factuals_white$x - pos_arrow_ref))
arrows(
  x1 = d_aware_factuals_white$x[ind_min_ref],
  y1 = d_aware_factuals_white$y[ind_min_ref],
  x0 = pos_arrow_ref,
  y0 = 5,
  length = 0.05, col = colours_all[["Reference"]]
)
text(x = .53, y = 6, "Multi. OT", col = colours_all[["OT"]])

# Naive
tb_aware_naive <- tb_aware |> filter(counterfactual == "naive")
# Predicted values, focusing on Black --> White
pred_aware_naive_black_star <- tb_aware_naive |> filter(S == "White") |> pull("pred")
# Estimated densities
d_aware_naive_black_star <- density(pred_aware_naive_black_star)

plot(
  d_aware_factuals_black,
  main = "", xlab = "", ylab = "",
  axes = FALSE, col = NA,
  xlim = x_lim, ylim = y_lim
)
axis(1)
axis(2)
polygon(d_aware_factuals_black, col = alpha(colours_all[["Source"]], .5), border = NA)
lines(d_aware_factuals_white, col = colours_all[["Reference"]], lty = 2, lwd = 2)
polygon(d_aware_naive_black_star, col = alpha(colours_all[["Naive"]], .5), border = NA)
text(x = .35, y = 6, "Naive", col = colours_all[["Naive"]])


# Fairadapt
tb_aware_fpt <- tb_aware |> filter(counterfactual == "fpt")
# Predicted values, focusing on Black --> White
pred_aware_fpt_black_star <-
  tb_aware_fpt |> filter(S == "White") |> pull("pred")
# Estimated densities
d_aware_fpt_black_star <- density(pred_aware_fpt_black_star)

plot(
  d_aware_factuals_black,
  main = "", xlab = "", ylab = "",
  axes = FALSE, col = NA,
  xlim = x_lim, ylim = y_lim
)
axis(1)
axis(2)
polygon(d_aware_factuals_black, col = alpha(colours_all[["Source"]], .5), border = NA)
lines(d_aware_factuals_white, col = colours_all[["Reference"]], lty = 2, lwd = 2)
polygon(d_aware_fpt_black_star, col = alpha(colours_all[["Fairadapt"]], .5), border = NA)
text(x = .53, y = 6, "fairadapt", col = colours_all[["Fairadapt"]])


# Sequential transport
tb_aware_seq <- tb_aware |> filter(counterfactual == "seq")
# Predicted values, focusing on Black --> White
pred_aware_seq_black_star <- tb_aware_seq |> filter(S == "White") |> pull("pred")
# Estimated densities
d_aware_seq_black_star <- density(pred_aware_seq_black_star)

plot(
  d_aware_factuals_black,
  main = "", xlab = "", ylab = "",
  axes = FALSE, col = NA,
  xlim = x_lim, ylim = y_lim
)
axis(1)
axis(2)
polygon(d_aware_factuals_black, col = alpha(colours_all[["Source"]], .5), border = NA)
lines(d_aware_factuals_white, col = colours_all[["Reference"]], lty = 2, lwd = 2)
polygon(d_aware_seq_black_star, col = alpha(colours_all[["Seq. T."]], .5), border = NA)
text(x = .53, y = 6, "Seq. T.", col = colours_all[["Seq. T."]])

## Metrics (Table 1, Table 3)----

# Probability threshold for classifier
threshold <- .5

inb_black <- which(df_race_c$S == "Black")
# Observed target variable
obs_0 <- df_race_c |> filter(S == "Black") |> pull("Y")
obs_1 <- df_race_c |> filter(S == "White") |> pull("Y")
# Scores using factuals
pred_0_aware <- factuals_aware |> filter(S_origin == "Black") |> pull("pred")
pred_1_aware <- factuals_aware |> filter(S_origin == "White") |> pull("pred")
pred_0_unaware <- factuals_unaware |> filter(S_origin == "Black") |> pull("pred")
pred_1_unaware <- factuals_unaware |> filter(S_origin == "White") |> pull("pred")
# Scores in groups S="Black" using naive counterfactuals
pred_0_naive_aware <- counterfactuals_aware_naive_black$pred
pred_0_naive_unaware <- counterfactuals_unaware_naive_black$pred
# Scores in groups S="Black" using OT counterfactuals
pred_0_ot_aware <- counterfactuals_aware_ot_black$pred[inb_black]
pred_0_ot_unaware <- counterfactuals_unaware_ot_black$pred[inb_black]
# Scores in groups S="Black" using fairadapt counterfactuals
pred_0_fpt_aware <- counterfactuals_aware_fpt_black$pred
pred_0_fpt_unaware <- counterfactuals_unaware_fpt_black$pred
# Scores in groups S="Black" using sequential transport counterfactuals
pred_0_seq_aware <- counterfactuals_aware_seq_black$pred
pred_0_seq_unaware <- counterfactuals_unaware_seq_black$pred

# Compute metrics
names_pred <- rep(c("naive", "ot", "fpt", "seq"), each = 2)
names_model <- rep(c("aware", "unaware"), 4)
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
