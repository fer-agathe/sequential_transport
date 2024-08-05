# Classifier (logistic regression) trained on the law school dataset

# Required packages
library(tidyverse)

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

# Load Data----
load("../data/df_race.rda")

# Pre-Processing----
# Y as a binary variable
med <- median(df_race$Y)
df_race_c <- df_race |>
  mutate(
    Y_c = ifelse(Y > med, 1, 0)
  ) |>
  select(S, X1, X2, Y = Y_c)

df_race_c$Y <- as.factor(df_race_c$Y)
levels(df_race_c$Y)

# Split Data
# (see ../functions/utils.R)
seed <- 2025
sets <- split_dataset(df_race_c, seed)
data_train <- sets$data_train
data_test <- sets$data_test

# Training the Model----

# Unaware: without S
# Aware: with S

# Using log_reg_train() (see ../functions/utils.R)

# Unaware logistic regression classifier (model without S)
pred_unaware <- log_reg_train(data_train, data_test, type = "unaware")
pred_unaware_train <- pred_unaware$pred_train
pred_unaware_test <- pred_unaware$pred_test

# Aware logistic regression classifier (model with S)
pred_aware <- log_reg_train(data_train, data_test, type = "aware")
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

# Unaware:
ggplot(
  data = df_test_unaware,
  mapping = aes(x = pred, fill = S)) +
  geom_histogram(
    mapping = aes(y = after_stat(density)),
    alpha = 0.5, position = "identity", binwidth = 0.05
  ) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Unaware Model, with S being Race",
    x = "Predictions for Y",
    y = "Density"
  ) +
  global_theme()

# Aware:
ggplot(
  data = df_test_aware,
  mapping = aes(x = pred, fill = S)) +
  geom_histogram(
    mapping = aes(y = after_stat(density)),
    alpha = 0.5, position = "identity", binwidth = 0.05
  ) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Aware Model, with S being Race",
    x = "Predictions for Y",
    y = "Density"
  ) +
  global_theme()


# Predictions----
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

# Saving Objects----
save(df_race_c, file = "../data/df_race_c.rda")
save(pred_aware, file = "../data/pred_aware.rda")
save(pred_unaware, file = "../data/pred_unaware.rda")
save(pred_unaware_all, file = "../data/pred_unaware_all.rda")
save(pred_aware_all, file = "../data/pred_aware_all.rda")
