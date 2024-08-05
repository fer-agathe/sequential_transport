# Law School dataset: Sequential Transport for counterfactual inference

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

# Counterfactuals with Sequential Transport----

#' Sequential transport
#'
#' @param data dataset with three columns:
#'  - S: sensitive attribute
#'  - X1: first predictor, assumed to be causally linked to S
#'  - X2: second predictor, assumed to be causally linked to S and X1
#' @param S_0 value for the sensitive attribute for the source distribution
#' @param number of cells in each dimension (default to 15)
#' @param h small value added to extend the area covered by the grid (default
#'  to .2)
#' @param d neighborhood weight when conditioning by x1 (default to .5)
transport_function <- function(data,
                               S_0,
                               n_grid = 15,
                               h = .2,
                               d = .5
) {

  # Subset of the data: S=0, and S=1
  D_SXY_0 <- data[data$S ==S_0, ]
  D_SXY_1 <- data[data$S!= S_0, ]

  # Coordinates of the cells of the grid on subset of S=0
  vx1_0 <- seq(min(D_SXY_0$X1) - h, max(D_SXY_0$X1) + h, length = n_grid + 1)
  vx2_0 <- seq(min(D_SXY_0$X2) - h, max(D_SXY_0$X2) + h, length = n_grid + 1)
  # and middle point of the cells
  vx1_0_mid <- (vx1_0[2:(1+n_grid)]+vx1_0[1:(n_grid)]) / 2
  vx2_0_mid <- (vx2_0[2:(1+n_grid)]+vx2_0[1:(n_grid)]) / 2

  # Coordinates of the cells of the grid on subset of S=0
  vx1_1 <- seq(min(D_SXY_1$X1) -h, max(D_SXY_1$X1) + h, length = n_grid + 1)
  vx1_1_mid <- (vx1_1[2:(1 + n_grid)] + vx1_1[1:(n_grid)]) / 2
  # and middle point of the cells
  vx2_1 <- seq(min(D_SXY_1$X2) - h, max(D_SXY_1$X2) + h, length = n_grid + 1)
  vx2_1_mid <- (vx2_1[2:(1 + n_grid)] + vx2_1[1:(n_grid)]) / 2

  # Creation of the grids for the CDF and Quantile function
  # init with NA values
  # One grid for X1 and X2, on both subsets of the data (S=0, and S=1)
  F1_0 <- F2_0 <- F1_1 <- F2_1 <- matrix(NA, n_grid, n_grid)
  Q1_0 <- Q2_0 <- Q1_1 <- Q2_1 <- matrix(NA, n_grid, n_grid)

  # Empirical CDF for X1 on subset of S=0
  FdR1_0 <- Vectorize(function(x) mean(D_SXY_0$X1 <= x))
  f1_0 <- FdR1_0(vx1_0_mid)
  # Empirical CDF for X2 on subset of S=0
  FdR2_0 <- Vectorize(function(x) mean(D_SXY_0$X2 <= x))
  f2_0 <- FdR2_0(vx2_0_mid)
  # Empirical CDF for X1 on subset of S=1
  FdR1_1 <- Vectorize(function(x) mean(D_SXY_1$X1 <= x))
  f1_1 <- FdR1_1(vx1_1_mid)
  # Empirical CDF for X2 on subset of S=1
  FdR2_1 <- Vectorize(function(x) mean(D_SXY_1$X2 <= x))
  f2_1 <- FdR2_1(vx2_1_mid)

  u <- (1:n_grid) / (n_grid + 1)
  # Empirical quantiles for X1 on subset of S=0
  Qtl1_0 <- Vectorize(function(x) quantile(D_SXY_0$X1, x))
  q1_0 <- Qtl1_0(u)
  # Empirical quantiles for X2 on subset of S=0
  Qtl2_0 <- Vectorize(function(x) quantile(D_SXY_0$X2, x))
  q2_0 <- Qtl2_0(u)
  # Empirical quantiles for X1 on subset of S=1
  Qtl1_1 <- Vectorize(function(x) quantile(D_SXY_1$X1, x))
  q1_1 <- Qtl1_1(u)
  # Empirical quantiles for X2 on subset of S=1
  Qtl2_1 <- Vectorize(function(x) quantile(D_SXY_1$X2, x))
  q2_1 <- Qtl2_1(u)

  # Compute c.d.f and quantile at each cell of the grid in both groups
  for(i in 1:n_grid) {
    # Subset of S=0
    idx1_0 <- which(abs(D_SXY_0$X1 - vx1_0_mid[i]) < d)
    FdR2_0 <- Vectorize(function(x) mean(D_SXY_0$X2[idx1_0] <= x))
    F2_0[, i] <- FdR2_0(vx2_0_mid)
    Qtl2_0 <- Vectorize(function(x) quantile(D_SXY_0$X2[idx1_0], x))
    Q2_0[, i] <- Qtl2_0(u)

    idx2_0 <- which(abs(D_SXY_0$X2 - vx2_0_mid[i]) < d)
    FdR1_0 <- Vectorize(function(x) mean(D_SXY_0$X1[idx2_0] <= x))
    F1_0[, i] <- FdR1_0(vx1_0_mid)
    Qtl1_0 <- Vectorize(function(x) quantile(D_SXY_0$X1[idx2_0], x))
    Q1_0[, i] <- Qtl1_0(u)

    # Subset of S=1
    idx1_1 <- which(abs(D_SXY_1$X1 - vx1_1_mid[i]) < d)
    FdR2_1 <- Vectorize(function(x) mean(D_SXY_1$X2[idx1_1] <= x))
    F2_1[, i] <- FdR2_1(vx2_1_mid)
    Qtl2_1 <- Vectorize(function(x) quantile(D_SXY_1$X2[idx1_1], x))
    Q2_1[, i] <- Qtl2_1(u)

    idx2_1 <- which(abs(D_SXY_1$X2-vx2_1_mid[i])<d)
    FdR1_1 <- Vectorize(function(x) mean(D_SXY_1$X1[idx2_1] <= x))
    F1_1[, i] <- FdR1_1(vx1_1_mid)
    Qtl1_1 <- Vectorize(function(x) quantile(D_SXY_1$X1[idx2_1], x))
    Q1_1[, i] <- Qtl1_1(u)
  }

  # Transport for X2
  T2 <- function(x2) {
    i <- which.min(abs(vx2_0_mid - x2))
    p <- f2_0[i]
    i <- which.min(abs(u - p))
    x2star <- q2_1[i]
    x2star
  }

  # Transport for X1
  T1 <- function(x1) {
    i <- which.min(abs(vx1_0_mid - x1))
    p <- f1_0[i]
    i <- which.min(abs(u - p))
    x1star <- q1_1[i]
    x1star
  }

  # Transport for X2 conditional on X1
  T2_cond_x1 <- function(x2, x1) {
    k0 <- which.min(abs(vx1_0_mid - x1))
    k1 <- which.min(abs(vx1_1_mid - T1(x1)))
    i <- which.min(abs(vx2_0_mid - x2))
    p <- F2_0[i, k0]
    i <- which.min(abs(u - p))
    x2star <- Q2_1[i, k1]
    x2star
  }

  # Transport for X1 conditional on X2
  T1_cond_x2 <- function(x1, x2) {
    k0 <- which.min(abs(vx2_0_mid - x2))
    k1 <- which.min(abs(vx2_1_mid - T2(x2)))
    i <- which.min(abs(vx1_0_mid - x1))
    p <- F1_0[i, k0]
    i <- which.min(abs(u - p))
    x1star <- Q1_1[i, k1]
    x1star
  }

  list(
    Transport_x1 = T1,
    Transport_x2 = T2,
    Transport_x1_cond_x2 = T1_cond_x2,
    Transport_x2_cond_x1 = T2_cond_x1
  )
}


df_race_c_light <- df_race_c |> select(S, X1, X2)
ind_white <- which(df_race_c_light$S == "White")
ind_black <- which(df_race_c_light$S == "Black")

seq_functions <- transport_function(
  data = df_race_c_light, S_0 = "Black", n_grid = 500
)

# Functions T1(X1) and T2(X2)
T_X1 <- seq_functions$Transport_x1
T_X2 <- seq_functions$Transport_x2
# T2(X2|X1)
T_X2_c_X1 <- seq_functions$Transport_x2_cond_x1

# Values of X1 and X2 for Black individuals
a10 <- df_race_c_light$X1[ind_black]
a20 <- df_race_c_light$X2[ind_black]
# Transported values
x1_star <- map_dbl(a10, T_X1) # Transport X1 to group S=White
x2_star <- map2_dbl(a20, a10, T_X2_c_X1) # Transport X2|X1 to group S=White

df_counterfactuals_seq_black <-
  df_race_c_light |> mutate(id = row_number()) |>
  filter(S == "Black") |>
  mutate(
    S = "White",
    X1 = x1_star,
    X2 = x2_star
  )

# Predictions on counterfactuals
# Unaware model
model_unaware <- pred_unaware$model
pred_seq_unaware <- predict(
  model_unaware, newdata = df_counterfactuals_seq_black,type = "response"
)
# Aware model
model_aware <- pred_aware$model
pred_seq_aware <- predict(
  model_aware, newdata = df_counterfactuals_seq_black,type = "response"
)
counterfactuals_unaware_seq_black <-
  df_counterfactuals_seq_black |>
  mutate(pred = pred_seq_unaware, type = "counterfactual")
counterfactuals_aware_seq_black <-
  df_counterfactuals_seq_black |>
  mutate(pred = pred_seq_aware, type = "counterfactual")

factuals_aware <- tibble(
  S = df_race$S,
  X1 = df_race$X1,
  X2 = df_race$X2,
  pred = pred_aware_all,
  type = "factual"
)

factuals_unaware <- tibble(
  S = df_race$S,
  X1 = df_race$X1,
  X2 = df_race$X2,
  pred = pred_unaware_all,
  type = "factual"
)

aware_seq_black <- bind_rows(
  factuals_aware |> mutate(id = row_number()) |> filter(S == "Black"),
  counterfactuals_aware_seq_black
)
unaware_seq_black <- bind_rows(
  factuals_unaware |> mutate(id = row_number()) |> filter(S == "Black"),
  counterfactuals_aware_seq_black)

# Distribution of predicted values

## Unaware model
ggplot(
  data = unaware_seq_black,
  mapping = aes(x = pred, fill = type)
) +
  geom_histogram(
    mapping = aes(y = after_stat(density)),
    alpha = 0.5, position = "identity", binwidth = 0.05
  ) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Unware model, S: Race - Black --> White",
    x = "Predictions for Y",
    y = "Density") +
  global_theme()

## Aware model
ggplot(
  data = aware_seq_black,
  mapping = aes(x = pred, fill = type)
) +
  geom_histogram(
    mapping = aes(y = after_stat(density)),
    alpha = 0.5, position = "identity", binwidth = 0.05
  ) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Aware model, S: Race - Black --> White",
    x = "Predictions for Y",
    y = "Density") +
  global_theme()

# Comparison for Two Individuals----

# With the previous methods, we looked at individuals in row 24 (Black) and in
# row 25 (White)
# Here, we focus on the first three Black individuals
# (including the one in row 24)

## Unaware Model----
factuals_unaware |> mutate(id = row_number()) |>
  filter(S == "Black") |>
  dplyr::slice(1:3)

# Characteristics after sequential transport
indiv_counterfactuals_unaware_seq <- counterfactuals_unaware_seq_black[c(1:3), ]
indiv_counterfactuals_unaware_seq

indiv_unaware_seq <- bind_rows(
  factuals_unaware |>
    mutate(id = row_number()) |>
    filter(S == "Black") |>
    dplyr::slice(1:3),
  indiv_counterfactuals_unaware_seq
)
indiv_unaware_seq

## Aware Model----
indiv_counterfactuals_aware_seq <- counterfactuals_aware_seq_black[c(1:3), ]
indiv_counterfactuals_aware_seq

indiv_aware_seq <- bind_rows(
  factuals_aware |>
    mutate(id = row_number()) |>
    filter(S == "Black") |>
    dplyr::slice(1:3),
  indiv_counterfactuals_aware_seq
)
indiv_aware_seq

# Counterfactual Demographic Parity----

## Unaware Model
mean(
  counterfactuals_unaware_seq_black$pred -
    factuals_unaware |> filter(S == "Black") |> pull("pred")
)

## Aware Model
mean(
  counterfactuals_aware_seq_black$pred -
    factuals_aware |> filter(S == "Black") |> pull("pred")
)

# Saving Objects----
save(
  counterfactuals_unaware_seq_black,
  file = "../data/counterfactuals_unaware_seq_black.rda"
)
save(
  counterfactuals_aware_seq_black,
  file = "../data/counterfactuals_aware_seq_black.rda"
)
