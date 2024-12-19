# Wrong Causal Assumptions
# Simulate data with independent covariates
# Assume relationship between these on the causal graph
# Compare when correctly assume no dependence.

# Required packages----
library(tidyverse)
library(ks)
library(expm)

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

colours <- c(
  `0` = "#5BBCD6",
  `1` = "#FF0000",
  A = "#00A08A",
  B = "#F2AD00",
  with = "#046C9A",
  without = "#C93312",
  `2` = "#0B775E"
)

library(devtools)
# Load our package
load_all("../seqtransfairness/")


# Data----

set.seed(123) # set the seed for reproductible results
n <- 500

# group s = 0
X1_0 <- runif(n, 0, 1)
X2_0 <- runif(n, 0, 1)
# group s = 1
X1_1 <- runif(n, 1, 2)
X2_1 <- runif(n, 1, 2)

# Drawing random binary response variable Y with logistic model for each group
eta_1 <- (X1_1 * .8 + X2_1 / 2 * 1.2) / 2
eta_0 <- (X1_0 * 1.2 + X2_0 / 2 * .8) / 2
p_1 <- exp(eta_1) / (1 + exp(eta_1))
p_0 <- exp(eta_0) / (1 + exp(eta_0))

Y_0 <- rbinom(n, size = 1, prob = p_0)
Y_1 <- rbinom(n, size = 1, prob = p_1)

D_SXY_0 <- tibble(
  S = 0,
  X1 = X1_0,
  X2 = X2_0,
  Y = Y_0
)
D_SXY_1 <- tibble(
  S = 1,
  X1 = X1_1,
  X2 = X2_1,
  Y = Y_1
)

D_SXY <- bind_rows(D_SXY_0, D_SXY_1)

hist(p_0,
     col = colours[["A"]],
     breaks = 20,
     xlim = c(0, 1),
     ylim = c(0,100),
     main = "Histogram with probability values for both groups"
)

# Ajout du second histogramme
hist(p_1,
     col = colours[["B"]],
     breaks = 10,
     add = TRUE)

legend("topleft", legend = c("p_0", "p_1"), fill = colours[c("A", "B")])


# Computation of smoothing parameters (bandwidth) for kernel density estimation
H0 <- Hpi(D_SXY_0[, c("X1","X2")])
H1 <- Hpi(D_SXY_1[, c("X1","X2")])

# Calculating multivariate densities in each group
f0_2d <- kde(D_SXY_0[, c("X1","X2")], H = H0, xmin = c(-1, -1), xmax = c(5, 3))
f1_2d <- kde(D_SXY_1[, c("X1","X2")], H = H1, xmin = c(-1, -1), xmax = c(5, 3))

par(mar = c(2,2,0,0))
plot(
  c(-1,3), c(-1,3),
  xlab = "", ylab = "", axes = FALSE, col = NA,
  xlim = c(-1, 3), ylim = c(-1, 3)
)
axis(1)
axis(2)
# Observed values
points(D_SXY_0$X1, D_SXY_0$X2, col = alpha(colours["A"], .4), pch = 19, cex = .3)
# Estimated density
contour(
  f0_2d$eval.point[[1]],
  f0_2d$eval.point[[2]],
  f0_2d$estimate,col=scales::alpha(colours["A"], 1),
  add = TRUE
)
points(D_SXY_1$X1, D_SXY_1$X2, col = alpha(colours["B"], .4), pch = 19, cex = .3)
contour(
  f1_2d$eval.point[[1]],
  f1_2d$eval.point[[2]],
  f1_2d$estimate,col=scales::alpha(colours["B"], 1),
  add = TRUE
)


# Assumed Causal Models----

variables <- c("S", "X1", "X2", "Y")

## Correct Assumption----

adj_indep_correct <- matrix(
  # S  X1 X2 Y
  c(0, 1, 1, 1, # S
    0, 0, 0, 1, # X1
    0, 0, 0, 1, # X2
    0, 0, 0, 0  # Y
  ),
  ncol = length(variables),
  dimnames = rep(list(variables), 2),
  byrow = TRUE
)

causal_graph_correct <- fairadapt::graphModel(adj_indep_correct)
plot(causal_graph_correct)

## Wrong Assumption: X1 causes X2----

adj_indep_inc_x1_then_x2 <- matrix(
  # S  X1 X2 Y
  c(0, 1, 1, 1, # S
    0, 0, 1, 1, # X1
    0, 0, 0, 1, # X2
    0, 0, 0, 0  # Y
  ),
  ncol = length(variables),
  dimnames = rep(list(variables), 2),
  byrow = TRUE
)

causal_graph_inc_x1_then_x2 <-
  fairadapt::graphModel(adj_indep_inc_x1_then_x2)
plot(causal_graph_inc_x1_then_x2)


## Wrong Assumption: X2 causes X1

adj_indep_inc_x2_then_x1 <- matrix(
  # S  X1 X2 Y
  c(0, 1, 1, 1, # S
    0, 0, 0, 1, # X1
    0, 1, 0, 1, # X2
    0, 0, 0, 0  # Y
  ),
  ncol = length(variables),
  dimnames = rep(list(variables), 2),
  byrow = TRUE
)

causal_graph_inc_x2_then_x1 <-
  fairadapt::graphModel(adj_indep_inc_x2_then_x1)
plot(causal_graph_inc_x2_then_x1)


# Counterfactuals----

## Multivariate OT----

write_csv(D_SXY, file = "../data/D_SXY_indep.csv")

library(reticulate)
use_virtualenv("~/quarto-python-env", required = TRUE)
py_run_file("05_wrong-causal-assumptions-ot.py")

# Import transported values after the script was run
counterfactuals_ot <-
  read_csv("../data/../data/counterfactuals_ot_test_indep.csv")

counterfactuals_ot

## Sequential Transport----

# With correct assumption
trans_indep_correct <- seq_trans(
  data = D_SXY, adj = adj_indep_correct, s = "S", S_0 = 0, y = "Y"
)
# With wrong assumptions
trans_indep_inc_x1_then_x2 <- seq_trans(
  data = D_SXY, adj = adj_indep_inc_x1_then_x2, s = "S", S_0 = 0, y = "Y"
)
trans_indep_inc_x2_then_x1 <- seq_trans(
  data = D_SXY, adj = adj_indep_inc_x2_then_x1, s = "S", S_0 = 0, y = "Y"
)

# New Observation----

new_obs <- tibble(S = 0, X1 = .5, X2 = .5)
# Transport of this new observation with sequential transport
new_obs_indep_correct <- seq_trans_new(
  x = trans_indep_correct, newdata = new_obs, data = D_SXY
)
new_obs_indep_inc_x1_then_x2 <- seq_trans_new(
  x = trans_indep_inc_x1_then_x2, newdata = new_obs, data = D_SXY
)
new_obs_indep_inc_x2_then_x1 <- seq_trans_new(
  x = trans_indep_inc_x2_then_x1, newdata = new_obs, data = D_SXY
)

# Hypothetical Model----

#' Logistic regression
#'
#' @param x1 first numerical predictor
#' @param x2 second numerical predictor
#' @param s sensitive attribute (0/1)
logistique_reg <- function(x1, x2, s) {
  eta <- (x1 + x2) / 2 - s
  exp(eta) / (1 + exp(eta))
}

# Predictions with that model over a grid, for reference
vx0 <- seq(-5, 5, length = 251)
data_grid <- expand.grid(x = vx0, y = vx0)
L0 <- logistique_reg(x1 = data_grid$x, x2 = data_grid$y, s = 0)
L1 <- logistique_reg(x1 = data_grid$x, x2 = data_grid$y, s = 1)
# as a grid:
dlogistique0 <- matrix(L0, length(vx0), length(vx0))
dlogistique1 <- matrix(L1, length(vx0), length(vx0))

# Visualization----

# Coordinates of points (new obs and counterfactuals)
coords_indep <- tibble(
  # 1. (x1, x2)
  start = c(new_obs$X1, new_obs$X2, 0),
  #
  # 2. (T_1(x1 | x2), T_2(x2)), correct assumption
  correct = c(
    new_obs_indep_correct$X1, new_obs_indep_correct$X2, 1
  ),
  # 3. (T_1(x1), T_2(x2 | x1)), assuming X1 -> X2
  inc_x1_then_x2 = c(
    new_obs_indep_inc_x1_then_x2$X1, new_obs_indep_inc_x1_then_x2$X2, 1
  ),
  # 4. (T_1(x1 | x2), T_2(x2)), assuming X2 -> X1
  inc_x2_then_x1 = c(
    new_obs_indep_inc_x2_then_x1$X1, new_obs_indep_inc_x2_then_x1$X2, 1
  ),
  #
  # 5. (T_1(x1), x2)
  correct_interm_x2 = c(new_obs_indep_correct$X1, new_obs$X2, 0.5),
  # 6. (T_1(x1), x2), assuming X1 -> X2
  inc_x1_then_x2_interm_x2 = c(new_obs_indep_inc_x1_then_x2$X1, new_obs$X2, 0.5),
  # 7. (T_1(x1), x2), assuming X2 -> X1
  inc_x2_then_x1_interm_x2 = c(new_obs_indep_inc_x2_then_x1$X1, new_obs$X2, 0.5),
  #
  # 8. (x1, T(x2))
  correct_interm_x1 = c(new_obs$X1, new_obs_indep_correct$X2, 0.5),
  # 9. (T_1(x1), x2), assuming X1 -> X2
  inc_x1_then_x2_interm_x1 = c(new_obs$X1, new_obs_indep_inc_x1_then_x2$X2, 0.5),
  # 10. (T_1(x1), x2), assuming X2 -> X1
  inc_x2_then_x1_interm_x1 = c(new_obs$X1, new_obs_indep_inc_x2_then_x1$X2, 0.5),
  # 11. T*(x1,x2)
  ot = c(last(counterfactuals_ot$X1), last(counterfactuals_ot$X2), 1)
)

# Predictions with the hypothetical model
predicted_val <- apply(
  coords_indep,
  2,
  function(column) logistique_reg(
    x1 = column[1], x2 = column[2], s = column[3]
  )
)

predicted_val <- c(
  naive = logistique_reg(
    x1 = coords_indep$start[1], x2 = coords_indep$start[2], s = 1
  ),
  predicted_val
)

## Figure (18), Appendix E----

# Colour scale from colour of class 0 to class 1
colfunc <- colorRampPalette(c(colours["0"], colours["1"]))
scl <- scales::alpha(colfunc(9),.9)

par(mar = c(2, 2, 0, 0))
# Group 0
## Estimated density: level curves for (x1, x2) -> m(0, x1, x2)
CeX <- 1
contour(
  f0_2d$eval.point[[1]],
  f0_2d$eval.point[[2]],
  f0_2d$estimate,
  col = scales::alpha(colours["A"], .3),
  axes = FALSE, xlab = "", ylab = "",
  xlim = c(-1, 3), ylim = c(-1, 3)
)
# Group 1
## Estimated density: level curves for (x1, x2) -> m(1, x1, x2)
contour(
  f1_2d$eval.point[[1]],
  f1_2d$eval.point[[2]],
  f1_2d$estimate,
  col = scales::alpha(colours["B"], .3), add = TRUE
)
contour(
  vx0, vx0, dlogistique0,
  levels = seq(0.05, 0.95, by = 0.05),
  col = scl,
  add = TRUE,lwd = 2
)
axis(1)
axis(2)

###
# Individual (S=0, x1=.5, x2=.5)
###
points(
  coords_indep$start[1], coords_indep$start[2],
  pch = 19, cex = CeX, col = "darkblue"
)

## Predicted value for the individual, based on factuals
text(
  coords_indep$start[1], coords_indep$start[2],
  paste(round(predicted_val[1] * 100, 1), "%", sep = ""),
  pos = 1, cex = CeX, col = "darkblue"
)

## Figure (18, right), Appenxix E----

colour_start <- "darkblue"
colour_correct <- "#CC79A7"
colour_inc_x1_then_x2 <- "darkgray"
colour_inc_x2_then_x1 <- "#56B4E9"
colour_ot <- "#C93312"
colour_naive <- "#000000"



CeX <- 1
par(mar = c(2, 2, 0, 0))
# Group 0
## Estimated density: level curves for (x1, x2) -> m(0, x1, x2)
contour(
  f0_2d$eval.point[[1]],
  f0_2d$eval.point[[2]],
  f0_2d$estimate,
  col = scales::alpha(colours["A"], .3),
  axes = FALSE, xlab = "", ylab = "",
  xlim = c(-1, 3), ylim = c(-1, 3)
)
# Group 1
## Estimated density: level curves for (x1, x2) -> m(1, x1, x2)
contour(
  f1_2d$eval.point[[1]],
  f1_2d$eval.point[[2]],
  f1_2d$estimate,
  col = scales::alpha(colours["B"], .3), add = TRUE
)

# Contour of estimates by the model for s=1
contour(
  vx0, vx0, dlogistique1,
  levels = seq(0.05, 0.95, by = 0.05),
  col = scl, lwd=2,
  add = TRUE
)
axis(1)
axis(2)
###
# Individual (s=0, x1=.5, x2=.5)
###
points(
  coords_indep$start[1], coords_indep$start[2],
  pch = 19, cex = CeX, col = colour_start
)
## Predicted value for the individual, based on factuals
text(
  coords_indep$start[1], coords_indep$start[2],
  paste(round(predicted_val["start"] * 100, 1), "%", sep = ""),
  pos = 1, cex = CeX, col = colour_start
)

###
# Transported individual using correct DAG
###
points(
  coords_indep$correct[1], coords_indep$correct[2],
  pch = 19, cex = CeX, col = colour_correct
)
text(
  coords_indep$correct[1]-0.1, coords_indep$correct[2]+0.15,
  paste(round(predicted_val[["correct"]]*100,1),"%",sep=""),
  pos = 4, cex = CeX, col = colour_correct
)
segments(
  x0 = coords_indep$start[1], y0 = coords_indep$start[2],
  x1 = coords_indep$correct_interm_x2[1], y1 = coords_indep$correct_interm_x2[2],
  lwd = .8, col = colour_correct
)
segments(
  x0 = coords_indep$correct_interm_x2[1], y0 = coords_indep$correct_interm_x2[2],
  x1 = coords_indep$correct[1], y1 = coords_indep$correct[2],
  lwd = .8, col = colour_correct
)
## Intermediate point
points(
  coords_indep$correct_interm_x2[1], coords_indep$correct_interm_x2[2],
  pch = 19, col = "white", cex = CeX
)
points(
  coords_indep$correct_interm_x2[1], coords_indep$correct_interm_x2[2],
  pch = 1, cex = CeX, col = colour_correct
)


###
# Transported individual assuming X2 depends on X1
###
points(
  coords_indep$inc_x1_then_x2[1], coords_indep$inc_x1_then_x2[2],
  pch=19,cex=CeX, col = colour_inc_x1_then_x2
)
segments(
  x0 = coords_indep$start[1], y0 = coords_indep$start[2],
  x1 = coords_indep$inc_x1_then_x2[1], y1 = coords_indep$inc_x1_then_x2_interm_x2[2],
  lwd = .8, col = colour_inc_x1_then_x2
)
segments(
  x0 = coords_indep$inc_x1_then_x2[1], y0 = coords_indep$inc_x1_then_x2_interm_x2[2],
  x1 = coords_indep$inc_x1_then_x2[1], y1 = coords_indep$inc_x1_then_x2[2],
  lwd = .8, col = colour_inc_x1_then_x2
)
## Intermediate point
points(
  coords_indep$inc_x1_then_x2[1], coords_indep$inc_x1_then_x2_interm_x2[2],
  pch = 19, col = "white", cex = CeX
)
points(
  coords_indep$inc_x1_then_x2[1], coords_indep$inc_x1_then_x2_interm_x2[2],
  pch = 1, cex = CeX, col = colour_inc_x1_then_x2
)
## New predicted value
text(
  coords_indep$inc_x1_then_x2[1]+0.6, coords_indep$inc_x1_then_x2[2],
  paste(round(predicted_val[["inc_x1_then_x2"]]*100,1),"%",sep=""),
  pos = 2, cex = CeX, col = colour_inc_x1_then_x2
)


###
# Transported individual assuming X1 depends on X2
###
points(
  coords_indep$inc_x2_then_x1[1], coords_indep$inc_x2_then_x1[2],
  pch=19,cex=CeX, col = colour_inc_x2_then_x1
)
segments(
  x0 = coords_indep$start[1], y0 = coords_indep$start[2],
  x1 = coords_indep$inc_x2_then_x1_interm_x1[1], y1 = coords_indep$inc_x2_then_x1[2],
  lwd = .8, col = colour_inc_x2_then_x1
)
segments(
  x0 = coords_indep$inc_x2_then_x1_interm_x1[1], y0 = coords_indep$inc_x2_then_x1[2],
  x1 = coords_indep$inc_x2_then_x1[1], y1 = coords_indep$inc_x2_then_x1[2],
  lwd = .8, col = colour_inc_x2_then_x1
)
## Intermediate point
points(
  coords_indep$inc_x2_then_x1_interm_x1[1], coords_indep$inc_x2_then_x1[2],
  pch = 19, col = "white", cex = CeX
)
points(
  coords_indep$inc_x2_then_x1_interm_x1[1], coords_indep$inc_x2_then_x1[2],
  pch = 1, cex = CeX, col = colour_inc_x2_then_x1
)
## New predicted value
text(
  coords_indep$inc_x2_then_x1[1]+0.3, coords_indep$inc_x2_then_x1[2]-0.1,
  paste(round(predicted_val[["inc_x2_then_x1"]]*100,1),"%",sep=""),
  pos = 3, cex = CeX, col = colour_inc_x2_then_x1
)


###
# Transported individual with multivariate optimal transport
###
points(
  coords_indep$ot[1], coords_indep$ot[2],
  pch=15,cex=CeX, col = colour_ot
)
segments(
  x0 = coords_indep$start[1], y0 = coords_indep$start[2],
  x1 = coords_indep$ot[1], y1 = coords_indep$ot[2],
  lwd = .8, col = colour_ot, lty = 2
)
## New predicted value
text(
  coords_indep$ot[1]-0.1, coords_indep$ot[2]+0.1,
  paste(round(predicted_val[["ot"]] * 100, 1), "%", sep = ""),
  pos = 4, cex = CeX, col = colour_ot
)

###
# New predicted value for (do(s=1), x1, x2), no transport
###
ry <- .09
plotrix::draw.circle(
  x = coords_indep$start[1] - .9*ry * sqrt(2),
  y = coords_indep$start[2] - .9*ry * sqrt(2),
  radius = ry * sqrt(2), border = colour_naive
)
text(
  coords_indep$start[1], coords_indep$start[2],
  paste(round(predicted_val["naive"] * 100, 1), "%", sep = ""),
  pos = 3, cex = CeX, col = colour_naive
)
legend(
  "topleft",
  legend = c(
    "Start point",
    "Naive",
    "Multivariate optimal transport",
    "Seq. T.: Correct DAG",
    latex2exp::TeX("Seq. T.: Assuming $X_1 \\rightarrow X_2$"),
    latex2exp::TeX("Seq. T.: Assuming $X_2 \\rightarrow X_1$")
  ),
  col = c(
    colour_start,
    colour_naive,
    colour_ot,
    colour_correct,
    colour_inc_x1_then_x2,
    colour_inc_x2_then_x1
  ),
  pch = c(19, 19, 15, 19, 19, 19),
  lty = c(NA, NA, 2, 1, 1, 1), bty = "n"
)

## Figure (19), Appendix E----

tb_transpoort_ot <- counterfactuals_ot |>
  slice(1:nrow(D_SXY)) |> # remove last observation: this is the new point
  mutate(S = D_SXY$S) |>
  filter(S == 0) |> select(-S)

H1_OT <- Hpi(tb_transpoort_ot)
H1_indep_correct <- Hpi(as_tibble(trans_indep_correct$transported))
H1_indep_inc_x1_then_x2 <- Hpi(as_tibble(trans_indep_inc_x1_then_x2$transported))
H1_indep_inc_x2_then_x1 <- Hpi(as_tibble(trans_indep_inc_x2_then_x1$transported))
f1_2d_OT <- kde(tb_transpoort_ot, H = H1_OT, xmin = c(-5, -5), xmax = c(5, 5))
f1_2d_indep_correct <- kde(
  as_tibble(trans_indep_correct$transported),
  H = H1_indep_correct, xmin = c(-5, -5), xmax = c(5, 5)
)
f1_2d_indep_inc_x1_then_x2 <- kde(
  as_tibble(trans_indep_inc_x1_then_x2$transported),
  H = H1_indep_inc_x1_then_x2, xmin = c(-5, -5), xmax = c(5, 5)
)
f1_2d_indep_inc_x2_then_x1 <- kde(
  as_tibble(trans_indep_inc_x2_then_x1$transported),
  H = H1_indep_inc_x2_then_x1, xmin = c(-5, -5), xmax = c(5, 5)
)

x_lim <- c(-.5, 2.5)
y_lim <- c(-.25, 3)

# Plotting densities
par(mar = c(2,2,0,0), mfrow = c(2,2))
# Group S=0
contour(
  f0_2d$eval.point[[1]], f0_2d$eval.point[[2]], f0_2d$estimate,
  col = colours["A"], axes = FALSE, xlab = "", ylab = "",
  xlim = x_lim, ylim = y_lim
)
axis(1)
axis(2)
# Group S=1
contour(
  f1_2d$eval.point[[1]], f1_2d$eval.point[[2]], f1_2d$estimate,
  col = colours["B"], add = TRUE
)

# Group S=1, Optimal transport
contour(
  f1_2d_OT$eval.point[[1]], f1_2d_OT$eval.point[[2]], f1_2d_OT$estimate,
  col = colour_ot, add = TRUE
)
legend(
  "topleft", legend = c("Obs S=0", "Obs S=1", "OT"),
  lty=1,
  col = c(colours["A"], colours["B"], colour_ot), bty="n"
)



# Group S=1, Sequential transport, correct DAG
# par(mar = c(2,2,0,0))
# Group S=0
contour(
  f0_2d$eval.point[[1]], f0_2d$eval.point[[2]], f0_2d$estimate,
  col = colours["A"], axes = FALSE, xlab = "", ylab = "",
  xlim = x_lim, ylim = y_lim
)
axis(1)
axis(2)
# Group S=1
contour(
  f1_2d$eval.point[[1]], f1_2d$eval.point[[2]], f1_2d$estimate,
  col = colours["B"], add = TRUE
)
contour(
  f1_2d_indep_correct$eval.point[[1]], f1_2d_indep_correct$eval.point[[2]],
  f1_2d_indep_correct$estimate,
  col = colour_correct, add = TRUE
)
legend(
  "topleft", legend = c("Obs S=0", "Obs S=1", "Seq. T: Correct DAG"),
  lty=1,
  col = c(colours["A"], colours["B"], colour_correct), bty="n"
)


# Group S=1, Sequential transport, Wrong DAG: assuming X2 depends on X1
# par(mar = c(2,2,0,0))
# Group S=0
contour(
  f0_2d$eval.point[[1]], f0_2d$eval.point[[2]], f0_2d$estimate,
  col = colours["A"], axes = FALSE, xlab = "", ylab = "",
  xlim = x_lim, ylim = y_lim
)
axis(1)
axis(2)
# Group S=1
contour(
  f1_2d$eval.point[[1]], f1_2d$eval.point[[2]], f1_2d$estimate,
  col = colours["B"], add = TRUE
)
contour(
  f1_2d_indep_inc_x1_then_x2$eval.point[[1]],
  f1_2d_indep_inc_x1_then_x2$eval.point[[2]],
  f1_2d_indep_inc_x1_then_x2$estimate,
  col = colour_inc_x1_then_x2, add = TRUE
)
legend(
  "topleft", legend = c(
    "Obs S=0", "Obs S=1",
    latex2exp::TeX("Seq. T.: Assuming $X_1 \\rightarrow X_2$")
  ),
  lty=1,
  col = c(colours["A"], colours["B"], colour_inc_x1_then_x2), bty="n"
)

# Group S=1, Sequential transport, Wrong DAG: assuming X1 depends on X2
# par(mar = c(2,2,0,0))
# Group S=0
contour(
  f0_2d$eval.point[[1]], f0_2d$eval.point[[2]], f0_2d$estimate,
  col = colours["A"], axes = FALSE, xlab = "", ylab = "",
  xlim = x_lim, ylim = y_lim
)
axis(1)
axis(2)
# Group S=1
contour(
  f1_2d$eval.point[[1]], f1_2d$eval.point[[2]], f1_2d$estimate,
  col = colours["B"], add = TRUE
)
contour(
  f1_2d_indep_inc_x2_then_x1$eval.point[[1]],
  f1_2d_indep_inc_x2_then_x1$eval.point[[2]],
  f1_2d_indep_inc_x2_then_x1$estimate,
  col = colour_inc_x2_then_x1, add = TRUE
)
legend(
  "topleft", legend = c(
    "Obs S=0", "Obs S=1",
    latex2exp::TeX("Seq. T.: Assuming $X_2 \\rightarrow X_1$")
  ),
  lty=1,
  col = c(colours["A"], colours["B"], colour_inc_x2_then_x1), bty="n"
)

