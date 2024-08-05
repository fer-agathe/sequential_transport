# Regression
# Demonstration of interpretable counterfactual fairness methodology based on
# sequantial transport, using simulated data

library(tidyverse)
library(ks)
library(dichromat)
colours <- c(
  `0` = "#5BBCD6",
  `1` = "#FF0000",
  A = "#00A08A",
  B = "#F2AD00",
  with = "#046C9A",
  without = "#C93312",
  `2` = "#0B775E"
)
# Colour scale from colour of class 0 to class 1
colfunc <- colorRampPalette(c(colours["0"], colours["1"]))
scl <- scales::alpha(colfunc(9),.9)

# Data Generation----
# Number of observations per group
set.seed(123) # set the seed for reproductible results
n <- 100

# First bivariate Gaussian distribution: group s=0
M0 <- c(-1, -1)
S0 <- matrix(c(1, .5, .5,1) * 1.2^2, 2, 2)
X0 <- mnormt::rmnorm(n, M0, S0)
D_SXY_0 <- data.frame(
  S = 0,
  X1 = X0[, 1],
  X2 = X0[, 2]
)

# Second bivariate Gaussian distribution: group s=1
M1 <- c(1.5, 1.5)
S1 <- matrix(c(1, -.4, -.4, 1) * .9^2, 2, 2)
X1 <- mnormt::rmnorm(n, M1, S1)
D_SXY_1 <- data.frame(
  S = 1,
  X1 = X1[,1],
  X2 = X1[,2]
)

# Drawing random binary response variable Y with logistic model for each group
eta_0 <- (D_SXY_0$X1 * 1.2 + D_SXY_0$X2 / 2 * .8) / 2
eta_1 <- (D_SXY_1$X1 * .8 + D_SXY_1$X2 / 2 * 1.2) / 2
p_0 <- exp(eta_0) / (1 + exp(eta_0))
p_1 <- exp(eta_1) / (1 + exp(eta_1))
D_SXY_0$Y <- rbinom(n, size = 1, prob = p_0)
D_SXY_1$Y <- rbinom(n, size = 1, prob = p_1)

# Datasets
D_SXY <- rbind(D_SXY_0, D_SXY_1)
# Dataset with individuals in group 0 only
D_SXY0 <- D_SXY[D_SXY$S == 0, ]
# Dataset with individuals in group 1 only
D_SXY1 <- D_SXY[D_SXY$S == 1,]


# Computation of smoothing parameters (bandwidth) for kernel density estimation
H0 <- Hpi(D_SXY0[, c("X1","X2")])
H1 <- Hpi(D_SXY1[, c("X1","X2")])

# Calculating multivariate densities in each group
f0_2d <- kde(D_SXY0[, c("X1","X2")], H = H0, xmin = c(-5, -5), xmax = c(5, 5))
f1_2d <- kde(D_SXY1[, c("X1","X2")], H = H1, xmin = c(-5, -5), xmax = c(5, 5))

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

# Grid----
n_grid <- 500
h <- .2

# Group 0
## First dimension (x1)
vx1_0 <- seq(
  min(D_SXY_0$X1) - h,
  max(D_SXY_0$X1) + h,
  length = n_grid + 1
)
## Second dimension (x2)
vx2_0 <- seq(
  min(D_SXY_0$X2) - h,
  max(D_SXY_0$X2) + h,
  length = n_grid + 1
)
# Group 1
## First dimension (x1)
vx1_1 <- seq(
  min(D_SXY_1$X1) - h,
  max(D_SXY_1$X1) + h,
  length = n_grid + 1
)
## Second dimension (x2)
vx2_1 <- seq(
  min(D_SXY_1$X2) - h,
  max(D_SXY_1$X2) + h,
  length = n_grid + 1
)

# Middle of each cell
# Group 1
vx1_0_mid <- (vx1_0[2:(1 + n_grid)] + vx1_0[1:(n_grid)]) / 2
vx1_1_mid <- (vx1_1[2:(1 + n_grid)] + vx1_1[1:(n_grid)]) / 2
# Group 2
vx2_0_mid <- (vx2_0[2:(1 + n_grid)] + vx2_0[1:(n_grid)]) / 2
vx2_1_mid <- (vx2_1[2:(1 + n_grid)] + vx2_1[1:(n_grid)]) / 2

# Cumulative distribution function and quantile function
F1_0 <- F2_0 <- F1_1 <- F2_1 <- matrix(NA, n_grid, n_grid)
Q1_0 <- Q2_0 <- Q1_1 <- Q2_1 <- matrix(NA, n_grid, n_grid)

u <- (1:n_grid) / (n_grid + 1)

FdR1_0 <- Vectorize(function(x) mean(D_SXY_0$X1 <= x))
f1_0 <- FdR1_0(vx1_0_mid)
FdR2_0 <- Vectorize(function(x) mean(D_SXY_0$X2 <= x))
f2_0 <- FdR2_0(vx2_0_mid)
FdR1_1 <- Vectorize(function(x) mean(D_SXY_1$X1 <= x))
f1_1 <- FdR1_1(vx1_1_mid)
FdR2_1 <- Vectorize(function(x) mean(D_SXY_1$X2<=x))
f2_1 <- FdR2_1(vx2_1_mid)

Qtl1_0 <- Vectorize(function(x) quantile(D_SXY_0$X1, x))
q1_0 <- Qtl1_0(u)
Qtl2_0 <- Vectorize(function(x) quantile(D_SXY_0$X2, x))
q2_0 <- Qtl1_0(u)
Qtl1_1 <- Vectorize(function(x) quantile(D_SXY_1$X1, x))
q1_1 <- Qtl1_1(u)
Qtl2_1 <- Vectorize(function(x) quantile(D_SXY_1$X2, x))
q2_1 <- Qtl2_1(u)

# Loop on the cells of the grid to compute c.d.f in each group, and quantiles

d <- .5 # neighbourhood parameter

for (i in 1:n_grid) {
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
  idx1_1 <- which(abs(D_SXY_1$X1 - vx1_1_mid[i]) < d)
  FdR2_1 <- Vectorize(function(x) mean(D_SXY_1$X2[idx1_1] <= x))
  F2_1[, i] <- FdR2_1(vx2_1_mid)
  Qtl2_1 <- Vectorize(function(x) quantile(D_SXY_1$X2[idx1_1], x))
  Q2_1[, i] <- Qtl2_1(u)
  idx2_1 <- which(abs(D_SXY_1$X2 - vx2_1_mid[i]) < d)
  FdR1_1 <- Vectorize(function(x) mean(D_SXY_1$X1[idx2_1] <= x))
  F1_1[, i] <- FdR1_1(vx1_1_mid)
  Qtl1_1 <- Vectorize(function(x) quantile(D_SXY_1$X1[idx2_1], x))
  Q1_1[, i] <- Qtl1_1(u)
}

## Predictions on the grid----
vx0 <- seq(-5, 5, length = 251)
data_grid <- expand.grid(x = vx0, y = vx0)
L0 <- logistique_reg(x1 = data_grid$x, x2 = data_grid$y, s = 0)
L1 <- logistique_reg(x1 = data_grid$x, x2 = data_grid$y, s = 1)

# as a grid:
dlogistique0 <- matrix(L0, length(vx0), length(vx0))
dlogistique1 <- matrix(L1, length(vx0), length(vx0))


# Functions for Sequential Transport----
#' Transport of x1 from s=0 to s=1
#'
#' @param x2 vector of x2's values
#' @descriptions
#'  - vx1_0_mid: coordinates of center of cells (axis x1, s=0)
#'  - f1_0: c.d.f. values for x1 in group s=0
#'  - u: quantile levels
#'  - q1_1: quantile values for x1 in group s=1
transport_x1 <- function(x1) {
  # identify closest cell of the grid to the coordinates of x1 in group s=0
  i <- which.min(abs(vx1_0_mid - x1))
  # c.d.f. for that cell, in group s=0
  p <- f1_0[i]
  # identify closest quantile level
  i <- which.min(abs(u - p))
  # corresponding quantile in group s=1
  x1star <- q1_1[i]
  x1star
}

#' Transport of x2 from s=0 to s=1
#'
#' @param x2 vector of x2's values
#' @descriptions
#'  - vx2_0_mid: coordinates of center of cells (axis x2, s=0)
#'  - f2_0: c.d.f. values for x2 in group s=0
#'  - u: quantile levels
#'  - q2_1: quantile values of x2 in group s=1
transport_x2 <- function(x2) {
  # identify closest cell of the grid to the coordinates of x2 in group s=0
  i <- which.min(abs(vx2_0_mid - x2))
  # c.d.f. for that cell, in group s=0
  p <- f2_0[i]
  # identify closest quantile level
  i <- which.min(abs(u - p))
  # corresponding quantile in group s=1
  x2star <- q2_1[i]
  x2star
}


#' Transport for x1 conditional on x2, from s=0 to s=1
#'
#' @param x1 numerical vector with x1's values
#' @param x2 numerical vector with x2's values
#' @description
#'  - vx2_0_mid: coordinates of center of cells (axis x2, s=0)
#'  - vx2_1_mid: coordinates of center of cells (axis x2, s=1)
#'  - vx1_0_mid: coordinates of center of cells (axis x1, s=0)
transport_x1_cond_x2 <- function(x1, x2) {
  # identify closest cell in s=0 for x2 coordinate
  k0 <- which.min(abs(vx2_0_mid - x2))
  # identify closest cell in s=1 for transported x2 coordinate
  k1 <- which.min(abs(vx2_1_mid - transport_x2(x2)))
  # identify closest cell in s=0 for x1 coordinate
  i_0 <- which.min(abs(vx1_0_mid - x1))
  # c.d.f. for the closest cell in s=0 for x1 coordinates
  p <- F1_0[i_0, k0]
  # identify closest quantile level
  i_1 <- which.min(abs(u - p))
  # corresponding quantile in group s=1
  x1star <- Q1_1[i_1, k1]
  x1star
}

#' Transport for x2 conditional on x1, from s=0 to s=1
#'
#' @param x2 numerical vector with x2's values
#' @param x1 numerical vector with x1's values
#' @description
#'  - vx1_0_mid: coordinates of center of cells (axis x1, s=0)
#'  - vx1_1_mid: coordinates of center of cells (axis x1, s=1)
#'  - vx2_0_mid: coordinates of center of cells (axis x2, s=0)
transport_x2_cond_x1 <-  function(x2, x1){
  # identify closest cell in s=0 for x1 coordinate
  k0 <- which.min(abs(vx1_0_mid - x1))
  # identify closest cell in s=1 for transported x1 coordinate
  k1 <- which.min(abs(vx1_1_mid - transport_x1(x1)))
  # identify closest cell in s=0 for x2 coordinate
  i_0 <- which.min(abs(vx2_0_mid - x2))
  # c.d.f. for the closest cell in s=0 for x2 coordinates
  p <- F2_0[i_0, k0]
  # identify closest quantile level
  i_1 <- which.min(abs(u - p))
  # corresponding quantile in group s=1
  x2star <-  Q2_1[i_1, k1]
  x2star
}

# Figures----

# Individual of interest:
xystart <- c(-2,-1)

# Predictions for that individual depending on the values (transported or not)

# First the coordinates
coords <- data.frame(
  # 1. (x1, x2)
  start = xystart,
  # 2. (T_1(x1 | x2), T_2(x2))
  x2_then_x1 = c(
    transport_x1_cond_x2(x1 = xystart[1], x2 = xystart[2]),
    transport_x2(xystart[2])
  ),
  # 3. (T_1(x1), T_2(x2 | x1))
  x1_then_x2 = c(
    transport_x1(xystart[1]),
    transport_x2_cond_x1(x2 = xystart[1], x1 = xystart[2])
  ),
  # 4. (T_1(x1), x2)
  x1_intermediaire = c(transport_x1(x1 = xystart[1]), xystart[2]),
  # 5. (x1, T_2(x2))
  x2_intermediaire = c(xystart[1], transport_x2(xystart[2]))
)

# then the predicted values:
library(plotrix)
v <- c(
  # 1. Prediction at initial values (S=0, x1, x2)
  logistique_reg(x1 = coords$start[1], x2 = coords$start[2], s = 0),
  # 2. Prediction if (S=1, x1, x2)
  logistique_reg(coords$start[1], coords$start[2], 1),
  # 3. Prediction if (S = 1, T_1(x1), T_2(x2 | x1))
  logistique_reg(coords$x1_then_x2[1], coords$x1_then_x2[2], 1),
  # 4. Prediction if (S = 1, T_1(x1 | x2), T_2(x2))
  logistique_reg(coords$x2_then_x1[1], coords$x2_then_x1[2], 1),
  # 5. Prediction if (S = 1, T_1(x1), x2)
  logistique_reg(coords$x1_intermediaire[1], coords$x1_intermediaire[2], 1),
  # 6. Prediction if (S = 1, x1, T_2(x2))
  logistique_reg(coords$x2_intermediaire[1], coords$x2_intermediaire[2], 1)
)
v

### Figure 9 (left) in the paper----
par(mar = c(2, 2, 0, 0))
# Group 0
## Estimated density: level curves for (x1, x2) -> m(0, x1, x2)
CeX <- 1
contour(
  f0_2d$eval.point[[1]],
  f0_2d$eval.point[[2]],
  f0_2d$estimate,
  col = scales::alpha(colours["A"], .3),
  axes = FALSE, xlab = "", ylab = ""
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
  levels = (1:9)/10,
  col = scl,
  add = TRUE,lwd = 2
)
axis(1)
axis(2)

###
# Individual (S=0, x1=-2, x2=-1)
###
points(coords$start[1], coords$start[2], pch = 19, cex = CeX)
## Predicted value for the individual, based on factuals
text(
  coords$start[1], coords$start[2],
  paste(round(v[1] * 100, 1), "%", sep = ""),
  pos = 1, cex = CeX, col = "darkblue"
)

### Figure 9 (right) in the paper----
par(mar = c(2, 2, 0, 0))
# Group 0
## Estimated density: level curves for (x1, x2) -> m(0, x1, x2)
contour(
  f0_2d$eval.point[[1]],
  f0_2d$eval.point[[2]],
  f0_2d$estimate,
  col = scales::alpha(colours["A"], .3),
  axes = FALSE, xlab = "", ylab = ""
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
  levels = (1:9) / 10,
  col = scl, lwd=2,
  add = TRUE
)
axis(1)
axis(2)

###
# Individual (s=0, x1=-2, x2=-1)
###
points(coords$start[1], coords$start[2], pch = 19, cex = CeX)
## Predicted value for the individual, based on factuals
text(
  coords$start[1] - .3, coords$start[2] - .42 - .3,
  paste(round(v[1] * 100, 1), "%", sep = ""),
  pos = 1, cex = CeX, col = "darkblue"
)

###
# Transported individual when transporting x1 and then x2, i.e.,
# (do(s=1), T_1^*(x1), T_2^*(x_2 | x_1))
###
points(coords$x1_then_x2[1],coords$x1_then_x2[2], pch = 19, cex = CeX)
segments(
  x0 = coords$start[1], y0 = coords$start[2],
  x1 = coords$x1_then_x2[1], y1 = coords$start[2],
  lwd = .8
)
segments(
  x0 = coords$x1_then_x2[1], y0 = coords$x1_then_x2[2],
  x1 = coords$x1_then_x2[1], y1 = coords$start[2],
  lwd = .8
)
## Intermediate point
points(coords$x1_then_x2[1], coords$start[2], pch = 19, col = "white", cex = CeX)
points(coords$x1_then_x2[1], coords$start[2], pch = 1, cex = CeX)
text(
  coords$x1_then_x2[1], coords$start[2] - .42,
  paste(round(v[5] * 100, 1), "%", sep = ""), pos = 4, cex = CeX
)
# New predicted value for # (do(s=1), T_1^*(x1), T_2^*(x_2 | x_1))
text(
  coords$x1_then_x2[1], coords$x1_then_x2[2],
  paste(round(v[3]*100,1),"%",sep=""), pos = 3, cex = CeX
)

###
# Transported individual when transporting x2 and then x1, i.e.,
# (do(s=1), T_1^*(x1 | x2), T_2^*(x_2))
###
points(coords$x2_then_x1[1],coords$x2_then_x1[2],pch=19,cex=CeX)
segments(
  x0 = coords$start[1], y0 = coords$start[2],
  x1 = coords$start[1], y1 = coords$x2_then_x1[2],
  lwd = .8
)
segments(
  x0 = coords$x2_then_x1[1], y0 = coords$x2_then_x1[2],
  x1 = coords$start[1], y1 = coords$x2_then_x1[2],
  lwd = .8
)
## Intermediate point
points(coords$start[1], coords$x2_then_x1[2], pch = 19, col = "white", cex = CeX)
points(coords$start[1], coords$x2_then_x1[2], pch = 1, cex = CeX)
text(
  coords$start[1], coords$x2_then_x1[2],
  paste(round(v[6] * 100, 1), "%", sep = ""), pos = 2, cex = CeX
)
## New predicted value for (do(s=1), T_1^*(x1 | x2), T_2^*(x_2))
text(
  coords$x2_then_x1[1], coords$x2_then_x1[2],
  paste(round(v[4] * 100, 1), "%", sep = ""), pos = 3, cex = CeX
)


###
# New predicted value for (do(s=1), x1, x2), no transport
###
ry <- .2
draw.circle(
  x = coords$start[1] - ry, y = coords$start[2] - ry,
  radius = ry * sqrt(2)
)
text(
  coords$start[1], coords$start[2] + .42,
  paste(round(v[2] * 100, 1), "%", sep = ""), pos = 4, cex = CeX
)

