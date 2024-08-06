# Gaussian Simulations
# Demonstration of sequential transport methodology applied to bivariate
# Gaussian distributions.
# Objective: transport X = (X_1, X_2) from group S=0 to S=1

# Required packages
library(tidyverse)
library(igraph)
library(scales)
# library(expm)
library(ks)
library(pbapply)
library(parallel)

# Graphs
colours <- c(
  `0` = "#5BBCD6",
  `1` = "#FF0000",
  A = "#00A08A",
  B = "#F2AD00",
  with = "#046C9A",
  without = "#C93312",
  `2` = "#0B775E"
)

# Seed
set.seed(123)

source("../functions/utils.R")
source("../functions/graphs.R")

op <- par()

# Data----

# Data
# Number of observations per group
n <- 100

# First bivariate Gaussian distribution: group S=0
M0 <- c(-1, -1)
S0 <- matrix(c(1, .5, .5, 1) * 1.2^2, 2, 2)
X0 <- mnormt::rmnorm(n, M0, S0)
D_SXY_0 <- data.frame(
  S = 0,
  X1 = X0[, 1],
  X2 = X0[, 2]
)

# Second bivariate Gaussian distribution: group S=1
M1 <- c(1.5,1.5)
S1 <- matrix(c(1, -.4, -.4, 1) * .9^2, 2, 2)
X1 <- mnormt::rmnorm(n, M1, S1)
D_SXY_1 <- data.frame(
  S = 1,
  X1 = X1[, 1],
  X2 = X1[, 2]
)

# Binary response variable
# Drawing random binary response variable Y with logistic model for each group
eta_0 <- (D_SXY_0$X1 * 1.2 + D_SXY_0$X2 / 2 * .8) / 2
eta_1 <- (D_SXY_1$X1 * .8 + D_SXY_1$X2 / 2 * 1.2) / 2
p_0 <- exp(eta_0) / (1 + exp(eta_0))
p_1 <- exp(eta_1) / (1 + exp(eta_1))
D_SXY_0$Y <- rbinom(n, size = 1, prob = p_0)
D_SXY_1$Y <- rbinom(n, size = 1, prob = p_1)

# Final dataframe
D_SXY <- rbind(D_SXY_0, D_SXY_1)

# Densities and point of interest (to be transported)
D_SXY0 <- D_SXY[D_SXY$S == 0, ]
D_SXY1 <- D_SXY[D_SXY$S == 1, ]

# Computation of smoothing parameters (bandwidth) for kernel density estimation
H0 <- Hpi(D_SXY0[, c("X1", "X2")])
H1 <- Hpi(D_SXY1[, c("X1", "X2")])

# Calculating multivariate densities in each group S=0 and S=1
f0_2d <- kde(D_SXY0[, c("X1", "X2")], H = H0, xmin = c(-5, -5), xmax = c(5, 5))
f1_2d <- kde(D_SXY1[, c("X1", "X2")], H = H1, xmin = c(-5, -5), xmax = c(5, 5))

# Plotting densities
par(mar = c(2,2,0,0))
# Group S=0
contour(
  f0_2d$eval.point[[1]], f0_2d$eval.point[[2]], f0_2d$estimate,
  col = colours["A"], axes = FALSE, xlab = "", ylab = ""
)
axis(1)
axis(2)
# Group S=1
contour(
  f1_2d$eval.point[[1]], f1_2d$eval.point[[2]], f1_2d$estimate,
  col = colours["B"], add = TRUE
)
# Display one individual on the graph in group S=0
points(-2, -1, pch = 19)

# Sequential Transport----

## Transport of X1----

# Uniform weights: density of X1 for subset S=0
w0 <- rep(1,length(D_SXY0[, "X1"])) / length(D_SXY0[, "X1"])
d0x1 <- function(x) {
  dens <- density(
    D_SXY0[, "X1"],
    bw = .2,
    weights = w0,
    from = x, to = x, n = 1
  )
  dens$y
}
# Uniform weights: density of X1 for subset S=1
w1 <- rep(1,length(D_SXY1[, "X1"])) / length(D_SXY1[, "X1"])
d1x1 <- function(x) {
  dens <- density(
    D_SXY1[, "X1"],
    bw = .2,
    weights = w1,
    from = x, to = x, n = 1
  )
  dens$y
}

# cdf of X1 for subset S=0
FD0x1 <- function(x, denom = NULL) {
  x_val <- seq(-10, x, by = .0025)
  if (is.null(denom)) {
    x_val_2 <- seq(-10, 10, by = .0025)
    denom <- sum(map_dbl(x_val_2, d0x1))
  }
  sum(map_dbl(x_val, d0x1)) / denom
}
FD0x1_denom <- map_dbl(seq(-10, 10, by = .0025), d0x1) |> sum()

# cdf of X1 for subset S=1
FD1x1 <- function(x, denom = NULL) {
  x_val <- seq(-10, x, by = .0025)
  if (is.null(denom)) {
    x_val_2 <- seq(-10, 10, by = .0025)
    denom <- sum(map_dbl(x_val_2, d1x1))
  }
  sum(map_dbl(x_val, d1x1)) / denom
}
FD1x1_denom <- map_dbl(seq(-10, 10, by = .0025), d1x1) |> sum()

# Definition of the inverse function
inverse <- function (f, lower = -100, upper = 100, denom = NULL) {
  function (y) as.numeric(
    uniroot((function (x) f(x, denom = denom) - y), lower = lower, upper = upper)[1]
  )
}
# Quantile function of X1 for subset S=1
QD1x1 <- inverse(FD1x1, -10, 10, denom = FD1x1_denom)

# Transport function for X1
T_X1 <- function(x) as.numeric(QD1x1(FD0x1(x, denom = FD0x1_denom)))

# We will transport a vector of values:
x1_grid_t <- seq(-3.5, 1.75, length = 101)

# This code is not run in this document, it was run before
ncl <- detectCores()-1
(cl <- makeCluster(ncl))
clusterEvalQ(cl, {
  library(purrr)
}) |>
  invisible()
clusterExport(cl, c("w0" ,"w1", "D_SXY0", "D_SXY1", "FD0x1_denom", "FD1x1_denom"))
clusterExport(cl, c("FD0x1", "FD1x1", "d0x1", "d1x1", "inverse", "QD1x1"))

x1_star_grid_t <-
  pblapply(x1_grid_t, T_X1, cl = cl) |>
  list_c()

stopCluster(cl)

save(x1_star_grid_t, file = "../data/x1_grid_transport.rda")

# load("../data/x1_grid_transport.rda")

# Calculation of densities with grid values for X1
x1_grid_d <- seq(-5, 5, length = 251)
density_x1_0 <- map_dbl(x1_grid_d, d0x1)
d_0 <- data.frame(x = x1_grid_d, y = density_x1_0)
# Density of X1 in subset 1
density_x1_1 <- map_dbl(x1_grid_d, d1x1)
d_1 <- data.frame(x = x1_grid_d, y = density_x1_1)

# Individual from subset S=0 to transport
x1 <- -2
x2 <- -1
# Transported value for x1 for that individual
x1_star <- T_X1(x1)

# Define indices of X1 grid
idx1 <- which(d_0$x <= x1)
idx1_star <- which(d_0$x <= x1_star)

### Figure 8 (top right) in the paper----
# Graph parameters
par(mar = c(2,2,0,0))
limA <- c(-5, 5)
limB <- c(-5, 5)
limY <- c(0, .5)
lab <- c("A", "B")
sub <- 6
{
  mat <- matrix(c(1, 2, 0, 3), 2)
  par(mfrow = c(2, 2))
  layout(mat, c(3.5, 1), c(1, 3))
  par(mar = c(0.5, 4.5, 0.5, 0.5))
}

# Plot density of X1 in subset S=0
plot(
  d_0$x, d_0$y, type = "l", col = colours[lab[1]], lwd = 2,
  axes = FALSE, xlab = "", ylab = "", xlim = limA, ylim = limY
)
polygon(
  c(min(d_0$x), d_0$x, max(d_0$x)), c(0, d_0$y, 0),
  col = scales::alpha(colours[lab[1]], 0.1), border = NA
)
# Plot cdf of X1 in subset S=0
polygon(
  c(min(d_0$x), d_0$x[idx1], max(d_0$x[idx1])),
  c(0, d_0$y[idx1], 0),
  col = scales::alpha(colours["A"], .2),
  border = NA
)
# Add x-axis for density and cdf
axis(
  1, at = seq(limA[1], limA[2], length = sub),
  label = c(NA, seq(limA[1], limA[2], length = sub)[-1])
)

# Plot transport line on X1 from S=0 to S=1
par(mar = c(4.5, 4.5, 0.5, 0.5))
plot(
  x1_grid_t, x1_star_grid_t, col = colours["1"], lwd = 2,
  type = "l", xlab = "", ylab = "", xlim = limA, ylim = limB,
  axes = FALSE
)
# Identity function line
abline(a = 0, b = 1, col = colours["0"], lty = 2)

# Add x-axis and y-axis of the transport line
axis(1)
axis(2)
# Legend
mtext("distribution (group 0)", side = 1, line = 3, col = "black")
mtext("distribution (group 1)", side = 2, line = 3, col = "black")

# Plot individual from subset S=0
points(x1, x1_star, pch = 19, col = colours["1"])
segments(x1, x1_star, x1, 10, lwd = .4, col = colours["1"])
segments(x1, x1_star, 10, x1_star, lwd = .4, col = colours["1"])

# Plot density of X1 in subset S=1
par(mar = c(4.5, 0.5, 0.5, 0.5))
plot(
  d_1$y, d_1$x, type = "l", col = colours[lab[2]], lwd = 2,
  ylim = limB, xlim = limY, xlab = "", ylab = "", axes = FALSE
)
polygon(
  c(0, d_1$y, 0), c(min(d_1$x), d_1$x, max(d_1$x)),
  col = scales::alpha(colours[lab[2]], 0.1), border = NA
)
# Plot cdf of X1 in subset S=1
polygon(
  c(0, d_1$y[idx1_star], 0),
  c(min(d_1$x), d_1$x[idx1_star], max(d_1$x[idx1_star])),
  col = scales::alpha(colours["B"], .2),
  border = NA
)
# Add y-axis for density and cdf
axis(
  2, at = seq(limB[1], limB[2], length = sub),
  label = c(NA, seq(limB[1], limB[2], length = sub)[-c(1, sub)], NA)
)

par(op)


### Figure 8 (top left) in the paper----
# Graph parameters
par(mar = c(2,2,0,0))

# Contour line of bivariate Gaussian density for subset S=0
contour(
  f0_2d$eval.point[[1]], f0_2d$eval.point[[2]], f0_2d$estimate,
  col = scales::alpha(colours["A"], .4),
  axes = FALSE, xlab = "", ylab = ""
)
# Contour line of bivariate Gaussian density for subset S=1
contour(
  f1_2d$eval.point[[1]], f1_2d$eval.point[[2]], f1_2d$estimate,
  col = scales::alpha(colours["B"], .4), add = TRUE
)

# Add x-axis and y-axis
axis(1)
axis(2)

# Plot density of X1 for subset S=0
polygon(
  c(min(d_0$x), d_0$x, max(d_0$x)),  c(0, d_0$y, 0) * 5 - 5,
  col = scales::alpha(colours[lab[1]], 0.4),
  border = NA
)
# Add cdf of X1 for subset S=0
polygon(
  c(min(d_0$x), d_0$x[idx1], max(d_0$x[idx1])), c(0, d_0$y[idx1],0) * 5 - 5,
  col = scales::alpha(colours["A"], 0.4),
  border = NA
)

# Plot density of X1 for subset S=1
polygon(
  c(min(d_1$x), d_1$x, max(d_1$x)),
  c(0, d_1$y, 0) * 5 - 5,
  col = scales::alpha(colours[lab[2]], 0.4), border = NA
)
# Add cdf of X1 for subset S=1
polygon(
  c(min(d_1$x), d_1$x[idx1_star], max(d_1$x[idx1_star])),
  c(0, d_1$y[idx1_star],0) * 5 - 5,
  col = scales::alpha(colours["B"], .4),
  border = NA
)

# Plot example individual from subset S=0
points(x1, x2, pch = 19)
abline(v = x1, lwd = .4, col = colours["A"])
abline(v = x1_star, col = "black", lwd = .5)

## Transport of X2|X1----

# Bandwidth for Gaussian kernels
h <- 0.5
# Weighted density of X2 for subset S=0
w0 <-  dnorm(D_SXY0[, "X1"], x1, h)
w0 <- w0 / sum(w0)
d0x2cx1 <- function(x) {
  dens <- density(
    D_SXY0[, "X2"],
    bw = .2,
    weights = w0,
    from = x, to = x, n = 1
  )
  dens$y
}
# Weighted density of X2 for subset S=1
w1 <- dnorm(D_SXY0[, "X1"], x1_star, h)
w1 <- w1/sum(w1)
d1x2cx1 <- function(x) {
  dens <- density(
    D_SXY1[, "X2"],
    bw = .2,
    weights = w1,
    from = x, to = x, n = 1
  )
  dens$y
}
# cdf of X2 for subset S=0
FD0x2 <- function(x, denom = NULL) {
  x_val <- seq(-10, x, by = .0025)
  if (is.null(denom)) {
    x_val_2 <- seq(-10, 10, by = .0025)
    denom <- map_dbl(x_val_2, d0x2cx1) |> sum()
  }
  sum(map_dbl(x_val, d0x2cx1)) / denom
}
FD0x2_denom <- map_dbl(seq(-10, 10, by = .0025), d0x2cx1) |> sum()

# cdf of X2 for subset S=1
FD1x2 <- function(x, denom = NULL) {
  x_val <- seq(-10, x, by = .0025)
  if (is.null(denom)) {
    x_val_2 <- seq(-10, 10, by = .0025)
    denom <- map_dbl(x_val_2, d1x2cx1) |> sum()
  }
  sum(map_dbl(x_val, d1x2cx1)) / denom
}
FD1x2_denom <- map_dbl(seq(-10, 10, by = .0025), d1x2cx1) |> sum()

# Definition of the inverse function
inverse <- function (f, lower = -100, upper = 100, denom) {
  function (y) as.numeric(
    uniroot((function (x) f(x, denom = denom) - y), lower = lower, upper = upper)[1]
  )
}
# Quantile function of X2 for subset S=1
QD1x2 <- inverse(FD1x2, -10, 10, denom = FD1x2_denom)

# Transport function for X2|X1=x1
T_X2_c_x1 <- function(x) as.numeric(QD1x2(FD0x2(x, denom = FD0x2_denom)))

# Vector of values to transport
x2_grid_t <- seq(-3.5, 1.75, length=101)

ncl <- detectCores()-1
(cl <- makeCluster(ncl))

clusterEvalQ(cl, {
  library(purrr)
}) |>
  invisible()

clusterExport(cl, c("w0" ,"w1", "D_SXY0", "D_SXY1", "FD0x2_denom", "FD1x2_denom"))
clusterExport(cl, c("d0x2cx1", "d1x2cx1", "FD0x2", "FD1x2", "QD1x2", "FD0x2"))

x2_star_grid_t <- pblapply(x2_grid_t, T_X2_c_x1, cl = cl)

stopCluster(cl)

save(x2_star_grid_t, file = "../data/x2_grid_transport.rda")

# load("../data/x2_grid_transport.rda")

# Calculation of densities with grid values for X2
density_x2cx1_0 <- map_dbl(d_0$x, d0x2cx1)
density_x2cx1_1 <- map_dbl(d_1$x, d1x2cx1)

# Transport of the individual of interest
x2_c_x1_star <- T_X2_c_x1(x2)

# Define indices of X1 grid
idx2 <- which(d_0$x <= x2)
idx2_star <- which(d_0$x <= x2_c_x1_star)

### Figure 8 (bottom right) in the paper----
# Graph parameters
par(op)
par(mar = c(2,2,0,0))
{
  mat <- matrix(c(1, 2, 0, 3), 2)
  par(mfrow = c(2, 2))
  layout(mat, c(3.5, 1), c(1, 3))
  par(mar = c(0.5, 4.5, 0.5, 0.5))
}

# Plot density of X2|X1 in subset S=0 with weights w0
plot(
  d_0$x, density_x2cx1_0, type = "l", col = colours[lab[1]], lwd = 2,
  axes = FALSE, xlab = "", ylab = "", xlim = limA, ylim = limY
)
polygon(
  c(min(d_0$x), d_0$x, max(d_0$x)), c(0, density_x2cx1_0, 0),
  col = scales::alpha(colours[lab[1]], 0.1), border = NA
)
# Plot cdf of X1 in subset S=0 with weights w0
polygon(
  c(min(d_0$x), d_0$x[idx2], max(d_0$x[idx2])), c(0, density_x2cx1_0[idx2], 0),
  col = scales::alpha(colours["A"], .2),
  border = NA
)
# Add x-axis for cdf and density
axis(
  1, at = seq(limA[1], limA[2], length = sub),
  label = c(NA, seq(limA[1], limA[2], length = sub)[-1])
)

# Plot transport line for X2|X1 from subset S=0 to S=1
par(mar = c(4.5, 4.5, 0.5, 0.5))
plot(
  x2_grid_t, x2_star_grid_t, col = colours["1"], lwd = 2,
  type = "l", xlab = "", ylab = "", xlim = limA, ylim = limB,
  axes = FALSE
)
abline(a = 0, b = 1, col = colours["0"], lty = 2)

# Add x-axis and y-axis
axis(1)
axis(2)

# Legend
mtext("distribution (group 0)", side = 1, line = 3, col = "black")
mtext("distribution (group 1)", side = 2, line = 3, col = "black")

# Plot example individual on the transport line
points(x2, x2_c_x1_star, pch = 19, col = colours["1"])
segments(x2, x2_c_x1_star, x2, 10, lwd = .4, col = colours["1"])
segments(x2, x2_c_x1_star, 10, x2_c_x1_star, lwd = .4, col = colours["1"])

# Plot density of X2|X1 in subset S=1 with weights w1
par(mar = c(4.5, 0.5, 0.5, 0.5))
plot(
  density_x2cx1_1, d_1$x, type = "l", col = colours[lab[2]], lwd = 2,
  axes = FALSE, xlab = "", ylab = "", xlim = limY, ylim = limB
)
polygon(
  c(0, density_x2cx1_1, 0), c(min(d_1$x), d_1$x, max(d_1$x)),
  col = scales::alpha(colours[lab[2]], 0.1),
  border = NA
)
# Plot cdf of X1 in subset S=1 with weights w1
polygon(
  c(0, density_x2cx1_1[idx2_star], 0),
  c(min(d_1$x), d_1$x[idx2_star], max(d_1$x[idx2_star])),
  col = scales::alpha(colours["B"], .2),
  border = NA
)
# Add y-axis for cdf and density
axis(
  2, at = seq(limB[1], limB[2], length = sub),
  label = c(NA, seq(limB[1], limB[2], length = sub)[-c(1, sub)], NA)
)


### Figure 8 (bottom left) in the paper----
par(op)
# Graph parameters
par(mar = c(2, 2, 0, 0))

# Contour lines for bivariate density of gaussian distribution in subset S=0
contour(
  f0_2d$eval.point[[1]], f0_2d$eval.point[[2]], f0_2d$estimate,
  col = scales::alpha(colours["A"], .4), axes = FALSE, xlab = "", ylab = ""
)

# x-axis and y-axis
axis(1)
axis(2)

# Contour lines for bivariate density of gaussian distribution in subset S=1
contour(
  f1_2d$eval.point[[1]], f1_2d$eval.point[[2]], f1_2d$estimate,
  col = scales::alpha(colours["B"], .4), add = TRUE
)

# Plot the density of X2|X1 in subset S=0 with weights w0
polygon(
  c(0, density_x2cx1_0, 0)*5-2, c(-5, d_0$x, 5),
  col = scales::alpha(colours[lab[1]], 0.4),
  border = NA
)
# Plot the cdf of X2|X1 in subset S=0
polygon(
  c(0, density_x2cx1_0[idx2], 0) * 5 - 2,
  c(min(d_0$x), d_0$x[idx2], max(d_0$x[idx2])),
  col = scales::alpha(colours["A"], .4),
  border = NA
)
# Plot crossing lines (X1, X2) at example individual from subset S=0
abline(v = x1, col = colours["A"], lwd = .5)
abline(h = x2, col = colours[lab[1]], lwd = .5)
points(x1, x2, pch=19)

# Plot the density of X2|X1 in subset S=1 with weights w1
polygon(
  c(0, density_x2cx1_1, 0) * 5 + x1_star, c(-5, d_1$x, 5),
  col = scales::alpha(colours[lab[2]], 0.4),
  border = NA
)
# Plot the cdf of X2|X1 in subset S=1
polygon(
  c(0, density_x2cx1_1[idx2_star], 0) * 5 + x1_star,
  c(min(d_1$x), d_1$x[idx2_star], max(d_1$x[idx2_star])),
  col = scales::alpha(colours["B"], .4),
  border = NA
)
# Plot crossing lines (X1, X2) at example individual when transported to subset S=1
abline(v = x1_star, col = colours[lab[2]], lwd = .5)
abline(h = x2_c_x1_star, col = "black", lwd = .5)
points(x1_star, x2_c_x1_star, pch=19)


# Faster Sequential Transport----

#' Sequential transport
#'
#' @param data dataset with three columns:
#'  - S: sensitive attribute, transport from S_0 to the other group
#'  - X1: first predictor, assumed to be causally linked to S
#'  - X2: second predictor, assumed to be causally linked to S and X1
#' @param S_0 Modality called group 0 to (source distribution)
#' @param number of cells in each dimension (default to 15)
#' @param h small value added to extend the area covered by the grid (default
#'  to .2)
#' @param d neighborhood weight when conditioning by x1 (default to .5)
transport_function_2 <- function(data,
                                 S_0,
                                 n_grid = 15,
                                 h = .2,
                                 d = .5) {

  # Subset of the data: 0 for Black, 1 for White
  D_SXY_0 <- data[data$S == S_0, ]
  D_SXY_1 <- data[data$S != S_0, ]

  # Coordinates of the cells of the grid on subset of 0 (Black)
  vx1_0 <- seq(min(D_SXY_0$X1) - h, max(D_SXY_0$X1) + h, length = n_grid + 1)
  vx2_0 <- seq(min(D_SXY_0$X2) - h, max(D_SXY_0$X2) + h, length = n_grid + 1)
  # and middle point of the cells
  vx1_0_mid <- (vx1_0[2:(1+n_grid)]+vx1_0[1:(n_grid)]) / 2
  vx2_0_mid <- (vx2_0[2:(1+n_grid)]+vx2_0[1:(n_grid)]) / 2

  # Coordinates of the cells of the grid on subset of 1 (White)
  vx1_1 <- seq(min(D_SXY_1$X1) -h, max(D_SXY_1$X1) + h, length = n_grid + 1)
  vx1_1_mid <- (vx1_1[2:(1 + n_grid)] + vx1_1[1:(n_grid)]) / 2
  # and middle point of the cells
  vx2_1 <- seq(min(D_SXY_1$X2) - h, max(D_SXY_1$X2) + h, length = n_grid + 1)
  vx2_1_mid <- (vx2_1[2:(1 + n_grid)] + vx2_1[1:(n_grid)]) / 2

  # Creation of the grids for the CDF and Quantile function
  # init with NA values
  # One grid for X1 and X2, on both subsets of the data (Black/White)
  F1_0 <- F2_0 <- F1_1 <- F2_1 <- matrix(NA, n_grid, n_grid)
  Q1_0 <- Q2_0 <- Q1_1 <- Q2_1 <- matrix(NA, n_grid, n_grid)

  # Empirical CDF for X1 on subset of Black
  FdR1_0 <- Vectorize(function(x) mean(D_SXY_0$X1 <= x))
  f1_0 <- FdR1_0(vx1_0_mid)
  # Empirical CDF for X2 on subset of Black
  FdR2_0 <- Vectorize(function(x) mean(D_SXY_0$X2 <= x))
  f2_0 <- FdR2_0(vx2_0_mid)
  # Empirical CDF for X1 on subset of White
  FdR1_1 <- Vectorize(function(x) mean(D_SXY_1$X1 <= x))
  f1_1 <- FdR1_1(vx1_1_mid)
  # Empirical CDF for X2 on subset of White
  FdR2_1 <- Vectorize(function(x) mean(D_SXY_1$X2 <= x))
  f2_1 <- FdR2_1(vx2_1_mid)

  u <- (1:n_grid) / (n_grid + 1)
  # Empirical quantiles for X1 on subset of Black
  Qtl1_0 <- Vectorize(function(x) quantile(D_SXY_0$X1, x))
  q1_0 <- Qtl1_0(u)
  # Empirical quantiles for X2 on subset of Black
  Qtl2_0 <- Vectorize(function(x) quantile(D_SXY_0$X2, x))
  q2_0 <- Qtl2_0(u)
  # Empirical quantiles for X1 on subset of White
  Qtl1_1 <- Vectorize(function(x) quantile(D_SXY_1$X1, x))
  q1_1 <- Qtl1_1(u)
  # Empirical quantiles for X2 on subset of White
  Qtl2_1 <- Vectorize(function(x) quantile(D_SXY_1$X2, x))
  q2_1 <- Qtl2_1(u)

  for (i in 1:n_grid) {
    # Subset of Black individuals
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

    # Subset of White individuals
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
    Transport_x2_cond_x1 = T2_cond_x1,
    vx1_0 = vx1_0,
    vx1_0_mid = vx1_0_mid,
    vx1_1 = vx1_1,
    vx1_1_mid = vx1_1_mid,
    vx2_0 = vx2_0,
    vx2_1 = vx2_1,
    f1_0 = f1_0,
    f1_1 = f1_1,
    F2_0 = F2_0,
    F2_1 = F2_1
  )
}

# Application on the Gaussian database
n_grid <- 15
S_0 <- 0
seq_functions_grids <- transport_function_2(D_SXY, S_0 = S_0, n_grid = n_grid)

## Illustration----

### Figure 14 in the Appendix----
# Grid for X1 in subset S=0
vx1_0 <- seq_functions_grids$vx1_0
# Midpoints of the cells
vx1_0_mid <- seq_functions_grids$vx1_0_mid
# cdf (marginal) of X1 in subset S=0
f1_0 <- seq_functions_grids$f1_0

# Grid for X1 in subset S=1
vx1_1 <- seq_functions_grids$vx1_1
# Midpoints of the cells
vx1_1_mid <- seq_functions_grids$vx1_1_mid
# cdf (marginal) of X1 in subset S=1
f1_1 <- seq_functions_grids$f1_1

# Add x-axis
par(op)
par(mar = c(2,2,0,0))
plot(
  d_0$x, d_0$x*0, xlab = "", ylab = "",
  axes = FALSE, col = NA, ylim = c(.5, 2.5)
)
axis(1)
# Plot the cdf of X1 in subset S=0 with color scale
for (i in 1:n_grid) {
  rect(vx1_0[i], .7, vx1_0[i+1], 0.6,
       col = scales::alpha(colours["A"], f1_0[i]),
       border = "grey"
  )
}
# Plot the cdf of X1 in subset S=1 with color scale
for (i in 1:n_grid) {
  rect(
    vx1_1[i], .8, vx1_1[i+1], .9,
    col = scales::alpha(colours["B"], f1_1[i]),
    border = "grey"
  )
}

### Figure 15 (left) in the paper----
# Grid for X2 in subset S=0
vx2_0 <- seq_functions_grids$vx2_0
# cdf (conditional) of X2|X1 in subset S=0 (matrix)
F2_0 <- seq_functions_grids$F2_0

# Grid for X2 in subset S=1
vx2_1 <- seq_functions_grids$vx2_1
# cdf (conditional) of X2|X1 in subset S=1 (matrix)
F2_1 <- seq_functions_grids$F2_1

par(op)
# Add x-axis and y-axis (for the matrices with cdf's)
par(mar = c(2,2,0,0))
plot(d_0$x, d_0$x, xlab = "", ylab = "", axes = FALSE, col = NA)
axis(1)
axis(2)

# Add grid matrix of cdf's for X2|X1 in subset S=0
for (i in 1:n_grid) {
  for(j in 1:n_grid) {
    rect(vx1_0[i], vx2_0[j], vx1_0[i+1], vx2_0[j+1], border="grey")
  }
}
# Column corresponding to X1=x1
i <- which.min(abs(vx1_0_mid-x1))
for (j in 1:n_grid) {
  rect(
    vx1_0[i], vx2_0[j], vx1_0[i+1], vx2_0[j+1],
    col = scales::alpha(colours["A"], F2_0[j,i]),
    border="grey"
  )
}

### Figure 15 (right) in the appendix----
# Add x-axis and y-axis (for the matrices with cdf's)
par(op)
par(mar = c(2,2,0,0))
plot(d_1$x, d_1$x, xlab = "", ylab = "", axes = FALSE, col = NA)
axis(1)
axis(2)

# Add grid matrix of cdf's for X2|X1 in subset S=1
for (i in 1:n_grid) {
  for (j in 1:n_grid) {
    rect(vx1_1[i], vx2_1[j], vx1_1[i + 1], vx2_1[j + 1], border = "grey")
  }
}
# Column corresponding to X1=x1_star (i.e., x1 transported)
i <- which.min(abs(vx1_1_mid - x1_star))
for (j in 1:n_grid) {
  rect(
    vx1_1[i], vx2_1[j], vx1_1[i+1], vx2_1[j+1],
    col = scales::alpha(colours["B"], F2_1[j,i]),
    border = "grey"
  )
}


## Application to the data----

### Transport of X1----

T_X1 <- seq_functions_grids$Transport_x1
# Calculation of transport line for different values of X1 (grid)
x1_grid_t <- seq(-3.5, 1.75, length = 251)
x1_star_grid_t <- Vectorize(T_X1)(x1_grid_t)
# Transported x1
x1_star <- T_X1(x1)

# Define indices of X1 grid
idx1 <- which(d_0$x <= x1)
idx1_star <- which(d_0$x <= x1_star)

#### Equivalent of Figure 8 (top right) but with faster transport----
# Graph parameters
par(mar = c(2,2,0,0))
limA <- c(-5, 5)
limB <- c(-5, 5)
limY <- c(0, .5)
lab <- c("A", "B")
sub <- 6
{
  mat <- matrix(c(1, 2, 0, 3), 2)
  par(mfrow = c(2, 2))
  layout(mat, c(3.5, 1), c(1, 3))
  par(mar = c(0.5, 4.5, 0.5, 0.5))
}

# Plot density of X1 in subset S=0
plot(
  d_0$x, d_0$y, type = "l", col = colours[lab[1]], lwd = 2,
  axes = FALSE, xlab = "", ylab = "", xlim = limA, ylim = limY
)
polygon(
  c(min(d_0$x), d_0$x, max(d_0$x)), c(0, d_0$y, 0),
  col = scales::alpha(colours[lab[1]], 0.1), border = NA
)
# Plot cdf of X1 in subset S=0
polygon(
  c(min(d_0$x), d_0$x[idx1], max(d_0$x[idx1])),
  c(0,d_0$y[idx1],0),
  col = scales::alpha(colours["A"], .2),
  border = NA
)
# Add x-axis for density and cdf
axis(
  1, at = seq(limA[1], limA[2], length = sub),
  label = c(NA, seq(limA[1], limA[2], length = sub)[-1])
)

# Plot transport line on X1 from S=0 to S=1
par(mar = c(4.5, 4.5, 0.5, 0.5))
u_grid <- seq(0, 1, length=261)
u_grid <- u_grid[2:260]
plot(
  x1_grid_t, x1_star_grid_t, col = colours["1"], lwd = 2,
  type = "l", xlab = "", ylab = "", xlim = limA, ylim = limB,
  axes = FALSE
)
# Identity function line
abline(a = 0, b = 1, col = colours["0"], lty = 2)

# Add x-axis and y-axis of the transport line
axis(1)
axis(2)
# Legend
mtext("distribution (group 0)", side = 1, line = 3, col = "black")
mtext("distribution (group 1)", side = 2, line = 3, col = "black")

# Plot individual from subset S=0
points(x1, x1_star, pch = 19, col = colours["1"])
segments(x1, x1_star, x1, 10, lwd = .4, col = colours["1"])
segments(x1, x1_star, 10, x1_star, lwd = .4, col = colours["1"])

# Plot density of X1 in subset S=1
par(mar = c(4.5, 0.5, 0.5, 0.5))
plot(
  d_1$y, d_1$x, type = "l", col = colours[lab[2]], lwd = 2,
  ylim = limB, xlim = limY, xlab = "", ylab = "", axes = FALSE
)
polygon(
  c(0, d_1$y, 0), c(min(d_1$x), d_1$x, max(d_1$x)),
  col = scales::alpha(colours[lab[2]], 0.1), border = NA
)
# Plot cdf of X1 in subset S=1
polygon(
  c(0, d_1$y[idx1_star], 0),
  c(min(d_1$x), d_1$x[idx1_star], max(d_1$x[idx1_star])),
  col = scales::alpha(colours["B"], .2),
  border = NA
)
# Add y-axis for density and cdf
axis(
  2, at = seq(limB[1], limB[2], length = sub),
  label = c(NA, seq(limB[1], limB[2], length = sub)[-c(1, sub)], NA)
)

#### Equivalent of Figure 8 (top left) but with faster transport----
par(op)
# Graph parameters
par(mar = c(2,2,0,0))

# Contour line of bivariate Gaussian density for subset S=0
contour(
  f0_2d$eval.point[[1]], f0_2d$eval.point[[2]],
  f0_2d$estimate, col = scales::alpha(colours["A"],.4),
  axes = FALSE, xlab = "", ylab = ""
)
# Contour line of bivariate Gaussian density for subset S=1
contour(
  f1_2d$eval.point[[1]], f1_2d$eval.point[[2]], f1_2d$estimate,
  col = scales::alpha(colours["B"], .4),
  add = TRUE
)

# Add x-axis and y-axis
axis(1)
axis(2)

# Plot density of X1 for subset S=0
polygon(
  c(min(d_0$x), d_0$x, max(d_0$x)),
  c(0, d_0$y, 0)*5-5,
  col = scales::alpha(colours[lab[1]], 0.4),
  border = NA
)
# Add cdf of X1 for subset S=0
polygon(
  c(min(d_0$x), d_0$x[idx1], max(d_0$x[idx1])),
  c(0, d_0$y[idx1],0) * 5 - 5,
  col = scales::alpha(colours["A"], 0.4),
  border = NA
)

# Plot density of X1 for subset S=1
polygon(
  c(min(d_1$x), d_1$x, max(d_1$x)),
  c(0, d_1$y, 0)*5-5,
  col = scales::alpha(colours[lab[2]], 0.4), border = NA
)
# Add cdf of X1 for subset S=1
polygon(
  c(min(d_1$x), d_1$x[idx1_star], max(d_1$x[idx1_star])),
  c(0, d_1$y[idx1_star],0) * 5 - 5,
  col = scales::alpha(colours["B"], .4),
  border = NA
)

# Plot example individual from subset S=0
points(x1, x2, pch = 19)
abline(v = x1, lwd = .4, col = colours["A"])
abline(v = x1_star, col = "black", lwd = .5)

### Transport of X2----

# Transport function for X2|X1=x1
T_X2_c_x1 <- T_X1 <- seq_functions_grids$Transport_x2_cond_x1
# Calculation of transport line for different values of X2|X1=x1 (grid)
x2_grid_t <- seq(-3.5, 1.75, length = 251)
x2_star_grid_t <- Vectorize(function(x) T_X2_c_x1(x, x1))(x2_grid_t)
# Transported value for individual of interest
x2_c_x1_star <- T_X2_c_x1(x2, x1)

#### Equivalent of Figure 8 (bottom right) but with faster transport----
par(op)
# Graph parameters
{
  mat <- matrix(c(1, 2, 0, 3), 2)
  par(mfrow = c(2, 2))
  layout(mat, c(3.5, 1), c(1, 3))
  par(mar = c(0.5, 4.5, 0.5, 0.5))
}

# Plot density of X2|X1 in subset S=0 with weights w0
plot(
  d_0$x, density_x2cx1_0, type = "l", col = colours[lab[1]], lwd = 2,
  axes = FALSE, xlab = "", ylab = "", xlim = limA, ylim = limY
)
polygon(
  c(min(d_0$x), d_0$x, max(d_0$x)), c(0, density_x2cx1_0, 0),
  col = scales::alpha(colours[lab[1]], 0.1), border = NA
)
# Plot cdf of X1 in subset S=0 with weights w0
polygon(
  c(min(d_0$x), d_0$x[idx2], max(d_0$x[idx2])),
  c(0, density_x2cx1_0[idx2], 0),
  col = scales::alpha(colours["A"], .2),
  border = NA
)
# Add x-axis for cdf and density
axis(
  1, at = seq(limA[1], limA[2], length = sub),
  label = c(NA, seq(limA[1], limA[2], length = sub)[-1])
)

# Plot transport line for X2|X1 from subset S=0 to S=1
par(mar = c(4.5, 4.5, 0.5, 0.5))
plot(
  x2_grid_t, x2_star_grid_t, col = colours["1"], lwd = 2,
  type = "l", xlab = "", ylab = "", xlim = limA, ylim = limB,
  axes = FALSE
)
abline(a = 0, b = 1, col = colours["0"], lty = 2)

# Add x-axis and y-axis
axis(1)
axis(2)

# Legend
mtext("distribution (group 0)", side = 1, line = 3, col = "black")
mtext("distribution (group 1)", side = 2, line = 3, col = "black")

# Plot example individual on the transport line
points(x2, x2_c_x1_star, pch = 19, col = colours["1"])
segments(x2, x2_c_x1_star, x2, 10, lwd = .4, col = colours["1"])
segments(x2, x2_c_x1_star, 10, x2_c_x1_star, lwd = .4, col = colours["1"])

# Plot density of X2|X1 in subset S=1 with weights w1
par(mar = c(4.5, 0.5, 0.5, 0.5))
plot(
  density_x2cx1_1, d_1$x, type = "l", col = colours[lab[2]], lwd = 2,
  axes = FALSE, xlab = "", ylab = "", xlim = limY, ylim = limB
)
polygon(
  c(0, density_x2cx1_1, 0), c(min(d_1$x), d_1$x, max(d_1$x)),
  col = scales::alpha(colours[lab[2]], 0.1),
  border = NA
)
# Plot cdf of X1 in subset S=1 with weights w1
polygon(
  c(0, density_x2cx1_1[idx2_star], 0),
  c(min(d_1$x), d_1$x[idx2_star], max(d_1$x[idx2_star])),
  col = scales::alpha(colours["B"], .2),
  border = NA
)
# Add y-axis for cdf and density
axis(
  2, at = seq(limB[1], limB[2], length = sub),
  label = c(NA, seq(limB[1], limB[2], length = sub)[-c(1, sub)], NA)
)

#### Equivalent of Figure 8 (bottom left) but with faster transport----
par(op)
# Graph parameters
par(mar = c(2, 2, 0, 0))

# Contour lines for bivariate density of gaussian distribution in subset S=0
contour(
  f0_2d$eval.point[[1]], f0_2d$eval.point[[2]], f0_2d$estimate,
  col = scales::alpha(colours["A"],.4),
  axes = FALSE, xlab = "", ylab = ""
)

# x-axis and y-axis
axis(1)
axis(2)

# Contour lines for bivariate density of gaussian distribution in subset S=1
contour(
  f1_2d$eval.point[[1]], f1_2d$eval.point[[2]], f1_2d$estimate,
  col = scales::alpha(colours["B"],.4), add = TRUE
)

# Plot the density of X2|X1 in subset S=0 with weights w0
polygon(
  c(0, density_x2cx1_0, 0)*5-2, c(-5, d_0$x, 5),
  col = scales::alpha(colours[lab[1]], 0.4),
  border = NA
)
# Plot the cdf of X2|X1 in subset S=0
polygon(
  c(0, density_x2cx1_0[idx2], 0)*5-2,
  c(min(d_0$x), d_0$x[idx2], max(d_0$x[idx2])),
  col = scales::alpha(colours["A"],.4),
  border = NA
)
# Plot crossing lines (X1, X2) at example individual from subset S=0
abline(v = x1, col = colours["A"], lwd = .5)
abline(h = x2, col = colours[lab[1]], lwd = .5)
points(x1, x2, pch=19)

# Plot the density of X2|X1 in subset S=1 with weights w1
polygon(
  c(0, density_x2cx1_1, 0) * 5 + x1_star, c(-5, d_1$x, 5),
  col = scales::alpha(colours[lab[2]], 0.4),
  border = NA
)
# Plot the cdf of X2|X1 in subset S=1
polygon(
  c(0, density_x2cx1_1[idx2_star], 0) * 5 + x1_star,
  c(min(d_1$x), d_1$x[idx2_star], max(d_1$x[idx2_star])),
  col = scales::alpha(colours["B"],.4),
  border = NA
)
# Plot crossing lines (X1, X2) at example individual when transported to subset S=1
abline(v = x1_star, col = colours[lab[2]], lwd = .5)
abline(h = x2_c_x1_star, col = "black", lwd = .5)
points(x1_star, x2_c_x1_star, pch=19)
