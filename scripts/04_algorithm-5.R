# Faster Algorithm
# Uses Algorithm 5 (Sequential transport with weights) form Appendix E

library(tidyverse)
library(ks)
library(dichromat)
library(fairadapt)
library(expm)
library(cluster)
library(devtools)

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
n0 <- 200

# First bivariate Gaussian distribution: group s=0
M0 <- c(-1, -1)
S0 <- matrix(c(1, .5, .5,1) * 1.2^2, 2, 2)
X0 <- mnormt::rmnorm(n0, M0, S0)
D_SXY_0 <- data.frame(
  S = 0,
  X1 = X0[, 1],
  X2 = X0[, 2]
)

# Second bivariate Gaussian distribution: group s=1
n1 <- 400
M1 <- c(1.5, 1.5)
S1 <- matrix(c(1, -.4, -.4, 1) * .9^2, 2, 2)
X1 <- mnormt::rmnorm(n1, M1, S1)
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
D_SXY_0$Y <- rbinom(n0, size = 1, prob = p_0)
D_SXY_1$Y <- rbinom(n1, size = 1, prob = p_1)

# Single dataset
D_SXY <- rbind(D_SXY_0, D_SXY_1)
# Dataset per group:
# Dataset with individuals in group 0 only
D_SXY0 <- D_SXY[D_SXY$S == 0, ]
# Dataset with individuals in group 1 only
D_SXY1 <- D_SXY[D_SXY$S == 1,]

# Estimation of the bivariate distribution
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

vx0 <- seq(-5, 5, length = 251)
data_grid <- expand.grid(x = vx0, y = vx0)
L0 <- logistique_reg(x1 = data_grid$x, x2 = data_grid$y, s = 0)
L1 <- logistique_reg(x1 = data_grid$x, x2 = data_grid$y, s = 1)
# as a grid:
dlogistique0 <- matrix(L0, length(vx0), length(vx0))
dlogistique1 <- matrix(L1, length(vx0), length(vx0))

# Optimal Transport----

AA <- sqrtm(S0) %*% S1 %*% (sqrtm(S0))
AA <- solve(sqrtm(S0)) %*% sqrtm(AA) %*% solve((sqrtm(S0)))
OT <- function(x) as.vector(M1 + AA %*% (x - M0))
transport_ot <- t(apply(D_SXY_0[, c("X1", "X2")], 1, OT))
head(transport_ot)

# Sequential Transport----

# Load our package
load_all("../seqtransfairness/")

variables <- c("S", "X1", "X2", "Y")

adj <- matrix(
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

# Causal ggraph:
causal_graph <- fairadapt::graphModel(adj)
plot(causal_graph)

# Counterfactuals using algorithm 5
transported <- seq_trans(data = D_SXY, adj = adj, s = "S", S_0 = 0, y = "Y")


# New Individual----

new_obs <- tibble(X1 = -2, X2 = -1, S = 0)

## Counterfactual with OT----

new_obs_ot <- t(apply(new_obs[, c("X1", "X2")], 1, OT))
new_obs_ot

## Sequential Transport----

variables <- c("S", "X1", "X2", "S")


### X1 then X2----

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

causal_graph_1 <- fairadapt::graphModel(adj_1)
plot(causal_graph_1)

# Mapping
trans_x1_then_x2 <- seq_trans(
  data = D_SXY, adj = adj_1, s = "S", S_0 = 0, y = "Y"
)

# Transported value for the new individual
new_obs_x1_then_x2 <- seq_trans_new(
  x = trans_x1_then_x2, newdata = new_obs, data = D_SXY
)
new_obs_x1_then_x2

### X2 then X2----

adj_2 <- matrix(
  # S  X1 X2 Y
  c(0, 1, 1, 1,# S
    0, 0, 0, 1,# X1
    0, 1, 0, 1,# X2
    0, 0, 0, 0  # Y
  ),
  ncol = length(variables),
  dimnames = rep(list(variables), 2),
  byrow = TRUE
)

causal_graph_2 <- fairadapt::graphModel(adj_2)
plot(causal_graph_2)

# Mapping (and transported values)
trans_x2_then_x1 <- seq_trans(
  data = D_SXY, adj = adj_2, s = "S", S_0 = 0, y = "Y"
)

# Transport of the new individual
new_obs_x2_then_x1 <- seq_trans_new(
  x = trans_x2_then_x1, newdata = new_obs, data = D_SXY
)
new_obs_x2_then_x1


# Visualization----

# Coordinates of new individual and its counterfactual values
coords <- tibble(
  # 1. (x1, x2)
  start = c(new_obs$X1, new_obs$X2),
  # 2. (T_1(x1 | x2), T_2(x2))
  x2_then_x1 = c(new_obs_x2_then_x1$X1, new_obs_x2_then_x1$X2),
  # 3. (T_1(x1), T_2(x2 | x1))
  x1_then_x2 = c(new_obs_x1_then_x2$X1, new_obs_x1_then_x2$X2),
  # 4. (T_1(x1), x2)
  x1_intermediaire = c(new_obs_x1_then_x2$X1, new_obs$X2),
  # 5. (x1, T_2(x2))
  x2_intermediaire = c(new_obs$X1, new_obs_x2_then_x1$X2),
  # 6. T*(x1,x2)
  x_ot = c(new_obs_ot[1], new_obs_ot[2])
)

# Predicted values by the hypothetical model
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
  logistique_reg(coords$x2_intermediaire[1], coords$x2_intermediaire[2], 1),
  # 7. Prediction if (S=1, T*(x1), T*(x2))
  logistique_reg(coords$x_ot[1], coords$x_ot[2], 1)
)

## Figure (7), left----
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

## Figure (7) righe----

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
  paste(round(v[4] * 100, 1), "%", sep = ""), pos = 4, cex = CeX
)

###
# New predicted value for (do(s=1), x1, x2), no transport
###
ry <- .2
plotrix::draw.circle(
  x = coords$start[1] - ry, y = coords$start[2] - ry,
  radius = ry * sqrt(2)
)
text(
  coords$start[1], coords$start[2] + .42,
  paste(round(v[2] * 100, 1), "%", sep = ""), pos = 4, cex = CeX
)

###
# Transported individual with optimal multivariate transport
###
points(coords$x_ot[1],coords$x_ot[2], pch = 15, cex = CeX, col = "#C93312")
segments(
  x0 = coords$start[1], y0 = coords$start[2],
  x1 = coords$x_ot[1], y1 = coords$x_ot[2],
  lwd = .8,
  col = "#C93312", lty = 2
)
text(
  coords$x_ot[1], coords$x_ot[2] + .22,
  paste(round(v[7] * 100, 1), "%", sep = ""), pos = 4, cex = CeX,
  col = "#C93312",
)
