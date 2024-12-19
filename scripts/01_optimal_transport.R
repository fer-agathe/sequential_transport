# Optimal Transport
# Demonstration of Optimal Transport methodology
# Application to simulated Gaussian and general marginal distributions.
# The objective is to transport, with OT theory, X=(X1,X2) from group S=0 to S=1
# where S is the binary sensitive attribute

save_figs <- TRUE
op <- par()
if(!dir.exists("../figs/")) dir.create("../figs/")

# Setup----

# Required packages
library(tidyverse)
library(glue)
library(igraph)
library("wesanderson")
library(scales)
library(kableExtra)
library(expm)
library(ks)

# Graphs
colors <- c(
  `0` = "#5BBCD6",
  `1` = "#FF0000",
  A = "#00A08A",
  B = "#F2AD00",
  with = "#046C9A",
  without = "#C93312",
  `2` = "#0B775E"
)

# Seed
set.seed(1234)

source("../functions/utils.R")
source("../functions/graphs.R")

# Univariate Optimal Transport----

## Gaussian Distribution----
# Univariate Gaussian distribution S=0
x1_grid <- seq(-5, 5, length = 251)
m0 <- -1
s0 <- 1.2
d0x1 <- dnorm(x1_grid, m0, s0)
d_0 <- data.frame(x = x1_grid, y = d0x1)

# Univariate Gaussian distribution S=1
m1 <- 1.5
s1 <- .9
d1x1 <- dnorm(x1_grid, m1, s1)
d_1 <- data.frame(x = x1_grid, y = d1x1)

# Plot OT mapping for one example individual from S=0
u <- 0.1586553
# u-quantile of X1 for subset S=0
x1 <- qnorm(u, m0, s0)
# u-quantile of X1 for subset S=1
x1_star <- qnorm(u, m1, s1)

# Identify index of X1 coordinates below that of the individual and its
# counterfactual
idx1 <- which(x1_grid <= x1)
idx1_star <- which(x1_grid <= x1_star)


#### Figure 4 (left) in the paper----

if (save_figs == TRUE) {
  pdf(file = "../figs/fig4-left.pdf", width = 4, height = 4)
}
# Graph parameters
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

# Density of X1 in subset S=0
plot(
  d_0$x, d_0$y, type = "l", col = colors[lab[1]], lwd = 2,
  axes = FALSE, xlab = "", ylab = "", xlim = limA, ylim = limY
)
polygon(
  c(0, d_0$x, 1), c(0, d_0$y, 0),
  col = scales::alpha(colors[lab[1]], 0.1),
  border = NA
)
# cdf of X1 in subset S=0
polygon(
  c(min(d_0$x), d_0$x[idx1], max(d_0$x[idx1])),
  c(0, d_0$y[idx1], 0),
  col = scales::alpha(colors["A"],.2),
  border = NA
)
# Add x-axis
axis(
  1, at = seq(limA[1], limA[2], length = sub),
  label = c(NA, seq(limA[1], limA[2], length = sub)[-1])
)

# Optimal transport from subset S=0 to S=1 (defined with quantile functions)
par(mar = c(4.5, 4.5, 0.5, 0.5))
u_grid <- seq(0, 1, length=261)
q_0 <- qnorm(u_grid, m0, s0)
q_1 <- qnorm(u_grid, m1, s1)
plot(
  q_0, q_1, col = colors["1"], lwd = 2, type = "l",
  xlab = "", ylab = "", xlim = limA, ylim = limB, axes = FALSE
)
abline(a = 0, b = 1, col = colors["0"], lty = 2)
# Add x-axis and y-axis
axis(1)
axis(2)
# Legend
mtext("distribution (group 0)", side = 1, line = 3, col = "black")
mtext("distribution (group 1)", side = 2, line = 3, col = "black")
# Example individual
points(x1, x1_star, pch = 19, col = colors["1"])
segments(x1, x1_star, x1, 10, lwd = .4, col = colors["1"])
segments(x1, x1_star, 10, x1_star, lwd = .4, col = colors["1"])

# Density of X1 in subset S=1
par(mar = c(4.5, 0.5, 0.5, 0.5))
plot(
  d_1$y, d_1$x, type = "l", col = colors[lab[2]], lwd = 2,
  ylim = limB, xlim = limY, xlab = "", ylab = "", axes = FALSE
)
polygon(
  c(0, d_1$y, 0), c(0, d_1$x, 1),
  col = scales::alpha(colors[lab[2]], 0.1), border = NA
)
# cdf of X1 in subset S=1
polygon(
  c(0, d_1$y[idx1_star], 0),
  c(min(d_1$x), d_1$x[idx1_star], max(d_1$x[idx1_star])),
  col = scales::alpha(colors["B"],.2),
  border = NA
)
# Add y-axis
axis(
  2, at = seq(limB[1], limB[2], length = sub),
  label = c(NA, seq(limB[1], limB[2], length = sub)[-c(1, sub)], NA)
)

if (save_figs == TRUE) dev.off()

## General Distribution----
# General distribution for subset S=0
x0 <- rnorm(13, m0, s0)
f0 <- density(x0, from = -5, to = 5, n = length(x1_grid))
d0 <- f0$y
d_0 <- data.frame(x = x1_grid, y = d0)
x0s <- sample(x0, size = 1e3, replace = TRUE) + rnorm(1e3, 0, f0$bw)
F0 <- Vectorize(function(x) mean(x0s <= x))
Q0 <- Vectorize(function(x) as.numeric(quantile(x0s, x)))

# General distribution for subset S=1
x1 <- rnorm(7, m1, 1)
f1 <- density(x1, from = -5, to = 5, n = length(x1_grid))
d1 <- f1$y
d_1 <- data.frame(x = x1_grid, y = d1)
x1s <- sample(x1, size = 1e3, replace = TRUE) + rnorm(1e3, 0, f1$bw)
F1 <- Vectorize(function(x) mean(x1s <= x))
Q1 <- Vectorize(function(x) as.numeric(quantile(x1s, x)))

# Define an example:
u <- 0.1586553
x1 <- Q0(u)
x1_star <- Q1(u)

# Identify index of X1 (and T(X1)) in the grid below the value of the individual
idx1 <- which(x1_grid <= x1)
idx1_star <- which(x1_grid <= x1_star)

### Figure 4 (right) in the paper----

if (save_figs == TRUE) {
  pdf(file = "../figs/fig4-right.pdf", width = 4, height = 4)
}

{
  mat <- matrix(c(1, 2, 0, 3), 2)
  par(mfrow = c(2, 2))
  layout(mat, c(3.5, 1), c(1, 3))
  par(mar = c(0.5, 4.5, 0.5, 0.5))
}

# Density of X1 in subset S=0
plot(d_0$x, d_0$y, type = "l", col = colors[lab[1]], lwd = 2,
     axes = FALSE, xlab = "", ylab = "", xlim = limA, ylim = limY)
polygon(c(0, d_0$x, 1), c(0, d_0$y, 0),
        col = scales::alpha(colors[lab[1]], 0.1),
        border = NA)
# cdf of X1 in subset S=0
polygon(
  c(min(d_0$x), d_0$x[idx1], max(d_0$x[idx1])),
  c(0, d_0$y[idx1], 0),
  col = scales::alpha(colors["A"],.2),
  border = NA
)
# Add x-axis
axis(
  1, at = seq(limA[1], limA[2], length = sub),
  label = c(NA, seq(limA[1], limA[2], length = sub)[-1])
)

# Optimal transport from subset S=0 to S=1 (defined with quantile functions)
par(mar = c(4.5, 4.5, 0.5, 0.5))
u_grid <- seq(0, 1, length=261)
q_0 <- Q0(u_grid)
q_1 <- Q1(u_grid)
plot(
  q_0, q_1, col = colors["1"], lwd = 2, type = "l",
  xlab = "", ylab = "", xlim = limA, ylim = limB, axes = FALSE
)
abline(a = 0, b = 1, col = colors["0"], lty = 2)
# Add x-axis and y-axis
axis(1)
axis(2)
# Legend
mtext("distribution (group 0)", side = 1, line = 3, col = "black")
mtext("distribution (group 1)", side = 2, line = 3, col = "black")
# Example individual
points(x1, x1_star, pch = 19, col = colors["1"])
segments(x1, x1_star, x1, 10, lwd = .4, col = colors["1"])
segments(x1, x1_star, 10, x1_star, lwd = .4, col = colors["1"])

# Density of X1 in subset S=1
par(mar = c(4.5, 0.5, 0.5, 0.5))
plot(
  d_1$y, d_1$x, type = "l", col = colors[lab[2]], lwd = 2,
  ylim = limB, xlim = limY, xlab = "", ylab = "", axes = FALSE
)
polygon(
  c(0, d_1$y, 0), c(0, d_1$x, 1),
  col = scales::alpha(colors[lab[2]], 0.1), border = NA
)
# cdf of X1 in subset S=1
polygon(
  c(0, d_1$y[idx1_star], 0),
  c(min(d_1$x), d_1$x[idx1_star], max(d_1$x[idx1_star])),
  col = scales::alpha(colors["B"],.2),
  border = NA
)
# Add y-axis
axis(
  2, at = seq(limB[1], limB[2], length = sub),
  label = c(NA, seq(limB[1], limB[2], length = sub)[-c(1, sub)], NA)
)

if (save_figs == TRUE) dev.off()

# Conditional Gaussian Transport----

### Figure 10 (left), Appendix A----

if (save_figs == TRUE) {
  pdf(file = "../figs/fig10-left.pdf", width = 4, height = 4)
}


# Transport along the x-axis first, then the y-axis
par(mar = c(2.5, 2.5, 0, 0))
par(mfrow = c(1, 1))

# Assumed cdf for the individual of interest
p1 <- 0.1586553
# y coordinate for that individual
b1 <- 3

vx <- seq(-5, 5, length = 6001)
vy1 <- dnorm(vx,-1, 1.2)*4
vy2 <- dnorm(vx, 1.5, .9)*4

# Marginal density of x0 in source group (at the bottom of graph)
plot(
  vx, vy1,
  col = colors["A"], xlab = "", ylab = "", axes = FALSE, type = "l",
  ylim = c(0, 10)
)
polygon(
  c(min(vx), vx, max(vx)),
  c(0, vy1, 0),
  col = scales::alpha(colors["A"], .2),
  border = NA
)
# Marginal density of x1 in target group (at the bottom of graph)
lines(vx, vy2, col = colors["B"])
polygon(
  c(min(vx), vx, max(vx)),
  c(0, vy2, 0),
  col = scales::alpha(colors["B"], .2),
  border = NA
)

# Corresponding quantile in the marginal distribution
a1 <- qnorm(p1, -1, 1.2) # in source group
# Transported value along x
a2 <- qnorm(p1, 1.5, .9) # in target group
# Identify observation in x below this quantile a1
idx1 <- which(vx <= a1)
# Identify observation in x below this quantile a2
idx2 <- which(vx <= a2)
# Showing P(X_1 < a1 | S = 0) on the marginal density plots
polygon(
  c(min(vx), vx[idx1], max(vx[idx1])),
  c(0, vy1[idx1],0),
  col = scales::alpha(colors["A"],.2),
  border = NA
)
# vertical line to show quantile a1
segments(a1, 0, a1, 100, col = colors["A"])
# Showing P(X_1 < a2 | S = 1) on the marginal density plots
polygon(
  c(min(vx), vx[idx2], max(vx[idx2])),
  c(0, vy2[idx2], 0),
  col=scales::alpha(colors["B"], .2),
  border = NA
)
# vertical line to show quantile a2
segments(a2, 0, a2, 100, col = colors["B"])

# Mean and variance matrix in both groups
M1 <- c(-1,-1+5)
M2 <- c(1.5,1.5+5)
S1 <- matrix(c(1,.5,.5,1)*1.2^2,2,2)
S2 <- matrix(c(1,-.4,-.4,1)*.9^2,2,2)

A <- sqrtm(S1) %*% S2 %*% (sqrtm(S1))
A <- solve(sqrtm(S1)) %*% sqrtm(A) %*% solve((sqrtm(S1)))
T <- function(x) as.vector(M2 + A %*% (x - M1))

# Bivariate Gaussian density
library(mvtnorm)
vx0 <- seq(-5, 5, length = 251)
data.grid <- expand.grid(x = vx0, y = vx0 + 5)
dgauss1 <- matrix(
  mvtnorm::dmvnorm(data.grid, mean = M1, sigma = S1), length(vx0), length(vx0)
)
dgauss2 <- matrix(
  mvtnorm::dmvnorm(data.grid, mean = M2, sigma = S2), length(vx0), length(vx0)
)

# Contour of the bivariate Gaussian density in source group
contour(vx0, vx0 + 5, dgauss1, col = colors["A"], add = TRUE)
# Contour of the bivariate Gaussian density in target group
contour(vx0, vx0 + 5, dgauss2, col = colors["B"], add = TRUE)

axis(1, at = seq(-2, 2) * 2, labels = NA)
axis(
  1, at = a1,
  labels = expression(x[0]),
  col.ticks = NA,
  col.axis = colors["A"], line = .5
)
axis(
  1, at = a2,
  labels = bquote(
    x[1]~"="~mu[1][x] + frac(sigma[1][x],sigma[0][x])~(x[0]-mu[0][x])
  ),
  col.ticks = NA,
  col.axis = colors["B"], line = .5
)
axis(
  1, at = a1,
  labels = NA,
  col.ticks = colors["A"], line = -.5
)
axis(
  1, at = a2,
  labels = NA,
  col.ticks = colors["B"], line = -.5
)

###
# Second axis
###
y <- b1
vx <- vx + 5
mu1 <- M1[2] + S1[1, 2] / S1[1, 1] * (a1 - M1[1])
sig1 <- sqrt(S1[2, 2] - S1[2, 1]^2 / S1[2, 2])
mu2 <- M2[2] + S2[1, 2] / S2[1, 1] * (a2 - M2[1])
sig2 <- sqrt(S2[2, 2]- S2[2, 1]^2 / S2[2, 2])
vz1 <- dnorm(vx, mu1, sig1) * 3
vz2 <- dnorm(vx, mu2, sig2) * 3

# Marginal density on y, source group
lines(vz1 - 5, vx, col = colors["A"])
polygon(
  c(0, vz1, 0) - 5,
  c(min(vx), vx, max(vx)),
  col = scales::alpha(colors["A"], .2),
  border = NA
)
# target group
lines(vz2 - 5, vx, col = colors["B"])
polygon(
  c(0, vz2, 0) - 5,
  c(min(vx), vx, max(vx)),
  col = scales::alpha(colors["B"], .2),
  border = NA
)

# Identify the cdf at b1 in the marginal distribution
p1 <- pnorm(b1, mu1, sig1)
# Transported value in the target marginal distribution
b2 <- qnorm(p1, mu2, sig2)
# Identify observation in y below this quantile b1
idx1 <- which(vx <= b1)
# Identify observation in y below this quantile b2
idx2 <- which(vx <= b2)
# Showing P(X_2 < b1 | S = 0) on the marginal density plots
polygon(
  c(0, vz1[idx1], 0) - 5,
  c(min(vx), vx[idx1], max(vx[idx1])),
  col = scales::alpha(colors["A"], .2),
  border = NA
)
# Showing P(X_2 < b2 | S = 1) on the marginal density plots
polygon(
  c(0, vz2[idx2], 0) - 5,
  c(min(vx), vx[idx2], max(vx[idx2])),
  col = scales::alpha(colors["B"], .2),
  border = NA
)
# horizontal line to show quantile b1
segments(-5, b1, 100, b1, col = colors["A"])
# horizontal line to show quantile b2
segments(-5, b2, 100, b2, col = colors["B"])

# Draw the individual of interest
points(a1, b1, pch = 19)


axis(2, at = c(0,1,3,4,5) * 2, labels = NA)
axis(
  2, at = b1,
  labels = expression(y[0]),
  col.ticks = NA,
  col.axis = colors["A"], line = .5
)
axis(
  2, at = b2,
  labels = bquote(y[1]),
  col.ticks = NA,
  col.axis = colors["B"], line = .5
)
axis(
  2,at = b1,
  labels = NA,
  col.ticks = colors["A"], line = -.5
)
axis(
  2,at = b2,
  labels = NA,
  col.ticks = colors["B"], line = -.5
)

# Drawing transported point
points(a2, b2, pch = 19)

# Decomposition of the sequential transport
# First transport along the x axis
segments(a1, b1, a2, b1, lwd = 2)
# Then transport along the y axis
arrows(a2, b1, a2, b2 - .1, length = .1, lwd = 2)

# Storing in a matrix the coordinates of the point before and after transport
X_then_Y <- matrix(c(a1, b1, a2, b2), 2, 2)
colnames(X_then_Y) = c("start","x_then_y")

# Showing the transported point if we assume another sequential transport:
# first along the y axis, and then along the x axis
M1 <- c(-1, -1+5)
M2 <- c(1.5, 1.5)
S1 <- matrix(c(1, .5, .5, 1) * 1.2^2, 2, 2)
S2 <- matrix(c(1, -.4, -.4, 1) * .9^2, 2, 2)

# Transport
AA <- sqrtm(S1) %*% S2 %*% (sqrtm(S1))
AA <- solve(sqrtm(S1)) %*% sqrtm(AA) %*% solve((sqrtm(S1)))
T <- function(x) as.vector(M2 + AA %*% (x - M1))
opt_ransp <- T(c(a1, b1))
XYopt <- matrix(c(a1, b1, opt_ransp[1], opt_ransp[2] + 5), 2, 2)
colnames(XYopt) = c("start","OT")

# Drawing the point transported with the different order in the sequence
points(opt_ransp[1], opt_ransp[2] + 5, pch = 15, col = "#C93312")

if (save_figs == TRUE) dev.off()


### Figure 10 (right), Appendix A----

if (save_figs == TRUE) {
  pdf(file = "../figs/fig-10-right.pdf", width = 4, height = 4)
}


# Transport along the y-axis first, then the x-axis
par(mar = c(2.5, 2.5, 0, 0))
par(mfrow = c(1, 1))

# Mean and variance matrix in both groups
M1 <- c(-1, -1 + 5)
M2 <- c(1.5, 1.5 + 5)
S1 <- matrix(c(1,.5,.5,1)*1.2^2,2,2)
S2 <- matrix(c(1,-.4,-.4,1)*.9^2,2,2)

# Quantile for the observartion of interest, in the distribution of x
a1 <- -2.2
# y coordinate for the individual of interest
b1 <- 3
b2 <- qnorm(pnorm(b1, 5 - 1, 1.2), 5 + 1.5, .9)

mu1 <- M1[2] + S1[1, 2] / S1[1, 1] * (b1 - M1[1] - 5)
sig1 <- sqrt(S1[2, 2] - S1[2, 1]^2 / S1[2, 2])
mu2 <- M2[2] + S2[1, 2] / S2[1, 1] * (b2 - M2[1] - 5)
sig2 <- sqrt(S2[2, 2] - S2[2, 1]^2 / S2[2, 2])

# Assumed cdf for the individual of interest
p1 <- pnorm(a1 + 5, mu1, sig1)

vx <- seq(-5, 5, length = 6001)
vx <- vx+5
vy1 <- dnorm(vx, mu1, sig1) * 3
vy2 <- dnorm(vx, mu2, sig2) * 3

vz1 <- dnorm(vx - 5, -1, 1.2) * 4
vz2 <- dnorm(vx - 5, 1.5, .9) * 4

# Marginal density of x0 in source group (at the bottom of graph)
plot(
  vx, vy1,
  col = colors["A"], xlab = "", ylab = "", axes = FALSE,
  type = "l", ylim = c(0, 10)
)
lines(vz1 - 5, vx, col = colors["A"])
polygon(
  c(min(vx), vx, max(vx)),
  c(0, vy1, 0),
  col = scales::alpha(colors["A"], .2),
  border = NA
)
# Marginal density of x1 in target group (at the bottom of graph)
lines(vx, vy2, col = colors["B"])
polygon(
  c(min(vx), vx, max(vx)),
  c(0, vy2, 0),
  col = scales::alpha(colors["B"], .2),
  border = NA
)


# Transported value along x
a2 <- qnorm(p1, mu2, sig2)
a1 <- a1 + 5
# Identify observation in x below this quantile a1
idx1 <- which(vx <= a1)
# Identify observation in x below this quantile a2
idx2 <- which(vx <= a2)
# Showing P(X_1 < a1 | S = 0) on the marginal density plots
polygon(
  c(min(vx), vx[idx1], max(vx[idx1])),
  c(0, vy1[idx1], 0),
  col = scales::alpha(colors["A"], .2),
  border = NA
)
# vertical line to show quantile a1
segments(a1, 0, a1, 100, col = colors["A"])
# Showing P(X_1 < a2 | S = 1) on the marginal density plots
polygon(
  c(min(vx), vx[idx2], max(vx[idx2])),
  c(0, vy2[idx2], 0),
  col = scales::alpha(colors["B"], .2),
  border = NA
)
# vertical line to show quantile a2
segments(a2, 0, a2, 100, col = colors["B"])

# Bivariate Gaussian density
library(mvtnorm)
vx0 <- seq(-5, 5, length = 251)
data.grid <- expand.grid(x = vx0, y = vx0 + 5)
dgauss1 <- matrix(
  mvtnorm::dmvnorm(data.grid, mean = M1, sigma = S1), length(vx0), length(vx0)
)
dgauss2 <- matrix(
  mvtnorm::dmvnorm(data.grid, mean = M2, sigma = S2), length(vx0), length(vx0)
)

# Contour of the bivariate Gaussian density in source group
contour(vx0 + 5,vx0 + 5, dgauss1, col = colors["A"], add = TRUE)
# Contour of the bivariate Gaussian density in target group
contour(vx0 + 5, vx0 + 5, dgauss2, col = colors["B"], add = TRUE)


# Showing and annoting x axis
axis(1, at = 5 + seq(-2, 2) * 2, labels = NA)
axis(
  1, at = a1,
  labels = expression(x[0]),
  col.ticks = NA,
  col.axis = colors["A"], line = .5
)
axis(
  1, at = a2,
  labels = bquote(x[1]),
  col.ticks = NA,
  col.axis = colors["B"], line = .5
)
axis(
  1, at = a1,
  labels = NA,
  col.ticks = colors["A"], line = -.5
)
axis(
  1, at = a2,
  labels = NA,
  col.ticks = colors["B"], line = -.5
)

###
# Second axis
###

# Marginal density on y, source group
lines(vz1, vx, col = colors["A"])
polygon(
  c(0, vz1, 0),
  c(min(vx), vx, max(vx)),
  col = scales::alpha(colors["A"], .2),
  border = NA
)
# target group
lines(vz2, vx, col = colors["B"])
polygon(
  c(0, vz2, 0),
  c(min(vx), vx, max(vx)),
  col = scales::alpha(colors["B"], .2),
  border = NA
)

# Identify the cdf at b1 in the marginal distribution
p1 <- pnorm(b1, 5 - 1, 1.2)
# Transported value in the target marginal distribution
b2 <- qnorm(p1, 5 + 1.5, .9)
# Identify observation in y below this quantile b1
idx1 <- which(vx <= b1)
# Identify observation in y below this quantile b2
idx2 <- which(vx <= b2)
# Showing P(X_2 < b1 | S = 0) on the marginal density plots
polygon(
  c(0, vz1[idx1], 0),
  c(min(vx), vx[idx1], max(vx[idx1])),
  col = scales::alpha(colors["A"], .2),
  border = NA
)
# Showing P(X_2 < b2 | S = 1) on the marginal density plots
polygon(
  c(0, vz2[idx2], 0),
  c(min(vx), vx[idx2], max(vx[idx2])),
  col = scales::alpha(colors["B"], .2),
  border = NA
)

# horizontal line to show quantile b1
segments(0, b1, 100, b1, col = colors["A"])
# horizontal line to show quantile b2
segments(0, b2, 100, b2, col = colors["B"])

# Draw the individual of interest
points(a1, b1, pch = 19)

# Drawing y axis
axis(2, at = c(0, 1, 3, 4, 5) * 2, labels = NA)
axis(
  2, at = b1,
  labels = expression(y[0]),
  col.ticks = NA,
  col.axis = colors["A"], line = 0
)
axis(
  2, at = b2,
  labels = bquote(
    y[1]~"="~mu[1][y]+frac(sigma[1][y],sigma[0][y])~(y[0]-mu[0][y])
  ),
  col.ticks = NA,
  col.axis = colors["B"], line = .5
)
axis(
  2, at = b1,
  labels=NA,
  col.ticks = colors["A"],line = -.5
)
axis(
  2,at = b2,
  labels = NA,
  col.ticks = colors["B"], line = -.5
)

# Drawing transported point
points(a2, b2, pch = 19)

# Decomposition of the sequential transport
# First transport along the x axis
segments(a1, b1, a1, b2, lwd = 2)
# Then transport along the y axis
arrows(a1, b2, a2 - .1, b2, length = .1, lwd = 2)

# Storing in a matrix the coordinates of the point before and after transport
Y_then_X = matrix(c(a1, b1, a2, b2), 2, 2)
colnames(Y_then_X) = c("start","y_then_x")

# Showing the transported point if we assume another sequential transport:
# first along the x axis, and then along the y axis
M1 <- c(-1, -1)
M2 <- c(1.5, 1.5)
S1 <- matrix(c(1, .5, .5, 1) * 1.2^2, 2, 2)
S2 <- matrix(c(1, -.4, -.4, 1) * .9^2, 2, 2)

# Transport
AA <- sqrtm(S1) %*% S2 %*% (sqrtm(S1))
AA <- solve(sqrtm(S1)) %*% sqrtm(AA) %*% solve((sqrtm(S1)))
T <- function(x) as.vector(M2 + AA %*% (x - M1))
opt_ransp <- T(c(a1 - 5, b1 - 5))

# Drawing the point transported with the different order in the sequence
points(opt_ransp[1] + 5, opt_ransp[2] + 5, pch = 15, col = "#C93312")

if (save_figs == TRUE) dev.off()


# General Conditional Transport----

### Figure 11 (left), Appendix A----

if (save_figs == TRUE) {
  pdf(file = "../figs/fig-11-left.pdf", width = 4, height = 4)
}

angle <- function(theta,
                  A = c(-2.2,-2)) {

  # Rotation
  R <- matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)), 2, 2)
  # Mean and Variance
  M1 <- c(-1, -1)
  M2 <- c(1.5, 1.5)
  S1 <- matrix(c(1, .5, .5, 1) * 1.2^2, 2, 2)
  S2 <- matrix(c(1, -.4, -.4, 1) * .9^2, 2, 2)

  # After applying the rotation
  M1 <- as.vector(R %*% M1)
  M2 <- as.vector(R %*% M2)
  S1 <- t(R) %*% S1 %*% R
  S2 <- t(R) %*% S2 %*% R
  A <- as.vector(R %*% A)

  # Coordinates
  a1 <- A[1]
  b1 <- A[2]
  a2 <- qnorm(pnorm(a1, M1[1], sqrt(S1[1, 1])), M2[1], sqrt(S2[1, 1]))
  mu1 <- M1[2] + S1[1, 2] / S1[1, 1] * (a1 - M1[1])
  sig1 <- sqrt(S1[2, 2] - S1[2, 1]^2 / S1[2, 2])
  mu2 <- M2[2] + S2[1, 2] / S2[1, 1] * (a2 - M2[1])
  sig2 <- sqrt(S2[2, 2] - S2[2, 1]^2 / S2[2, 2])
  p1 <- pnorm(b1, mu1, sig1)
  b2 <- qnorm(p1, mu2, sig2)
  B <- c(a2, b2)

  c(
    as.vector(t(R) %*% A),
    as.vector(t(R) %*% B)
  )
}

# Mean and variance
M1 <- c(-1, -1)
M2 <- c(1.5, 1.5)
S1 <- matrix(c(1, .5, .5, 1) * 1.2^2, 2, 2)
S2 <- matrix(c(1, -.4, -.4, 1) * .9^2, 2, 2)

A <- c(-2.2, -2)

par(mfrow = c(1, 1), mar = c(.5, .5, 0, 0))

# Bivariate Gaussian density
library(mvtnorm)
vx0 <- seq(-5, 5, length = 251)
data.grid <- expand.grid(x = vx0, y = vx0)
dgauss1 <- matrix(
  mvtnorm::dmvnorm(data.grid, mean = M1, sigma = S1), length(vx0), length(vx0)
)
dgauss2 <- matrix(
  mvtnorm::dmvnorm(data.grid, mean = M2, sigma = S2), length(vx0), length(vx0)
)

# Contour of the bivariate Gaussian density in source group
contour(
  vx0, vx0, dgauss1, col = colors["A"],
  xlim = c(-5, 5), ylim = c(-5, 5), axes = FALSE
)
# Contour of the bivariate Gaussian density in target group
contour(vx0, vx0, dgauss2, col = colors["B"], add = TRUE)

# Horizontal and vertical line showing the coordinates of the point of interest
segments(A[1], -5, A[1], 100, col = colors["A"])
segments(-5, A[2], 100, A[2], col = colors["A"])
# Drawing this point
points(A[1], A[2], pch = 19, col = "darkblue")

# Add axes
axis(1, at = c(0, 1, 2, 3, 4, 5) * 2 - 5, labels = NA)
axis(2, at = c(0, 1, 2, 3, 4, 5) * 2 - 5, labels = NA)

# Apply rotations
MANGLE <- Vectorize(angle)(seq(0, 2 * pi, length = 100))
# Draw the transported points for each rotation
lines(MANGLE[3, ], MANGLE[4, ])

# Display specific point when transporting first along the x axis and then the
# orthogonal axis, i.e., y
m <- angle(0, c(-2.2, -2))
points(m[3],m[4],pch=19)

# Same but transporting first along the y axis and then on the orthogonal axis,
# i.e., x
m <- angle(pi/2, c(-2.2, -2))
points(m[3], m[4], pch = 19)

# Global optimal transport
T <- function(x) as.vector(M2 + AA %*% (x - M1))
opt_ransp <- T(c(A[1], A[2]))
points(opt_ransp[1], opt_ransp[2], pch = 15, col = "#C93312")

if (save_figs == TRUE) dev.off()


### Figure 11 (right), Appendix A----

if (save_figs == TRUE) {
  pdf(file = "../figs/fig-11-right.pdf", width = 4, height = 4)
}

# Consider the following starting point
A = c(-2.2, .5)

par(mfrow = c(1, 1), mar = c(.5, .5, 0, 0))

# Bivariate Gaussian density
vx0 <- seq(-5, 5, length = 251)
data.grid <- expand.grid(x = vx0, y = vx0)
dgauss1 <- matrix(
  mvtnorm::dmvnorm(data.grid, mean = M1, sigma = S1), length(vx0), length(vx0)
)
dgauss2 <- matrix(
  mvtnorm::dmvnorm(data.grid, mean = M2, sigma = S2), length(vx0), length(vx0)
)
# Contour of the bivariate Gaussian density in source group
contour(
  vx0, vx0, dgauss1, col = colors["A"],
  xlim = c(-5, 5), ylim = c(-5, 5), axes = FALSE
)
# Contour of the bivariate Gaussian density in target group
contour(vx0, vx0, dgauss2, col = colors["B"], add = TRUE)

# Vertical and horizontal lines showing the coordinates of the point
segments(A[1], -5, A[1], 100, col = colors["A"])
segments(-5, A[2], 100, A[2], col = colors["A"])
# Drawing that starting point
points(A[1], A[2], pch = 19, col = "darkblue")

# Showing axes
axis(1, at = c(0,1,2,3,4,5) * 2 - 5, labels = NA)
axis(2, at = c(0,1,2,3,4,5) * 2 - 5, labels = NA)

# Apply rotations
MANGLE <-
  Vectorize(function(x) angle(x, c(-2.2, .5)))(seq(0, 2 * pi, length = 100))
# Draw the transported points for each rotation
lines(MANGLE[3, ], MANGLE[4, ])

# Display specific point when transporting first along the x axis and then the
# orthogonal axis, i.e., y
m <- angle(0, c(-2.2, .5))
points(m[3], m[4], pch = 19)

# Same but transporting first along the y axis and then on the orthogonal axis,
# i.e., x
m <- angle(pi/2, c(-2.2, .5))
points(m[3], m[4], pch = 19)

# Global optimal transport
AA <- sqrtm(S1) %*% S2 %*% (sqrtm(S1))
AA <- solve(sqrtm(S1)) %*% sqrtm(AA) %*% solve((sqrtm(S1)))
T <- function(x) as.vector(M2 + AA %*% (x - M1))
opt_ransp <- T(c(A[1], A[2]))
points(opt_ransp[1], opt_ransp[2], pch = 15, col = "#C93312")

if (save_figs == TRUE) dev.off()
