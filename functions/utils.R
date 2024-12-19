#' Sequential Transport for Binary Sensitive Attribute and Numerical Covariates
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
#'
#' @returns A list with four elements:
#' - `Transport_x1`: the function to transport x1 from S=0 to S=1
#' - `Transport_x2`: the function to transport x2 from S=0 to S=1
#' - `Transport_x1_cond_x2`: the function to transport x1|x2 from S=0 to S=1
#' - `Transport_x2_cond_x1`: the function to transport x2|x1 from S=0 to S=1
#' Note: these functions use appromixated values using a grid. The grid
#' coordinates are exported with each function.
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
