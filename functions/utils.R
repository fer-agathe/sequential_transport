#' Define the function to split the dataset into train/test sets
#'
#' @param data dataset
#' @param seed seed number to use for replication
#' @param train_ratio ratio of data to use in the train set (default to 0.7)
split_dataset <- function(data, seed, train_ratio = 0.7) {
  set.seed(seed)
  train_size <- floor(train_ratio * nrow(data))
  train_indices <- sample(seq_len(nrow(data)), replace = TRUE, size = train_size)
  # Split the data into train and test sets
  train_set <- data[train_indices, ]
  test_set <- data[-train_indices, ]
  # Return the train and test sets as a list
  list(
    data_train = train_set,
    data_test = test_set
  )
}

#' Train and predict a logistic regression classifier: aware model
#'
#' @param train_data train set. The sensitive variable S must be in the data
#' @param test_data test set. The sensitive variable S must be in the data
#' @param type type of model to train: if `"aware"`, the sensitive variable S
#'  is included in the formula of the glm, if `"unaware"`, the sensitive
#'  variable is not included in the formula of the model.
log_reg_train <- function(train_data,
                          test_data,
                          type = c("aware", "unaware")) {
  if (type == "unaware") {
    train_data_ <- train_data %>% select(-S)
    test_data_ <- test_data %>% select(-S)
  } else {
    train_data_ <- train_data
    test_data_ <- test_data
  }
  # Train the logistic regression model
  model <- glm(Y ~ ., data = train_data_, family = binomial)
  # Predictions on train and test sets
  pred_train <- predict(model, newdata = train_data_, type = "response")
  pred_test <- predict(model, newdata = test_data_, type = "response")
  list(
    model = model,
    pred_train = pred_train,
    pred_test = pred_test
  )
}

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

#' Sequential Transport Using a Pre-Defined Causal Graph
#'
#' The sensitive attribute, S, is assumed to be a binary variable with value
#' $S_0$ in the source distribution and $S_1$ in the target distribution.
#'
#' @param data Data frame with the observations.
#' @param adj Adjacency matrix for the causal graph.
#' @param s Name of the sensitive attribute column in the data.
#' @param S_0 Label of the sensitive attribute in the source distribution.
#' @param y Name of the outcome variable in the data.
#' @param num_neighbors Number of neighbors to use in the weighted quantile
#'        estimation. Default to 5.
#' @param silent If `TRUE`, the messages showing progress in the estimation are
#'        not shown. Default to `silent=FALSE`.
#'
#' @returns A list with the following elements:
#' * `transported`: A named list with the transported values. The names are those of the variables.
#' * `weights`: A list with the weights of each observation in the two groups.
#' * `ecdf`: A list with empirical distribution functions for numerical variables.
#' * `ecdf_values`: A list with the values of the ecdf evaluated for each observation in the source distribution.
#' * `fit_for_categ`: A list with the estimated multinomial models to predict categories using parents characteristics
#' * `params`: A list with some parameters used to transport observations:
#'     * `adj`: Adjacency matrix.
#'     * `top_order`: Topological ordering.
#'     * `s`: Name of the sensitive attribute.
#'     * `S_0`: Label of the sensitive attribute in the source distribution.
#'     * `y`: Name of the outcome variable in the data.
#'     * `num_neighbors`: Number of neighbors used when computing quantiles.
#' @md
#' @export
#'
#' @importFrom stats predict ecdf quantile
#' @importFrom dplyr across filter mutate pull select
#' @importFrom tidyselect where
#' @importFrom rlang sym !! := is_character
#' @importFrom cluster daisy
#' @importFrom Hmisc wtd.quantile
#' @importFrom nnet multinom
seq_trans <- function(data,
                      adj,
                      s,
                      S_0,
                      y,
                      num_neighbors = 5,
                      silent = FALSE) {
  # Make sure character variables are encoded as factors
  data <-
    data |>
    mutate(across(where(is_character), ~as.factor(.x)))

  # Topological ordering
  top_order <- topological_ordering(adj)
  variables <- top_order[!top_order %in% c(s, y)]
  # Observations in group S_0
  data_0 <- data |> filter(!!sym(s) == !!S_0)
  data_1 <- data |> filter(!!sym(s) != !!S_0)

  # Lists where results will be stored
  list_transported <- list()  # Transported values
  list_weights <- list()      # Weights
  list_ecdf <- list()         # Empirical dist. function
  list_ecdf_values <- list()  # Evaluated values of the ecdf
  fit_for_categ <- list()     # Fitted multinomial models for categ. variables

  for (x_name in variables) {
    if (silent == FALSE) cat("Transporting ", x_name, "\n")
    # Names of the parent variables
    parents <- colnames(adj)[adj[, x_name] == 1]
    # values of current x in each group
    x_S0 <- data_0 |> pull(!!x_name)
    x_S1 <- data_1 |> pull(!!x_name)
    # Check whether X is numeric
    is_x_num <- is.numeric(x_S0)
    # Characteristics of the parent variables (if any)
    parents_characteristics <- data_0 |> select(!!parents) |> select(-!!s)

    if (length(parents_characteristics) > 0) {
      data_0_parents <- data_0 |> select(!!parents) |> select(-!!s)
      data_1_parents <- data_1 |> select(!!parents) |> select(-!!s)
      # Weights in S_0
      weights_S0 <- as.matrix(cluster::daisy(data_0_parents, metric = "gower"))
      tot_weights_S0 <- apply(weights_S0, MARGIN = 1, sum)
      # Weights in S_1
      # First, we need to get the transported values for the parents, if necessary
      data_0_parents_t <- data_0_parents #init
      for (parent in parents) {
        # does the parent depend on the sensitive variable
        if (parent %in% names(list_transported)) {
          data_0_parents_t <-
            data_0_parents_t |>
            mutate(!!sym(parent) := list_transported[[parent]])
        }
      }
      # Unfortunately, we will compute a lot of distances not needed
      combined <- rbind(data_0_parents_t, data_1_parents)
      gower_dist <- cluster::daisy(combined, metric = "gower")
      gower_matrix <- as.matrix(gower_dist)
      n_0 <- nrow(data_0_parents_t)
      n_1 <- nrow(data_1_parents)
      weights_S1 <- gower_matrix[1:n_0, (n_0 + 1):(n_0 + n_1), drop = FALSE]
      weights_S1 <- weights_S1 + 1e-8
      weights_S1 <- 1 / (weights_S1)^2
      tot_weights_S1 <- apply(weights_S1, MARGIN = 1, sum)

      if (is_x_num == TRUE) {
        # Numerical variable to transport

        # Empirical distribution function
        f <- rep(NA, length(x_S0))
        for (i in 1:length(x_S0)) {
          f[i] <- weights_S0[i, ] %*% (x_S0 <= x_S0[i]) / tot_weights_S0[i]
        }
        list_ecdf_values[[x_name]] <- f
        f[f==1] <- 1-(1e-8)

        # Transported values
        transported <- rep(NA, length(x_S0))
        for (i in 1:length(x_S0)) {
          wts <- weights_S1[i, ]
          wts[-order(wts, decreasing = TRUE)[1:num_neighbors]] <- 0
          transported[i] <- Hmisc::wtd.quantile(
            x = x_S1, weights = weights_S1[i, ], probs = f[i]
          ) |> suppressWarnings()
        }
      } else {
        # X is non numeric and has parents

        # Fit a model to predict the categorical variables given the
        # characteristics of the parents in group to transport into
        fit_indix_x <- nnet::multinom(
          x_S0 ~ .,
          data = data_1_parents |> mutate(x_S0 = x_S1)
        )
        # Predictions with that model for transported parents
        pred_probs <- predict(fit_indix_x, type = "probs", newdata = data_0_parents_t)
        # For each observation, random draw of the class, using the pred probs
        # as weights
        drawn_class <- apply(
          pred_probs, 1,
          function(x) sample(1:ncol(pred_probs), prob = x, size = 1)
        )
        transported <- colnames(pred_probs)[drawn_class]
        if (is.factor(x_S1)) {
          transported <- factor(transported, levels = levels(x_S1))
        }
        fit_for_categ[[x_name]] <- fit_indix_x
      }
      list_transported[[x_name]] <- transported

      # Store weights for possible later use
      list_weights[[x_name]] <- list(
        w_S0 = list(weights = weights_S0, tot_weights = tot_weights_S0),
        w_S1 = list(weights = weights_S1, tot_weights = tot_weights_S1)
      )
    } else {
      # No parents
      if (is_x_num == TRUE) {
        # X is numerical and has no parents
        F_X_S0 <- ecdf(x_S0)
        list_ecdf[[x_name]] <- F_X_S0
        f <- F_X_S0(x_S0)
        list_ecdf_values[[x_name]] <- f
        transported <- as.numeric(quantile(x_S1, probs = f))
      } else {
        # X is not numerical and has no parents
        transported <- sample(x_S1, size = length(x_S0), replace = TRUE)
        if (is.factor(x_S1)) {
          transported <- factor(transported, levels = levels(x_S1))
        } else {
          transported <- as.factor(transported)
        }
      }
      list_transported[[x_name]] <- transported
    }
  }

  return(
    list(
      transported = list_transported,
      weights = list_weights,
      ecdf = list_ecdf,
      ecdf_values = list_ecdf_values,
      fit_for_categ = fit_for_categ,
      params = list(
        adj = adj,
        top_order = top_order,
        s = s,
        S_0 = S_0,
        y = y,
        num_neighbors = num_neighbors
      )
    )
  )
}

#' Sequential Transport of New Observations Using a Pre-Defined Causal Graph
#'
#' This function transports new observations from the source group to the target
#' group, using the mappings previously learned when evaluating the
#' `seq_transport_binary()` function.
#'
#' @param x Object containing previously transported values, obtained with
#'        `seq_transport_binary()`.
#' @param newdata New data with observations from initial group to transport.
#' @param data Data used during the construction of counterfactuals with
#'        `seq_transport_binary()`.
#' @export
#' @returns A list with the transported values. Each element contains the
#'          transported values of a variable.
#'
#' @importFrom stats predict quantile
#' @importFrom dplyr across filter mutate pull select
#' @importFrom purrr map_dbl
#' @importFrom rlang sym !! :=
#' @importFrom cluster daisy
#' @importFrom Hmisc wtd.quantile
#' @importFrom nnet multinom
seq_trans_new <- function(x,
                          newdata,
                          data) {

  top_order <- x$params$top_order
  s <- x$params$s
  S_0 <- x$params$S_0
  y <- x$params$y
  adj <- x$params$adj
  num_neighbors <- x$params$num_neighbors
  variables <- top_order[!top_order %in% c(s, y)]
  newdata_0 <- newdata |> filter(!!sym(s) == !!S_0)
  data_0 <- data |> filter(!!sym(s) == !!S_0)
  data_1 <- data |> filter(!!sym(s) != !!S_0)
  list_ecdf <- x$ecdf
  list_ecdf_values <- x$ecdf_values
  fit_for_categ <- x$fit_for_categ

  # Lists where results will be stored
  list_transported <- list()  # Transported values

  for (x_name in variables) {
    # Names of the parent variables
    parents <- colnames(adj)[adj[, x_name] == 1]
    # values of current x in each group
    x_S0 <- newdata |> pull(!!x_name)
    x_S0_initial <- data_0 |> pull(!!x_name)
    x_S1 <- data_1 |> pull(!!x_name)
    # Check whether X is numeric
    is_x_num <- is.numeric(x_S0)
    # Characteristics of the parent variables (if any)
    parents_characteristics <- newdata_0 |> select(!!parents) |> select(-!!s)

    if (length(parents_characteristics) > 0) {
      newdata_0_parents <- newdata_0 |> select(!!parents) |> select(-!!s)
      data_0_parents <- data_0 |> select(!!parents) |> select(-!!s)
      data_1_parents <- data_1 |> select(!!parents) |> select(-!!s)
      # Weights in S_0
      weights_S0 <- x$weights[[x_name]]$w_S0$weights
      tot_weights_S0 <- x$weights[[x_name]]$w_S0$tot_weights

      # Weights in S_1
      # First, we need to get the transported values for the parents, if necessary
      newdata_0_parents_t <- newdata_0_parents #init
      data_0_parents_t <- data_0_parents #init
      for (parent in parents) {
        # does the parent depend on the sensitive variable
        if (parent %in% names(list_transported)) {
          newdata_0_parents_t <-
            newdata_0_parents_t |>
            mutate(!!sym(parent) := list_transported[[parent]])
          data_0_parents_t <-
            data_0_parents_t |>
            mutate(!!sym(parent) := x$transported[[parent]])
        }
      }

      combined <- rbind(newdata_0_parents_t, data_1_parents)
      gower_dist <- cluster::daisy(combined, metric = "gower")
      gower_matrix <- as.matrix(gower_dist)
      n_0 <- nrow(newdata_0_parents_t)
      n_1 <- nrow(data_1_parents)
      weights_S1 <- gower_matrix[1:n_0, (n_0 + 1):(n_0 + n_1), drop = FALSE]
      weights_S1 <- weights_S1 + 1e-8
      weights_S1 <- 1 / (weights_S1)^2
      tot_weights_S1 <- apply(weights_S1, MARGIN = 1, sum)

      if (is_x_num == TRUE) {
        # Find the closest point in S0 with respect to the current variable
        closest_indices <- map_dbl(x_S0, ~which.min(abs(x_S0_initial - .x)))

        # Numerical variable to transport

        # Empirical distribution function
        f <- rep(NA, length(x_S0))
        for (i in 1:length(x_S0)) {
          ind_closest <- closest_indices[i]
          f[i] <- weights_S0[ind_closest, ] %*% (x_S0_initial <= x_S0_initial[ind_closest]) / tot_weights_S0[ind_closest]
        }
        f[f==1] <- 1-(1e-8)
        # Transported values
        transported <- rep(NA, length(x_S0))
        for (i in 1:length(x_S0)) {
          wts <- weights_S1[i, ]
          # Give weights to the 5 closests points only
          wts[-order(wts, decreasing = TRUE)[1:num_neighbors]] <- 0
          transported[i] <- Hmisc::wtd.quantile(
            x = x_S1, weights = weights_S1[i, ], probs = f[i]
          ) |> suppressWarnings()
        }
      } else {
        # X is non numeric and has parents

        # Retrieve fitted model (categorical variable given the characteristics
        # of the parents in group to transport into)
        fit_indix_x <- fit_for_categ[[x_name]]
        # Predictions with that model for transported parents
        pred_probs <- predict(fit_indix_x, type = "probs", newdata = newdata_0_parents_t)
        # For each observation, random draw of the class, using the pred probs
        # as weights
        drawn_class <- apply(
          pred_probs, 1,
          function(x) sample(1:ncol(pred_probs), prob = x, size = 1)
        )
        transported <- colnames(pred_probs)[drawn_class]
        if (is.factor(x_S1)) {
          transported <- factor(transported, levels = levels(x_S1))
        }
      }
      list_transported[[x_name]] <- transported
    } else {
      # No parents
      if (is_x_num == TRUE) {
        # X is numerical and has no parents
        F_X_S0 <- list_ecdf[[x_name]]
        f <- F_X_S0(x_S0)
        transported <- as.numeric(quantile(x_S1, probs = f))
      } else {
        # X is not numerical and has no parents
        transported <- sample(x_S1, size = length(x_S0), replace = TRUE)
        if (is.factor(x_S1)) {
          transported <- factor(transported, levels = levels(x_S1))
        } else {
          transported <- as.factor(transported)
        }
      }
      list_transported[[x_name]] <- transported
    }
  }
  list_transported
}

#' Topological Ordering
#'
#' @source This function comes from the fairadapt package. Drago Plecko,
#'         Nicolai Meinshausen (2020). Fair data adaptation with quantile
#'         preservation Journal of Machine Learning Research, 21.242, 1-44.
#'         URL https://www.jmlr.org/papers/v21/19-966.html.
#' @param adj_mat Adjacency matrix with names of the variables for both rows and
#'        columns.
#' @return A character vector (names of the variables) providing a topological
#'         ordering.
#' @export
topological_ordering <- function(adj_mat) {
  nrw <- nrow(adj_mat)
  num_walks <- adj_mat
  for (i in seq_len(nrw + 1L)) {
    num_walks <- adj_mat + num_walks %*% adj_mat
  }
  comparison_matrix <- num_walks > 0
  top_order <- colnames(adj_mat)
  for (i in seq_len(nrw - 1L)) {
    for (j in seq.int(i + 1L, nrw)) {
      if (comparison_matrix[top_order[j], top_order[i]]) {
        top_order <- swap(top_order, i, j)
      }
    }
  }
  top_order
}

#' Swap Two Elements in a Matrix.
#'
#' @source This function comes from the fairadapt package. Drago Plecko,
#'         Nicolai Meinshausen (2020). Fair data adaptation with quantile
#'         preservation Journal of Machine Learning Research, 21.242, 1-44.
#'         URL https://www.jmlr.org/papers/v21/19-966.html.
#' @param x A matrix.
#' @param i Index of the first element to swap.
#' @param j Index of the second element to swap.
#' @return The matrix x where the i-th and j-th elements have been swapped.
#' @noRd
swap <- function(x, i, j) {
  keep <- x[i]
  x[i] <- x[j]
  x[j] <- keep
  x
}
