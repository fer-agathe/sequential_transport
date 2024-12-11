#' Fast transport function using a pre-defined DAG
#'
#' @param data data frame with the observations
#' @param adj adjacency matrix
#' @param s name of the sensitive attribute in the data
#' @param S_0 value for the sensitive attribute for the source distribution
#' @param y name of the outcome variable
#' @param silent if TRUE, the messages showing progress in the estimation are
#'  not shown. Default to `FALSE`
#' @importFrom fairadapt topologicalOrdering
#' @importFrom dplyr filter pull select mutate
#' @importFrom stats dist quantile ecdf
#' @import rlang sym
#' @importFrom Hmisc wtd.quantile
#' @importFrom proxy dist
sequential_transport <- function(data,
                                 adj,
                                 s,
                                 S_0,
                                 y,
                                 silent = FALSE) {
  # Topological ordering
  top_order <- fairadapt:::topologicalOrdering(adj)
  variables <- top_order[!top_order %in% c(s, y)]
  # Observations in group S_0
  individuals <- data |> dplyr::filter(!!sym(s) == !!S_0)

  # Lists where results will be stored
  list_transported <- list()  # Transported values
  list_weights <- list()      # Weights
  list_ecdf <- list()         # Empirical dist. function
  list_ecdf_values <- list()  # Evaluated values of the ecdf

  for (x_name in variables) {
    if (silent == FALSE) cat("Transporting ", x_name, "\n")
    parents <- colnames(adj)[adj[, x_name] == 1]
    # Values of the current variable to transport
    indiv_x <- individuals |> dplyr::pull(!!x_name)

    # Characteristics of the parent variables (if any)
    indiv_characteristics <- individuals |>
      dplyr::select(!!parents) |> dplyr::select(-!!s)

    data_0 <- data |> dplyr::filter(!!sym(s) == !!S_0)
    data_1 <- data |> dplyr::filter(!!sym(s) != !!S_0)
    x_S0 <- data_0 |> dplyr::pull(!!x_name)
    x_S1 <- data_1 |> dplyr::pull(!!x_name)

    if (length(indiv_characteristics) > 0) {
      data_0_parents <- data_0 |> dplyr::select(!!parents) |> dplyr::select(-!!s)
      data_1_parents <- data_1 |> dplyr::select(!!parents) |> dplyr::select(-!!s)
      # Weights in S_0
      weights_S0 <- stats::dist(
        data_0_parents, diag = TRUE, upper = TRUE, method = "manhattan"
      )
      weights_S0 <- as.matrix(weights_S0)
      tot_weights_S0 <- apply(weights_S0, MARGIN = 1, sum)
      # Weights in S_1
      # First, we need to get the transported values for the parents, if necessary
      data_0_parents_t <- data_0_parents #init
      for (parent in parents) {
        # does the parent depend on the sensitive variable
        if (parent %in% names(list_transported)) {
          data_0_parents_t <-
            data_0_parents_t |>
            dplyr::mutate(!!sym(parent) := list_transported[[parent]])
        }
      }
      # Then, we compute the weights
      weights_S1 <- proxy::dist(
        data_0_parents_t, #rows
        data_1_parents, #cols
        method = "Euclidean"
      )
      tot_weights_S1 <- apply(weights_S1, MARGIN = 1, sum)

      # Empirical distribution function
      f <- rep(NA, length(indiv_x))
      for (i in 1:length(indiv_x)) {
        f[i] <- weights_S0[i, ] %*% (x_S0 <= indiv_x[i]) / tot_weights_S0[i]
      }
      list_ecdf_values[[x_name]] <- f

      # Transported values
      transported <- rep(NA, length(indiv_x))
      for (i in 1:length(indiv_x)) {
        # transported[i] <- modi::weighted.quantile(x = x_S1, w = weights_S1[i, ], prob = f[i], plot = FALSE)
        transported[i] <- as.numeric(
          Hmisc::wtd.quantile(x = x_S1, weights = weights_S1[i, ], probs = f[i])
        )
      }
      list_transported[[x_name]] <- transported

      # Store weights for possible later use
      list_weights[[x_name]] <- list(
        w_S0 = list(weights = weights_S0, tot_weights = tot_weights_S0),
        w_S1 = list(weights = weights_S1, tot_weights = tot_weights_S1)
      )

    } else {
      # No parents
      F_X_S0 <- ecdf(x_S0)
      list_ecdf[[x_name]] <- F_X_S0
      f <- F_X_S0(indiv_x)
      list_ecdf_values[[x_name]] <- f
      transported <- as.numeric(stats::quantile(x_S1, probs = f))
      list_transported[[x_name]] <- transported
    }
  }

  return(
    list(
      transported = list_transported,
      weights = list_weights,
      ecdf = list_ecdf,
      adj = adj,
      top_order = top_order,
      s = s,
      S_0 = S_0,
      y = y
    )
  )
}

#' Sequential transport for new observations
#'
#' @param object obtained with sequential_transport()
#' @param newdata tibble/data.frame with new observations from S_0 to transport
#' @param data data used to obtain the sequential transport
#' @importFrom dplyr filter pull select mutate bind_rows
#' @importFrom stats dist quantile ecdf
#' @import rlang sym
#' @importFrom Hmisc wtd.quantile
#' @importFrom proxy dist
seq_transport_new <- function(object,
                              newdata,
                              data) {
  list_transported <- object$transported
  list_weights <- object$weights
  list_ecdf <- object$ecdf
  adj <- object$adj
  top_order <- object$top_order
  s <- object$s
  S_0 <- object$S_0
  y <- object$y

  variables <- top_order[!top_order %in% c(s, y)]
  # Observations in group S_0
  individuals <- newdata

  # Lists where results were be stored
  list_transported_new <- list()  # Transported values

  for (x_name in variables) {
    parents <- colnames(adj)[adj[, x_name] == 1]
    # Values of the current variable to transport
    indiv_x <- individuals |> dplyr::pull(!!x_name)

    # Characteristics of the parent variables (if any)
    indiv_characteristics <- individuals |>
      dplyr::select(!!parents) |> dplyr::select(-!!s)

    data_0 <- data |> dplyr::filter(!!sym(s) == !!S_0)
    data_1 <- data |> dplyr::filter(!!sym(s) != !!S_0)
    x_S0 <- data_0 |> dplyr::pull(!!x_name)
    x_S1 <- data_1 |> dplyr::pull(!!x_name)
    n_0 <- nrow(data_0)

    if (length(indiv_characteristics) > 0) {
      data_0_parents <- data_0 |> dplyr::select(!!parents) |> dplyr::select(-!!s)
      data_1_parents <- data_1 |> dplyr::select(!!parents) |> dplyr::select(-!!s)
      # Weights in S_0
      weights_S0 <- stats::dist(
        dplyr::bind_rows(data_0_parents, indiv_characteristics),
        diag = TRUE, upper = TRUE, method = "manhattan"
      )
      weights_S0 <- as.matrix(weights_S0)
      tot_weights_S0 <- apply(weights_S0, MARGIN = 1, sum)
      # Keep only the weights for the new individuals
      weights_S0 <- weights_S0[-(1:n_0), , drop = FALSE]
      tot_weights_S0 <- tot_weights_S0[-c(1:n_0)]
      # Weights in S_1
      # First, we need to get the transported values for the parents, if necessary
      data_0_parents_t <- data_0_parents |> bind_rows(indiv_characteristics)
      for (parent in parents) {
        # does the parent depend on the sensitive variable
        if (parent %in% names(list_transported)) {
          t_val <- list_transported[[parent]]
          t_val_new <- list_transported_new[[parent]]
          data_0_parents_t <-
            data_0_parents_t |>
            dplyr::mutate(!!sym(parent) := c(t_val, t_val_new))
        }
      }

      # we get the weights for S_1 from the estimation previously made
      weights_S1 <- list_weights[[x_name]]$w_S1$weights
      tot_weights_S1 <- list_weights[[x_name]]$w_S1$tot_weights

      # Empirical distribution function
      f <- rep(NA, length(indiv_x))
      for (i in 1:length(indiv_x)) {
        f[i] <- weights_S0[i, ] %*% (c(x_S0, indiv_x) <= indiv_x[i]) / tot_weights_S0[i]
      }

      # Transported values
      transported_new <- rep(NA, length(indiv_x))
      for (i in 1:length(indiv_x)) {
        # transported_new[i] <- modi::weighted.quantile(x = x_S1, w = weights_S1[i, ], prob = f[i], plot = FALSE)
        transported_new[i] <- as.numeric(
          Hmisc::wtd.quantile(x = x_S1, weights = weights_S1[i, ], probs = f[i])
        )
      }
      list_transported_new[[x_name]] <- transported_new
    } else {
      # No parents
      F_X_S0 <- list_ecdf[[x_name]]
      f <- F_X_S0(indiv_x)
      transported_new <- as.numeric(stats::quantile(x_S1, probs = f))
      list_transported_new[[x_name]] <- transported_new
    }
  }

  list_transported_new
}
