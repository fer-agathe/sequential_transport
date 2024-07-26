# Import packages
library(dplyr)
library(quantreg)

#' Computes the counterfactuals given an input value of the sensitive attribute S.
#'
#' This function computes the mean of the given numeric vector,
#' ignoring any NA values present in the vector.
#'
#' @param train_data A dataset to learn the quantile and cumulative distribution functions.
#' @param test_data A dataset to test the results of quantile regression.
#' @param S_input The input value for S. Observations with a sensitive attribute different
#' from `S_input` will be transported. Otherwise, no changes.
#' @param top_order Topological order to perform quantile regression.
#' @param reg_method Regression method to use, either "linear" or with "splines".
#' The default is "linear".
#' @param tau Quantile levels to perform quantile regression.
#' #' The default is TRUE.
#' @return A tibble with `S_input`, transported `train_data` and `test_data`.
quant_reg <- function(
    train_data,
    test_data,
    S_input,
    top_order,
    reg_method = "linear",
    tau = c(0.001, seq(0.005, 0.995, by = 0.01), 0.999) # quantile levels
) {
  
  # Data rows with S equal to S_input: to be transported
  ind_t <- which(train_data$S == S_input)
  train_t <- train_data[ind_t, ]
  # Data rows with S different from S_input: no changes
  train_input <- train_data[-ind_t, ]
  
  # Transform train
  new_train <- train_data
  new_train_t <- train_t
  
  # Put S value to its intervention
  new_train_t$S <- as.factor(S_input)
  
  for (i_var in 2:length(top_order)) {
    var <- top_order[i_var]
    # S is considered as root node
    reg_vars <- top_order
    formula <- paste(var, "~", paste(reg_vars, collapse = " + "))
    # Convert the string to a formula object
    formula <- as.formula(formula)
    reg_input <- rq(formula, data = train_input, tau = tau)
    reg_non_ref <- rq(formula, data = train_non_ref, tau = tau)
    
    # Transform variable
    quant_non_ref <- predict(reg_non_ref, newdata = train_non_ref)
    quant_ref <- predict(reg_ref, newdata = new_train_non_ref)
    eval_non_ref <- train_non_ref[[var]]
    tau_non_ref <- vapply(
      seq_len(nrow(train_non_ref)),
      function(x) ecdf(quant_non_ref[x,]) (eval_non_ref[x]),
      numeric(1))
    infer_quant <- vapply(
      seq_len(nrow(quant_ref)),
      function(x) quantile(quant_ref[x,], tau_non_ref[x]),
      numeric(1))
    
    # Transform variable in a new dataset
    new_train_non_ref[[var]] <- infer_quant
    new_train[ind_non_ref, ] <- new_train_non_ref
  }
  tibble(
    S_input = S_input,
    new_train_data = new_train,
    new_test_data = NULL
  )
}

# Linear quantile regression: transform "male" to "female"
ind_male <- which(train_law$S == "male")
train_law_male <- train_law[ind_male, ]
train_law_female <- train_law[-ind_male, ]
tau = c(0.001, seq(0.005, 0.995, by = 0.01), 0.999) # quantile levels
# lsat|S
reg_lsat_male <- rq(lsat ~ 1, data = train_law_male, tau = tau)
reg_lsat_female <- rq(lsat ~ 1, data = train_law_female, tau = tau)
# ugpa|lsat,S
reg_ugpa_male <- rq(ugpa ~ lsat, data = train_law_male, tau = tau)
reg_ugpa_female <- rq(ugpa ~ lsat, data = train_law_female, tau = tau)
# Y|ugpa,lsat,S
reg_Y_male <- rq(Y ~ lsat+ugpa, data = train_law_male, tau = tau)
reg_Y_female <- rq(Y ~ lsat+ugpa, data = train_law_female, tau = tau)

# Transform train: set to "female"
new_train_law <- train_law
new_train_law_male <- train_law_male
# S="female"
new_train_law_male$S <- as.factor("female")
# Transform lsat
quant_lsat_male <- predict(reg_lsat_male, newdata = train_law_male)
quant_lsat_female <- predict(reg_lsat_female, newdata = new_train_law_male)
eval_lsat_male <- train_law_male$lsat
# Tau levels for lsat in original dataset
# Possible to optimize by only predicting for male
tau_lsat <- vapply(
  seq_len(nrow(train_law_male)),
  function(x) ecdf(quant_lsat_male[x,]) (eval_lsat_male[x]),
  numeric(1))
infer_quant_lsat <- vapply(
  seq_len(nrow(quant_lsat_female)),
  function(x) quantile(quant_lsat_female[x,], tau_lsat[x]),
  numeric(1))
new_train_law_male$lsat <- infer_quant_lsat

# Transform ugpa
quant_ugpa_male <- predict(reg_ugpa_male, newdata = train_law_male)
quant_ugpa_female <- predict(reg_ugpa_female, newdata = new_train_law_male)
eval_ugpa_male <- train_law_male$ugpa
# Tau levels for ugpa in original dataset
# Possible to optimize by only predicting for male
tau_ugpa <- vapply(
  seq_len(nrow(train_law_male)),
  function(x) ecdf(quant_ugpa_male[x,]) (eval_ugpa_male[x]),
  numeric(1))
infer_quant_ugpa <- vapply(
  seq_len(nrow(quant_ugpa_female)),
  function(x) quantile(quant_ugpa_female[x,], tau_ugpa[x]),
  numeric(1))
new_train_law_male$ugpa <- infer_quant_ugpa

# Transform Y (variable of interest -> counterfactual outcome)
quant_Y_male <- predict(reg_Y_male, newdata = train_law_male)
quant_Y_female <- predict(reg_Y_female, newdata = new_train_law_male)
eval_Y_male <- train_law_male$Y
# Tau levels for Y in original dataset
# Possible to optimize by only predicting for male
tau_Y <- vapply(
  seq_len(nrow(train_law_male)),
  function(x) ecdf(quant_Y_male[x,]) (eval_Y_male[x]),
  numeric(1))
infer_quant_Y <- vapply(
  seq_len(nrow(quant_Y_female)),
  function(x) quantile(quant_Y_female[x,], tau_Y[x]),
  numeric(1))
new_train_law_male$Y <- infer_quant_Y

new_train_law[ind_male, ] <- new_train_law_male

# With the function
tibble_train <- quant_reg(
  train_law,
  test_law,
  "female",
  c("lsat","ugpa","Y")
)
