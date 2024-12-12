#' Fit a Logistic Regression Model to a Binary Outcome With or Without a
#' Sensitive Attribute.
#'
#' @description
#' The aim is to predict a binary outcome (Y) using a set of predictors (X)
#' that may include (aware model) or may not include (unaware model) a sensitive
#' binary attribute (S). A logistic regression is used to to so. It is fitted on
#' a train set. Predicted scores are then returned on both the train and the
#' test set.
#'
#' @param train_data Train set.
#' @param test_data Test set.
#' @param s Name of the sensitive attribute.
#' @param y Name of the target variable.
#' @param type If `"type=aware"`, the model includes the sensitive attributes,
#'        otherwise, if `type=unaware`, it does not.
#'
#' @returns A list with three elements:
#' * `model`: The estimated logistic regression model.
#' * `pred_train`: Estimated scores on the train set.
#' * `pred_test`: Estimated scores on the test set.
#'
#' @importFrom dplyr select
#' @importFrom rlang !!
#' @importFrom stats glm predict as.formula binomial
log_reg_train <- function(train_data,
                          test_data,
                          s,
                          y,
                          type = c("aware", "unaware")) {
  if (type == "unaware") {
    train_data_ <- train_data |> select(-!!s)
    test_data_ <- test_data |> select(-!!s)
  } else {
    train_data_ <- train_data
    test_data_ <- test_data
  }
  # Train the logistic regression model
  form <- paste0(y, "~.")
  model <- glm(as.formula(form), data = train_data_, family = binomial)
  # Predictions on train and test sets
  pred_train <- predict(model, newdata = train_data_, type = "response")
  pred_test <- predict(model, newdata = test_data_, type = "response")
  list(
    model = model,
    pred_train = pred_train,
    pred_test = pred_test
  )
}
