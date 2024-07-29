# Define the function to split the dataset into train/test sets
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

# Train and predict a logistic regression classifier: aware model
log_reg_train <- function(train_data, test_data, type = c("aware", "unaware")) {
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