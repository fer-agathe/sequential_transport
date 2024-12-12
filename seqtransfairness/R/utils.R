#' Define the function to split the dataset into train/test sets
#'
#' @param data dataset
#' @param seed seed number to use for replication
#' @param train_ratio ratio of data to use in the train set (default to 0.7)
#'
#' @returns A list with two elements:
#' * `data_train`: The train set.
#' * `data_test`: The test set.
#'
#' @md
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
