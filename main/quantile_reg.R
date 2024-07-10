# Import packages
library(dplyr)
library(ggplot2)
library(fairadapt)
library(quantreg)
set.seed(2024)
#library(CASdatasets)

# Import data
law_data <- read.csv("data/bar_pass_prediction.csv")

# Select variables
law_data <- law_data %>% 
  select(
    #pass_bar,
    zfygpa, # normalized gpa first year
    lsat,
    ugpa,
    gender
  )
str(law_data)
law_data <- na.omit(law_data)
# Missing values
which(is.na(law_data), arr.ind=TRUE) # no NA
ind_delete <- which(law_data$gender=="")
law_data <- law_data[-ind_delete,]

# Target variable
#law_data$pass_bar <- as.factor(law_data$pass_bar)
# Sensitive attribute
law_data$gender <- as.factor(law_data$gender)

# Rename variables
law_data <- law_data %>% 
  rename(
    Y = zfygpa,
    S = gender
  )

# Specify the causal graph
law_data <- law_data %>% 
  select(
    S,
    lsat,
    ugpa,
    Y
  )
variables <- colnames(law_data)
# Adjacency matrix: upper triangular
adj <- matrix(
  c(0, 1, 1, 1,
    0, 0, 1, 1,
    0, 0, 0, 1,
    0, 0, 0, 0),
  ncol = length(variables), 
  dimnames = rep(list(variables), 2),
  byrow = TRUE)
# See the graph
causal_graph <- graphModel(adj)
plot(causal_graph)

# Linear quantile regression
# Split train/test set
n <- nrow(law_data)
n_train <- round(0.8*n)
train_indices <- sample(seq_len(n), size = n_train, replace = FALSE)

# Split the data into training and testing sets
train_law <- law_data[train_indices, ]
test_law <- law_data[-train_indices, ]

#train_data <- train_law
#S_ref <- "female"
#top_order <- c("lsat","ugpa","Y")

quant_reg <- function(
    train_data,
    test_data,
    S_ref,
    top_order, # topological order
    reg_method = NULL,
    tau = c(0.001, seq(0.005, 0.995, by = 0.01), 0.999) # quantile levels
) {
  
  ind_non_ref <- which(train_data$S != S_ref)
  train_non_ref <- train_data[ind_non_ref, ]
  train_ref <- train_data[-ind_non_ref, ]
  
  # Transform train
  new_train <- train_data
  new_train_non_ref <- train_non_ref
  
  # S = ref
  new_train_non_ref$S <- as.factor(S_ref)
  
  for (i_var in 1:length(top_order)) {
    var = top_order[i_var]
    if (i_var == 1) {
      reg_vars <- 1
    } else {
      reg_vars <- top_order[1:(i_var-1)]
    }
    formula <- paste(var, "~", paste(reg_vars, collapse = " + "))
    # Convert the string to a formula object
    formula <- as.formula(formula)
    reg_ref <- rq(formula, data = train_ref, tau = tau)
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
