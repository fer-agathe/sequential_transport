set.seed(2025)
n <- 1000
S <- rbinom(n, 1, 0.5)
X1 <- rnorm(n, mean = 0, sd = 1)
X2 <- rnorm(n, mean = 5, sd = 2)
Y <- 2 * S + 0.5 * X1 - 0.3 * X2 + rnorm(n)
data <- data.frame(S, X1, X2, Y)
variables <- colnames(data)
# Adjacency matrix: upper triangular
adj <- matrix(
  c(0, 1, 1, 1,
    0, 0, 1, 1,
    0, 0, 0, 1,
    0, 0, 0, 0),
  ncol = length(variables), 
  dimnames = rep(list(variables), 2),
  byrow = TRUE)
topologicalOrdering(adj)