% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/seq-transport.R
\name{seq_trans}
\alias{seq_trans}
\title{Sequential Transport Using a Pre-Defined Causal Graph}
\usage{
seq_trans(data, adj, s, S_0, y, num_neighbors = 5, silent = FALSE)
}
\arguments{
\item{data}{Data frame with the observations.}

\item{adj}{Adjacency matrix for the causal graph.}

\item{s}{Name of the sensitive attribute column in the data.}

\item{S_0}{Label of the sensitive attribute in the source distribution.}

\item{y}{Name of the outcome variable in the data.}

\item{num_neighbors}{Number of neighbors to use in the weighted quantile
estimation. Default to 5.}

\item{silent}{If \code{TRUE}, the messages showing progress in the estimation are
not shown. Default to \code{silent=FALSE}.}
}
\value{
An element of class \code{"sequential_transport"} (a list):
\itemize{
\item \code{transported}: A named list with the transported values. The names are those of the variables.
\item \code{weights}: A list with the weights of each observation in the two groups.
\item \code{ecdf}: A list with empirical distribution functions for numerical variables.
\item \code{ecdf_values}: A list with the values of the ecdf evaluated for each observation in the source distribution.
\item \code{fit_for_categ}: A list with the estimated multinomial models to predict categories using parents characteristics
\item \code{params}: A list with some parameters used to transport observations:
\itemize{
\item \code{adj}: Adjacency matrix.
\item \code{top_order}: Topological ordering.
\item \code{s}: Name of the sensitive attribute.
\item \code{S_0}: Label of the sensitive attribute in the source distribution.
\item \code{S_1}: Label of the sensitive attribute in the target distribution.
\item \code{y}: Name of the outcome variable in the data.
\item \code{num_neighbors}: Number of neighbors used when computing quantiles.
}
}
}
\description{
The sensitive attribute, S, is assumed to be a binary variable with value
$S_0$ in the source distribution and $S_1$ in the target distribution.
}
\examples{
# Data with two groups: S=0, S=1, an outcome Y and two covariates X1 and X2
sim_dat <- simul_dataset()
# Causal graph:
variables <- c("S", "X1", "X2", "Y")
adj <- matrix(
  # S  X1 X2 Y
  c(0, 1, 1, 1,# S
    0, 0, 1, 1,# X1
    0, 0, 0, 1,# X2
    0, 0, 0, 0  # Y
  ),
  ncol = length(variables),
  dimnames = rep(list(variables), 2),
  byrow = TRUE
)
# To visualize the causal graph:
# causal_graph <- fairadapt::graphModel(adj)
# plot(causal_graph)

# Sequential transport according to the causal graph
transported <- seq_trans(data = sim_dat, adj = adj, s = "S", S_0 = 0, y = "Y")
transported
# Transported values from S=0 to S=1, using the causal graph.
transported_val <- as.data.frame(transported$transported)
head(transported_val)
}
\seealso{
\code{\link[=seq_trans_new]{seq_trans_new()}}, \code{\link[=simul_dataset]{simul_dataset()}}
}
