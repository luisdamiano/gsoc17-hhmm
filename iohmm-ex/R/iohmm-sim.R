source('R/math.R')

#' Draws a sample from an Input Output Hidden Markov Model
#'
#' @param T Length of the sequence.
#' @param K Number of hidden states.
#' @param u The input matrix. Each row represents the input vector
#' at a step t.
#' @param w Regressors matrix for the state model. Each row
#' represents the regressor set for a given state k.
#' @param b Regressors matrix for the observation model. Each row
#' represents the regressor set for a given state k.
#' @param p.init Initial state probability vector of size 1xK.
#' @param obs.model A function that draws a sample from the
#' observation model. It takes two arguments, a vector of discrete states
#' between 1 and K as well as a vector of regressors. It returns a vector of
#' same size containing the observations.
#'
#' @return A list with four elements:
#' *u* the input matrix,
#' *z* the vector of hidden states,
#' *x* the vector of observations, and
#' *p.mat* a matrix mapping the probability that an observation belong to each
#' one of the possible hidden states.
#' @export
#'
#' @examples
iohmm_sim <- function(T, K, u, w, p.init, obs.model, b, s) {
  m <- ncol(u)

  if (dim(u)[1] != T)
    stop("The input matrix must have T rows.")

  if (any(dim(w) != c(K, m)))
    stop("The transition weight matrix must be of size Kxm, where m is the size of the input vector.")

  if (any(dim(b) != c(K, m)))
    stop("The regressors matrix must be of size Kxm, where m is the size of the input vector.")

  if (length(p.init) != K)
    stop("The vector p.init must have length K.")

  p.mat <- matrix(0, nrow = T, ncol = K)
  p.mat[1, ] <- p.init

  z <- vector("numeric", T)
  z[1] <- sample(x = 1:K, size = 1, replace = FALSE, prob = p.init)
  for (t in 2:T) {
    p.mat[t, ] <- softmax(sapply(1:K, function(j) {u[t, ] %*% w[j, ]}))
    z[t] <- sample(x = 1:K, size = 1, replace = FALSE, prob = p.mat[t, ])
  }

  x <- do.call(obs.model, list(u, z, b, s))

  list(
    u = u,
    z = z,
    x = x,
    p.mat = p.mat
  )
}

#' Draws a sample from an observation model parametrized as a linear regression
#' with Gaussian density.
#'
#' @param u The input matrix. Each row represents the input vector
#' at a step t.
#' @param z The vector of hidden states.
#' @param b Regressors matrix for the observation model. Each row
#' represents the regressor set for a given state k.
#' @param s The vector of variance parameters. Each element represents the error
#' variance for a given state k.
#'
#' @return A vector of observations.
#' @export
#'
#' @examples
obs.model <- function(u, z, b, s) {
  T.length <- nrow(u)

  x <- vector("numeric", T.length)
  for (t in 1:T.length) {
    x[t] <- rnorm(1, u[t, ] %*% b[z[t], ], s[z[t]])
  }
  return(x)
}
