source('iohmm-ex/R/math.R')

# bin_std <- function(z, base = min(z)) {
#   as.numeric(!(z - base))
# }

#' Draws a simulated sample from a Hidden Markov Model
#'
#' @param T Length of the sequence.
#' @param K Number of hidden states.
#' @param m Size of the input vector.
#' @param u The input matrix of size Txm. Each row represents the input vector at a point in time t.
#' @param w Transition weight matrix of size Kxm for the state model. Each row represents the transition weight for a given state k.
#' @param r Size of the output vector.
#' @param b Regressors matrix of size Kxr for the observation model. Each row represents the regressors for a given state k.
#' @param p.init Initial state probability vector of size Kx1
#' @param obs.model A function that draws a simulated sample from the observation model. It takes two arguments, a vector of discrete states between 1 and K and a vector of regressors, and returns a vector of same size containing the sampled observations.
#'
#' @return A list with three elements: *z* the vector of hidden states in the form 1:K, *zcl* the vector of hidden states in the standarized from 0:(K-1), and *x* the vector of observations.
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
