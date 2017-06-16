#' Draws a simulated sample from a Hidden Markov Model
#'
#' @param T Length of the sequence
#' @param K Number of hidden states
#' @param A Transition matrix of size KxK
#' @param p.init Initial state probability vector of size Kx1
#' @param obs.sim A function that draws a simulated sample from the observation
#' model. It takes only one argument, a vector of discrete states between 1 and
#' K, and returns a vector of same size containing the sampled observations.
#'
#' @return A list with three elements:
#' *z* the vector of hidden states, and
#' *x* the vector of observations.
#' @export
#'
#' @examples
hmm_sim <- function(T, K, A, p.init, obs.sim) {
  if (!is.matrix(A) || any(dim(A) != K))
    stop("A must be a KxK matrix.")

  if (any(rowSums(A) != 1))
    stop("The rows of A must add up to 1.")

  if (min(A) < 0 || max(A) > 1)
    stop("The elements of A can only take values between the interval [0, 1].")

  if (length(p.init) != K)
    stop("The vector p.init must have length K.")

  z <- vector("numeric", T)
  z[1] <- sample(x = 1:K, size = 1, replace = FALSE, prob = p.init)
  for (t in 2:T) {
    z[t] <- sample(x = 1:K, size = 1, replace = FALSE, prob = A[z[t - 1], ])
  }

  x <- do.call(obs.sim, list(z))

  list(
    z = z,
    x = x
  )
}
