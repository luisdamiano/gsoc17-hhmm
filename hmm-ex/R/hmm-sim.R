hmm_sim <- function(T, K, A, p.init, obs.sim) {
  if(!is.matrix(A) || any(dim(A) != K))
    stop("A must be a KxK matrix.")
  
  if(any(rowSums(A) != 1))
    stop("The rows of A must add up to 1.")
  
  if(min(A) < 0 || max(A) > 1)
    stop("The elements of A can only take values between the interval [0, 1].")
  
  z <- vector("numeric", T)
  z[1] <- sample(x = 1:K, size = 1, replace = FALSE, prob = p.init)
  for(t in 2:T) {
    z[t] <- sample(x = 1:K, size = 1, replace = FALSE, prob = A[z[t-1], ])
  }
  
  x <- do.call(obs.sim, list(z))
  
  list(
    z = z,
    x = x
  )
}