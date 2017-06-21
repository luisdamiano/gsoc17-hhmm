neighbouring_forecast <- function(x, oblik_t, h = 1, threshold = 0.05) {
  if (!is.vector(x) || length(x) != dim(oblik_t)[2])
    stop("The size of the observation vector and the width of the likelihood
         array must be equal.")

  n.samples <- dim(oblik_t)[1]
  T.length  <- dim(oblik_t)[2]

  find_closest <- function(target, candidates, threshold) {
    ind <- which(abs(target - candidates) < abs(target) * threshold)

    if (!length(ind))
      ind <- which(abs(target - candidates) == min(abs(target - candidates)))

    ind
  }

  forecast = vector("numeric", n.samples)
  for (n in 1:n.samples) {
    oblik_target <- oblik_t[n, T.length]
    oblik_cand   <- oblik_t[n, 1:(T.length - h)]

    closests <- find_closest(oblik_target, oblik_cand, threshold)
    d        <- abs(oblik_target - oblik_t[n, closests])
    w        <- exp(d)

    forecast[n] <- x[T.length] + sum((x[closests + h] - x[closests]) * w) / sum(w)
  }

  forecast
}
