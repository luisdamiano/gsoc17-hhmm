D = 2
T.length = 10

nodes <- list(
  list( # Node 1
    type    = "internal",
    d       = 1,
    pi_d    = c(0.2, 0.8), # K_d
    A_d     = matrix(c(0.2, 0.6, 0.2,
                       0.2, 0.6, 0.2,
                       0.2, 0.6, 0.2),
                       nrow = 2, ncol = 3, # K_d + 1,
                       byrow = TRUE),
    child   = list(
      list( # Node 1.1
        type    = "production",
        d       = 2,
        theta_i = c(-1, 1)
      ),
      list( # Node 1.2
        type    = "production",
        d       = 2,
        theta_i = c(-1, 1)
      )
    )
  )
)

obs <- vector("numeric", T.length)
activate <- function(current, parent, t = 1) {
  if (current$type == "internal") {
    K_d <- length(current$child)
    z_t <- sample(1:K_d, 1, prob = current$pi_d)
    activate(current$child[[z_t]], current)
  } else if (current$type == "production") {
    obs[t] <- rnorm(1, current$theta_i[1], current$theta_i[2])

    K_d <- length(current)
    z_t <- sample(1:K_d, 1, prob = current$pi_d)
    activate(current$parent[[z_t]], current)
  }
}
