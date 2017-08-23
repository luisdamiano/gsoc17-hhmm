library(rstan)
library(shinystan)
library(xts)

set.seed(9000)

# HHMM structure: K production/emission states, L possible outcomes
T.length = 1000
K = 4
L = 9

p_1k  <- exp(rnorm(K))
p_1k  <- p_1k / sum(p_1k)

A_ij  <- matrix(exp(rnorm(K*K)), K, K)
A_ij  <- A_ij / rowSums(A_ij)

# Odds only from states 1 and 2
# Even only from states 3 and 4
phi_k <- matrix(exp(rnorm(K*L)), K, L)
# phi_k[1, 1:4] <- 0
# phi_k[2, 1:4] <- 0
# phi_k[3, 5:9] <- 0
# phi_k[4, 5:9] <- 0
phi_k <- phi_k / rowSums(phi_k)

z <- vector("numeric", T.length)
z[1] <- sample(x = 1:K, size = 1, replace = FALSE, prob = p_1k)
for (t in 2:T) {
  z[t] <- sample(x = 1:K, size = 1, replace = FALSE, prob = A_ij[z[t - 1], ])
}

x <- vector("numeric", T.length)
for (t in 1:T) {
  x[t] <- sample(x = 1:L, size = 1, replace = FALSE, prob = phi_k[z[t], ])
}

table(z, x)

# Stan! -------------------------------------------------------------------
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

stan.model = 'tayal2009/stan/hhmm-constraints.stan'

stan.data = list(
  T = T.length,
  K = K,
  L = L,
  x = x)

stan.fit <- stan(file = stan.model, data = stan.data,
                 verbose = TRUE, chains = 1,
                 seed = 9000, iter = 500, warmup = 250)
