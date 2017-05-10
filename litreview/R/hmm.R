library(rstan)
library(shinystan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# Murphy (2012) 17.4.4.3 Example ------------------------------------------
K = 3
A = matrix(data =
  c(0.3, 0.7, 0.0,
    0.0, 0.9, 0.1,
    0.0, 0.0, 0.4),
  nrow = K, ncol = K, byrow = TRUE
)

V = 7
TT = 4

z = c(1, 2, 2, 3)
x = c(1, 3, 4, 6)

phi = matrix(data =
               c(0.5, 0.3, 0.2, 0.0, 0.0, 0.0, 0.0,
                 0.0, 0.0, 0.2, 0.7, 0.1, 0.0, 0.0,
                 0.0, 0.0, 0.0, 0.1, 0.0, 0.5, 0.4),
           nrow = V, ncol = K, byrow = FALSE
)

prior1 = rep(x = 1, K)
prior2 = rep(x = 1, V)

stan.data <- list(
  K = K,
  V = V,
  T = TT,
  z = z,
  x = x,
  prior1 = prior1,
  prior2 = prior2
)

elap <- system.time({
  stan.fit <- stan(file = 'litreview\\stan\\hmm.stan',
                   model_name = "Murphy (2012) 17.4.4.3 Example", data = stan.data,
                   seed = 2700, iter = 1000, warmup = 500, thin = 1, chains = 1, verbose = T, cores = 4)
})

launch_shinystan(stan.fit)
