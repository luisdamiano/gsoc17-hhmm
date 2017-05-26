library(rstan)
library(shinystan)
source('hmm-ex/R/sim.R')

# Set up ------------------------------------------------------------------
T.length = 500
K = 2
A = matrix(c(0.80, 0.35, 0.20, 0.65), K, K)
p.init = c(0.90, 0.10)
obs.model <- function(z) { rnorm(length(z), z*10, 1)}

# Data simulation ---------------------------------------------------------
set.seed(9000)
dataset <- hmm.sim(T.length, K, A, p.init, obs.model)

# Model estimation --------------------------------------------------------
rstan_options(auto_write = TRUE)  # Writes down the compiled sampler
options(mc.cores = parallel::detectCores()) # Use all available cores

bmodel = 'hmm-ex/stan/hmm.stan'
standata = list(
  T = T.length,
  K = K,
  x = dataset$x
)

stan.fit <- stan(file = bmodel,
                 model_name = "HMM",
                 data = standata, verbose = T,
                 iter = 400, warmup = 200, thin = 1, chains = 8, cores = 4,
                 control = list(adapt_delta = 0.80))

# Diagnosis ---------------------------------------------------------------
summary(stan.fit, pars = c('A_ij', 'mu', 'sigma'), probs = c(0.50))

summary(stan.fit, pars = c('alpha'), probs = c(0.50))

summary(stan.fit, pars = c('alpha', 'z_star'), probs = c(0.50))

summary(stan.fit, probs = c(0.50))

launch_shinystan(stan.fit)
