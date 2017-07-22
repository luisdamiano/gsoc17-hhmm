library(rstan)
library(shinystan)
source('common/R/plots.R')
source('hmm/R/hmm-sim.R')

# Set up ------------------------------------------------------------------
T.length = 500
K = 2
A = matrix(c(0.80, 0.35, 0.20, 0.65), K, K)
p1 = c(0.90, 0.10)
obs.model <- function(z) { rnorm(length(z), z*10, 3)}

n.iter = 500
n.warmup = 250
n.chains = 4
n.cores = 4
n.thin = 1
n.seed = 9000

set.seed(9000)

# Data simulation ---------------------------------------------------------
dataset <- hmm_sim(T.length, K, A, p1, obs.model)

# Model estimation --------------------------------------------------------
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

stan.model = 'hmm/stan/hmm.stan'
stan.data = list(
  T = T.length,
  K = K,
  x = dataset$x
)

# Chains are initialized close to k-means to speed up convergence
init_fun <- function() {
  clasif <- kmeans(stan.data$x, stan.data$K)
  init.mu <- by(stan.data$x, clasif$cluster, mean)
  init.sigma <- by(stan.data$x, clasif$cluster, sd)
  init.order <- order(init.mu)

  list(
    mu_k = init.mu[init.order],
    sigma_k = init.sigma[init.order]
  )
}

stan.fit <- stan(file = stan.model,
                 model_name = stan.model,
                 data = stan.data, verbose = T,
                 iter = n.iter, warmup = n.warmup,
                 thin = n.thin, chains = n.chains,
                 cores = n.cores, seed = n.seed, init = init_fun)

n.samples = (n.iter - n.warmup) * n.chains

# MCMC Diagnostics --------------------------------------------------------
summary(stan.fit,
        pars = c('p_1k', 'A_ij', 'mu_k', 'sigma_k'),
        probs = c(0.50))$summary
launch_shinystan(stan.fit)

# Estimates ---------------------------------------------------------------

# Extraction
alpha_tk <- extract(stan.fit, pars = 'alpha_tk')[[1]]
gamma_tk <- extract(stan.fit, pars = 'gamma_tk')[[1]]

# Summary -----------------------------------------------------------------
options(digits = 2)

print("Estimated initial state probabilities")
summary(stan.fit,
        pars = c('p_1k'),
        probs = c(0.10, 0.50, 0.90))$summary[, c(1, 3, 4, 5, 6)]

print("Estimated probabilities in the transition matrix")
summary(stan.fit,
        pars = c('A_ij'),
        probs = c(0.10, 0.50, 0.90))$summary[, c(1, 3, 4, 5, 6)]

print("Estimated mean and standard deviation of observations in each state")
summary(stan.fit,
        pars = c('mu_k', 'sigma_k'),
        probs = c(0.10, 0.50, 0.90))$summary[, c(1, 3, 4, 5, 6)]

# Inference plots
print("Estimated hidden states (hard naive classification using filtered prob)")
print(table(
  estimated = apply(round(apply(alpha_tk, c(2, 3),
                                function(x) {
                                  quantile(x, c(0.50)) })), 1, which.max),
  real = dataset$z))
plot_stateprobability(alpha_tk, gamma_tk, 0.8, dataset$z)

# Most likely hidden path (Viterbi decoding) - joint states
zstar <- extract(stan.fit, pars = 'zstar_t')[[1]]
round(table(rep(dataset$z - 1, each = n.samples), zstar) / n.samples, 0)

plot_statepath(zstar, dataset$z)
