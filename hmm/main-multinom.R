library(rstan)
library(shinystan)
source('common/R/plots.R')
source('hmm/R/hmm-sim.R')

# Set up ------------------------------------------------------------------
T.length = 500
K = 2
L = 2
A = matrix(c(0.80, 0.35, 0.20, 0.65), K, K)
p1 = c(0.90, 0.10)
obs.model <- function(z) {
  probs <- matrix(c(0.9, 0.1, 0.1, 0.9), K, L, TRUE)
  sapply(1:length(z), function(i) {
    which.max(rmultinom(1, 1, probs[z[i], ]))
  })
}

T.length = 5000
K = 4
L = 9
A = matrix(c(0.10, 0.20, 0.30, 0.40,
             0.30, 0.20, 0.40, 0.10,
             0.20, 0.10, 0.30, 0.40,
             0.40, 0.30, 0.20, 0.10),
           K, K, TRUE)
p1 = c(0.20, 0.20, 0.20, 0.40)
obs.model <- function(z) {
  probs <- matrix(c(1:L / sum(1:L),
                    L:1 / sum(1:L),
                    (1:L)^2 / sum((1:L)^2),
                    (1:L)^3 / sum((1:L)^3)),
                  K, L, TRUE)
  sapply(1:length(z), function(i) {
    which.max(rmultinom(1, 1, probs[z[i], ]))
  })
}

n.iter = 500
n.warmup = 250
n.chains = 1
n.cores = 1
n.thin = 1
n.seed = 9000

set.seed(9000)

# Data simulation ---------------------------------------------------------
dataset <- hmm_sim(T.length, K, A, p1, obs.model)

# Model estimation --------------------------------------------------------
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

stan.model = 'hmm/stan/hmm-multinom.stan'
stan.data = list(
  T = T.length,
  K = K,
  L = L,
  x = dataset$x
)

stan.fit <- stan(file = stan.model,
                 model_name = stan.model,
                 data = stan.data, verbose = T,
                 iter = n.iter, warmup = n.warmup,
                 thin = n.thin, chains = n.chains,
                 cores = n.cores, seed = n.seed)#w, init = init_fun)

n.samples = (n.iter - n.warmup) * n.chains

# MCMC Diagnostics --------------------------------------------------------
summary(stan.fit,
        pars = c('p_1k', 'A_ij', 'phi_k'),
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

print("Estimated event probabilities in each state")
summary(stan.fit,
        pars = c('phi_k'),
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
round(table(rep(dataset$z, each = n.samples), zstar) / n.samples, 0)

plot_statepath(zstar, dataset$z)

