library(rstan)
library(shinystan)
source('common/R/plots.R')
source('hmm/R/hmm-sim.R')

# Set up ------------------------------------------------------------------
T.length = 500
K = 4
L = 9
G = 2
g = c(1, 2, 2, 1) # 1 = D, 2 = U
p1 = c(0.50, 0.00, 0.50, 0.00)
A = matrix(c(0.00, 0.50, 0.50, 0.00,
             1.00, 0.00, 0.00, 0.00,
             0.50, 0.00, 0.00, 0.50,
             0.00, 0.00, 1.00, 0.00),
           K, K, TRUE)
# O = matrix(1:18, G, L, TRUE)
# B = matrix(c(1:L / sum(1:L),  # g1 -
#              1:L / sum(1:L),  # g2 +
#              L:1 / sum(1:L),  # g3 +
#              L:1 / sum(1:L)), # g4 -
#            K, L, TRUE)
# obs.model <- function(z, g, B, O) {
#   sapply(1:length(z), function(i) {
#     O[g[z[i]], which.max(rmultinom(1, 1, B[z[i], ]))]
#   })
# }

O = matrix(1:L, 1, L, TRUE)
B = matrix(c(1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,  # g1 -
             0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,  # g2 +
             0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,  # g3 +
             0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0), # g4 -
           K, L, TRUE)

obs.model <- function(z, g, B, O) {
  sapply(1:length(z), function(i) {
    O[which.max(rmultinom(1, 1, B[z[i], ]))]
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
dataset <- hmm_sim(T.length, K, A, p1, function(z) {obs.model(z, g, B, O)})

# Model estimation --------------------------------------------------------
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

stan.model = 'hmm/stan/hmm-multinom-semisup.stan'
stan.data = list(
  T = T.length,
  K = K,
  L = L,
  G = G,
  g = g[dataset$z],
  x = ifelse(dataset$x < 10, dataset$x, dataset$x - 9)
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

matrix(summary(stan.fit,
               pars = c('A_ij'),
               probs = c(0.10, 0.50, 0.90))$summary[, c(1, 3, 4, 5, 6)][, 4], 4, 4, TRUE)

print("Estimated event probabilities in each state")
summary(stan.fit,
        pars = c('phi_k'),
        probs = c(0.10, 0.50, 0.90))$summary[, c(1, 3, 4, 5, 6)]

# Inference plots
print("Estimated hidden states (hard naive classification using filtered prob)")
print(table(
  estimated = apply(apply(alpha_tk, c(2, 3), median), 1, which.max),
  # estimated = apply(round(apply(alpha_tk, c(2, 3),
  #                               function(x) {
  #                                 quantile(x, c(0.50)) })), 1, which.max),
  real = dataset$z))
plot_stateprobability(alpha_tk, gamma_tk, 0.8, dataset$z)

# Most likely hidden path (Viterbi decoding) - joint states
zstar <- extract(stan.fit, pars = 'zstar_t')[[1]]
round(table(rep(dataset$z, each = n.samples), zstar) / n.samples, 0)

plot_statepath(zstar, dataset$z)


# a <- apply(alpha_tk, c(2, 3), median)
# a1 <- apply(alpha_tk, c(2, 3), median)[, 1] + apply(alpha_tk, c(2, 3), median)[, 4]
# a2 <- apply(alpha_tk, c(2, 3), median)[, 2] + apply(alpha_tk, c(2, 3), median)[, 3]
# plot(a1, a2)
# plot(dataset$z, a1)
# plot(dataset$z, a2)
#
# table(dataset$z, apply(a, 1, which.max))
#


