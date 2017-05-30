library(rstan)
library(shinystan)
source('hmm-ex/R/hmm-sim.R')
source('hmm-ex/R/plots.R')

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

stan.model = 'hmm-ex/stan/hmm.stan'
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
alpha <- extract(stan.fit, pars = 'alpha_tk')[[1]]
gamma <- extract(stan.fit, pars = 'gamma_tk')[[1]]

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
layout(matrix(c(1, 2, 3, 4, 5, 6, 7, 7, 7), ncol = 3, nrow = 3, byrow = TRUE))

for(k in 1:K) {
  # Filtered probabilities (forward algoritm) - Belief states
  plot_intervals(
    x = 1:T.length,
    y = apply(alpha, c(2, 3),
              function(x) { 
                quantile(x, c(0.10, 0.50, 0.90)) })[, , k],
    z = dataset$zstd,
    xlab = bquote(t),
    ylab = bquote(p(z[t] == .(k) ~ "|" ~ x[" " ~ 1:t])),
    main = bquote("Filtered probability for Hidden State" ~ .(k))
  )
  
  # Smoothed probability (forwards-backwards algorithm)
  plot_intervals(
    x = 1:T.length,
    y = apply(gamma, c(2, 3),
              function(x) { 
                quantile(x, c(0.10, 0.50, 0.90)) })[, , k],
    z = dataset$zstd,
    xlab = bquote(t),
    ylab = bquote(p(z[t] == .(k) ~ "|" ~ x[" " ~ 1:T])),
    main = bquote("Smoothed probability for Hidden State" ~ .(k))
  )
  
  # Filtered vs smoothed
  cols <- ifelse(dataset$z == 1, 'green', 'red')
  plot(
    x = apply(alpha, c(2, 3),
              function(x) { 
                quantile(x, c(0.50)) })[, k],
    y = apply(gamma, c(2, 3),
              function(x) { 
                quantile(x, c(0.50)) })[, k],
    xlab = bquote(p(z[t] == .(k) ~ "|" ~ x[" " ~ 1:t])),
    ylab = bquote(p(z[t] == .(k) ~ "|" ~ x[" " ~ 1:T])),
    main = bquote("Filtered vs smoothed probability for Hidden State" ~ .(k)),
    type = 'p', pch = 21, col = cols, bg = cols, cex = 0.7
  )
  abline(0, 1, col = 'lightgray', lwd = 0.25)
}

# Most likely hidden path (Viterbi decoding) - joint states
zstar <- apply(extract(stan.fit, pars = 'zstar_t')[[1]], 2, bin_std)
round(table(rep(dataset$z - 1, each = n.samples), zstar) / n.samples, 0)

plot(
  x = 1:T.length,
  y = bin_std(apply(zstar, 2, median)),
  xlab = bquote(t),
  ylab = bquote(z^~"*"),
  main = bquote("Most probable sequence of states"),
  type = 'l', col = 'gray')

cols <- ifelse(dataset$z == 1, 'green', 'red')
points(x = 1:T.length, y = dataset$zstd,
       pch = 21, bg = cols, col = cols, cex = 0.7)
