library(rstan)
library(shinystan)
source('iohmm-ex/R/iohmm-sim.R')

# Set up ------------------------------------------------------------------
T.length = 50
K = 3
M = 2
R = 1
w = matrix(c(2, 0.80, 3, 0.50, 1.5, 0.80), nrow = K, ncol = M, byrow = TRUE)
b = matrix(c(1, 5, 0.01, 0.01, -1, -5), nrow = K, ncol = M, byrow = TRUE)
s = c(0.1, 1, 2)
p1 = c(0.45, 0.10, 0.45)
obs.model <- function(u, z, b, s) {
  T.length <- nrow(u)

  x <- vector("numeric", T.length)
  for(t in 1:T.length) {
    x[t] <- rnorm(1, u[t, ] %*% b[z[t], ], s[z[t]])
  }
  return(x)
}

n.iter = 1000
n.warmup = 500
n.chains = 4
n.cores = 4
n.thin = 1
n.seed = 9000

set.seed(9000)

# Data simulation ---------------------------------------------------------
u <- matrix(rnorm(T.length*M, 0, 1), nrow = T.length, ncol = M)
dataset <- iohmm_sim(T.length, K, u, w, p1, obs.model, b, s)

# Data exploration --------------------------------------------------------
par(mfrow = c(1, 2))
mtext("Input-Output relationship", side = 3, outer = TRUE)
for(m in 1:M) {
  plot(x = u[, m], y = dataset$x,
       pch = 21, cex = 0.7,
       col = dataset$z, bg = dataset$z,
       ylab = bquote("Output" ~ x), xlab = bquote("Input" ~ u[.(m)]))
}

par(mfrow = c(M, K))
for(mm in 1:M) {
  for(k in 1:K) {
      plot(x = u[, m], y = dataset$p.mat[, k],
         ylim = c(0, 1),
         pch = 21, cex = 0.7,
         col = dataset$z, bg = dataset$z,
         ylab = bquote("Prob of state" ~ .(k)), xlab = bquote("Input" ~ u[.(m)]))
  }
}

# Model estimation --------------------------------------------------------
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

stan.model = 'iohmm-ex/stan/iohmm.stan'
stan.data = list(
  T = T.length,
  K = K,
  M = M,
  u_t = as.array(u),
  x_t = dataset$x
)

# Chains are initialized close to k-means to speed up convergence
init_fun <- function() {
  # clasif <- kmeans(stan.data$x, stan.data$K)
  # init.mu <- by(stan.data$x, clasif$cluster, mean)
  # init.sigma <- by(stan.data$x, clasif$cluster, sd)
  # init.order <- order(init.mu)
  # 
  # list(
  #   mu_k = init.mu[init.order],
  #   sigma_k = init.sigma[init.order]
  # )
  list(
    w_k = w,
    b_k = b,
    s_k = s,
    p_1k = 1,
  )
}

stan.fit <- stan(file = stan.model,
                 model_name = stan.model,
                 data = stan.data, verbose = T,
                 iter = n.iter, warmup = n.warmup,
                 thin = n.thin, chains = n.chains,
                 cores = n.cores, seed = n.seed)
                 # cores = n.cores, seed = n.seed, init = init_fun)

n.samples = (n.iter - n.warmup) * n.chains

# MCMC Diagnostics --------------------------------------------------------
summary(stan.fit, 
        pars = c('p_1k', 'w_k', 'b_k', 's_k'), 
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
