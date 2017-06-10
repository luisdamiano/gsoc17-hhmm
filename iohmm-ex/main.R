library(rstan)
library(shinystan)
source('iohmm-ex/R/plots.R')
source('iohmm-ex/R/iohmm-sim.R')

# Set up ------------------------------------------------------------------
T.length = 100
K = 3
M = 4
R = 1
intercept = FALSE
w = matrix(c(1.2, 0.5, 0.3, 0.1, 0.5, 1.2, 0.3, 0.1, 0.5, 0.1, 1.2, 0.1), nrow = K, ncol = M, byrow = TRUE)
b = matrix(c(5, 6, 7, 0.5, 1, 5, 0.01, -0.5, 0.01, -1, -5, 0.2), nrow = K, ncol = M, byrow = TRUE)
s = c(0.1, 0.1, 0.1)
p1 = c(0.45, 0.10, 0.45)
obs.model <- function(u, z, b, s) {
  T.length <- nrow(u)

  x <- vector("numeric", T.length)
  for (t in 1:T.length) {
    x[t] <- rnorm(1, u[t, ] %*% b[z[t], ], s[z[t]])
  }
  return(x)
}

n.iter = 400
n.warmup = 200
n.chains = 1
n.cores = 1
n.thin = 1
n.seed = 9000

set.seed(9000)

# Data simulation ---------------------------------------------------------
u <- matrix(rnorm(T.length*M, 0, 1), nrow = T.length, ncol = M)
if (intercept)
  u[, 1] = 1

dataset <- iohmm_sim(T.length, K, u, w, p1, obs.model, b, s)

# Data exploration --------------------------------------------------------
layout(matrix(c(1:M, rep(M + 1, M)), nrow = 2, ncol = M, byrow = TRUE),
       heights = c(0.95, 0.05))
for (m in 1:M) {
  plot(x = u[, m], y = dataset$x,
       pch = 21, cex = 0.7,
       col = dataset$z, bg = dataset$z,
       ylab = bquote("Output" ~ x), xlab = bquote("Input" ~ u[.(m)]))
}
mtext("Input-Output relationship", side = 3, line = -2.5, outer = TRUE)

opar <- par(); par(mai = c(0,0,0,0))
plot.new()
legend(x = "center",
       legend = bquote(.(paste("Hidden state", 1:K))),
       lwd = 3, col = sort(unique(dataset$z)), horiz = TRUE, bty = 'n')
par(opar)

layout(matrix(c(1:(M*K), rep((M*K) + 1, M)), nrow = K + 1, ncol = M, byrow = TRUE),
       heights = c(rep((1- 0.02)/K, K), 0.02))
for (m in 1:M) {
  for (k in 1:K) {
      plot(x = u[, m], y = dataset$p.mat[, k],
         ylim = c(0, 1),
         pch = 21, cex = 0.7,
         col = dataset$z, bg = dataset$z,
         ylab = bquote("Prob of state" ~ .(k)), xlab = bquote("Input" ~ u[.(m)]))
  }
}

opar <- par(); par(mai = c(0,0,0,0))
plot.new()
legend(x = "center",
       legend = bquote(.(paste("Hidden state", 1:K))),
       lwd = 3, col = sort(unique(dataset$z)), horiz = TRUE, bty = 'n')
par(opar)
mtext("Input-State probability relationship", side = 3, line = -2.5, outer = TRUE)

# Model estimation --------------------------------------------------------
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

stan.model = 'iohmm-ex/stan/iohmm.stan'
stan.data = list(
  T = T.length,
  K = K,
  M = M,
  u_tm = as.array(u),
  x_t = dataset$x
)

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
        pars = c('p_1k', 'w_km', 'b_km', 's_k'),
        probs = c(0.50))$summary
launch_shinystan(stan.fit)

# Extraction
oblik_tk <- extract(stan.fit, pars = 'oblik_tk')[[1]]
w_km <- extract(stan.fit, pars = 'w_km')[[1]]
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

print("Estimated regressors of hidden states")
summary(stan.fit,
        pars = c('w_km'),
        probs = c(0.10, 0.50, 0.90))$summary[, c(1, 3, 4, 5, 6)]

print("Estimated regressors and standard deviation of observations in each state")
summary(stan.fit,
        pars = c('b_km', 's_k'),
        probs = c(0.10, 0.50, 0.90))$summary[, c(1, 3, 4, 5, 6)]

# Inference plots
layout(matrix(1:(3*K), ncol = 3, nrow = K, byrow = TRUE))

for (k in 1:K) {
  # Filtered probabilities (forward algoritm) - Belief states
  plot_intervals(
    x = 1:T.length,
    y = apply(alpha, c(2, 3),
              function(x) {
                quantile(x, c(0.10, 0.50, 0.90)) })[, , k],
    z = dataset$z,
    k = k,
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
    z = dataset$z,
    k = k,
    xlab = bquote(t),
    ylab = bquote(p(z[t] == .(k) ~ "|" ~ x[" " ~ 1:T])),
    main = bquote("Smoothed probability for Hidden State" ~ .(k))
  )

  # Filtered vs smoothed
  cols <- ifelse(dataset$z == k, 'green', 'red')
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

  # Hard (naive) classification for hidden state
  print("Estimated hidden states (hard naive classification using filtered prob)")
  print(table(
          estimated = round(apply(alpha, c(2, 3),
                            function(x) {
                              quantile(x, c(0.50)) })[, k]),
          real = dataset$z))
}

# Most likely hidden path (Viterbi decoding) - joint states
# zstar <- apply(extract(stan.fit, pars = 'zstar_t')[[1]], 2, bin_std)
zstar <- extract(stan.fit, pars = 'zstar_t')[[1]]
round(table(
  actual = rep(dataset$z, each = n.samples),
  fit = zstar) / n.samples, 0)

par(mfrow = c(1, 1))
plot(
  x = 1:T.length,
  y = apply(zstar, 2, median),
  xlab = bquote(t),
  ylab = bquote(z),
  main = bquote("Sequence of states"),
  type = 'l', col = 'gray')

legend(x = 0.15 * T.length, y = K + 0.22,
       legend = c('Fit', paste('Actual ', 1:K)),
       pch = c(NA, rep(21, K)),
       lwd = c(2, rep(NA, K)),
       col = c('lightgray', 1:K),
       pt.bg = c('lightgray', 1:K),
       bty = 'n', cex = 0.7,
       horiz = TRUE, xpd = TRUE)

points(x = 1:T.length, y = dataset$z,
       pch = 21, bg = dataset$z, col = dataset$z, cex = 0.7)

