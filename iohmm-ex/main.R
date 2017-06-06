library(rstan)
library(shinystan)
source('iohmm-ex/R/iohmm-sim.R')

# Set up ------------------------------------------------------------------
T.length = 200
K = 3
M = 2
R = 1
w = matrix(c(2, 0.80, 3, 0.50, 1.5, 0.80), nrow = K, ncol = M, byrow = TRUE)
b = matrix(c(1, 5, 0.01, 0.01, -1, -5), nrow = K, ncol = M, byrow = TRUE)
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

n.iter = 200
n.warmup = 100
n.chains = 12
n.cores = 3
n.thin = 1
n.seed = 9000

set.seed(9000)

# Data simulation ---------------------------------------------------------
u <- matrix(rnorm(T.length*M, 0, 1), nrow = T.length, ncol = M)
dataset <- iohmm_sim(T.length, K, u, w, p1, obs.model, b, s)

# Data exploration --------------------------------------------------------
layout(matrix(c(1, 2, 3, 3), nrow = 2, ncol = 2, byrow = TRUE),
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

layout(rbind(matrix(c(1:(M*K)), nrow = M, ncol = K, byrow = TRUE), (M*K) + 1),
       heights = c(rep((1 - 0.05) / M, M), 0.05))
for (m in 1:M) {
  for (k in 1:K) {
      plot(x = u[, m], y = dataset$p.mat[, k],
         ylim = c(0, 1),
         pch = 21, cex = 0.7,
         col = dataset$z, bg = dataset$z,
         ylab = bquote("Prob of state" ~ .(k)), xlab = bquote("Input" ~ u[.(m)]))
  }
}
mtext("Input-State probability relationship", side = 3, line = -2.5, outer = TRUE)

opar <- par(); par(mai = c(0,0,0,0))
plot.new()
legend(x = "center",
       legend = bquote(.(paste("Hidden state", 1:K))),
       lwd = 3, col = sort(unique(dataset$z)), horiz = TRUE, bty = 'n')
par(opar)

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

# Chains are initialized close to k-means to speed up convergence
# init_fun <- function() {
#   list(
#     w_km = w,
#     b_km = b,
#     s_k = s,
#     p_1k = 1
#   )
# }

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

# Parameters --------------------------------------------------------------
plot(x = dataset$x, y = apply(extract(stan.fit, pars = 'hatx_t')[[1]], 2, median))

tmp <- apply(extract(stan.fit, pars = 'hatx_t')[[1]], 2, function(x) {quantile(x, c(0.10, 0.50, 0.90))})
plot(
  x = dataset$x,
  y = tmp[2, ],
  xlim = c(-15, 15),
  ylim = c(-15, 15),
  xlab = bquote(t),
  ylab = bquote(p(z[t] == .(k) ~ "|" ~ x[" " ~ 1:t])),
  main = bquote("Filtered probability for Hidden State" ~ .(k))
)

points(
  x = dataset$x,
  y = tmp[1, ]
)

points(
  x = dataset$x,
  y = tmp[3, ]
)

# layout(rbind(matrix(c(1:(M*K)), nrow = M, ncol = K, byrow = TRUE), (M*K) + 1),
#        heights = c(rep((1 - 0.05) / M, M), 0.05))
# for (m in 1:M) {
#   for (k in 1:K) {
#     plot(x = rep(w[k, m], n.samples), y = w_km[, k, m],
#          pch = 21, cex = 0.7,
#          col = dataset$z, bg = dataset$z,
#          ylab = bquote("Fitted prob of state" ~ hat(w)[.(k, m)]), xlab = bquote("Prob of state" ~ w[.(k, m)]))
#   }
# }
# mtext("Input-State probability relationship", side = 3, line = -2.5, outer = TRUE)
#
# opar <- par(); par(mai = c(0,0,0,0))
# plot.new()
# legend(x = "center",
#        legend = bquote(.(paste("Hidden state", 1:K))),
#        lwd = 3, col = sort(unique(dataset$z)), horiz = TRUE, bty = 'n')
# par(opar)

# Estimates ---------------------------------------------------------------
# alpha <- extract(stan.fit, pars = 'alpha_tk')[[1]]

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
       horiz = TRUE, xpd=TRUE)

points(x = 1:T.length, y = dataset$z,
       pch = 21, bg = dataset$z, col = dataset$z, cex = 0.7)
