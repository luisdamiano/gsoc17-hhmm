library(rstan)
library(shinystan)
source('hmm-ex/R/hmm-sim.R')
source('hmm-ex/R/plot-addons.R')
source('hmm-ex/R/math.R')

# Set up ------------------------------------------------------------------
T.length = 500
K = 2
A = matrix(c(0.80, 0.35, 0.20, 0.65), K, K)
p1 = c(0.90, 0.10)
obs.model <- function(z) { rnorm(length(z), z*10, 5)}

n.iter = 1000
n.warmup = 500
n.chains = 4
n.thin = 1

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
                 init = init_fun)

# Diagnostics -------------------------------------------------------------
summary(stan.fit, 
        pars = c('p_1k', 'A_ij', 'mu_k', 'sigma_k'), 
        probs = c(0.50))
launch_shinystan(stan.fit)

# Estimates ---------------------------------------------------------------
# Let's see if the Stan implementation can recover well the parameters

# Filtered probability (forward algorithm)
alpha <- apply(
  extract(stan.fit, pars = 'alpha_tk')[[1]], c(1, 2),
  function(x) {softmax(x)})

plot(NULL, xlim = c(0, T.length), ylim = c(0, 1), type = 'l',
     xlab = bquote(t),
     ylab = bquote(p(z[t] == 1 ~ "|" ~ x[" " ~ 1:t])),
     main = "Filtered probability for Belief State 1")
interval_ribbons(x = 1:T.length,
                 y = t(apply(alpha, c(1, 3),
                             function(x) { 
                               quantile(x, c(0.10, 0.90)) })[, 1, ]),
                 col = 'lightgray')
lines(x = 1:T.length,
      y = apply(alpha, c(1, 3), median)[1, ],
      col = 'black')

boxplot(
  alpha ~ state, 
  data.frame(
    alpha = apply(alpha, c(1, 3), median)[1, ],
    state = dataset$z),
  # xlab = bquote(z[t]),
  # ylab = bquote(p(z[t] == 1 ~ "|" ~ x[" " ~ 1:t])),
  xlab = "Actual state",
  ylab = "Filtered probability for Belief State 1",
  main = "Filtered probability for Belief State 1",
  pch = 21, cex.pch = 0.7,
  outpch = 21, outcex = 0.8,
  outbg = "lightgray", outcol = "gray")
abline(h = 0.5, col = "lightgray")

hist(
  alpha[1, , dataset$z == 1],
  prob = TRUE, breaks = "FD", col = 2,
  main = "Filtered probability for Belief States",
  xlab = "Filtered probability")
hist(
  alpha[1, , ],
  prob = TRUE, breaks = "FD", col = 1, add = TRUE)
hist(
  alpha[1, , dataset$z == 2],
  prob = TRUE, breaks = "FD", col = 3, add = TRUE)
legend(x = "top", 
       legend = c(
         'All observations', 
         'Obs. in State 1', 
         'Obs. in State 2'),
       bty = 'n', cex = 0.7,
       col = 1:3, lwd = 5)

# Most likely path (joint states, Viterbi decoding)
zstar <- extract(stan.fit, pars = 'zstar_t')[[1]]
round(table(rep(dataset$z, each = 2000), zstar) / 2000, 0)
