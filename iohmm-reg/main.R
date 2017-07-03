library(rstan)
library(shinystan)
source('common/R/math.R')
source('common/R/plots.R')
source('iohmm-reg/R/iohmm-sim.R')

# Set up ------------------------------------------------------------------

# Data
T.length = 300
K = 3
M = 4
R = 1
u.intercept = FALSE
w = matrix(
  c(1.2, 0.5, 0.3, 0.1, 0.5, 1.2, 0.3, 0.1, 0.5, 0.1, 1.2, 0.1),
  nrow = K, ncol = M, byrow = TRUE)
b = matrix(
  c(5.0, 6.0, 7.0, 0.5, 1.0, 5.0, 0.1, -0.5, 0.1, -1.0, -5.0, 0.2),
  nrow = K, ncol = M, byrow = TRUE)
s = c(0.2, 1.0, 2.5)
p1 = c(0.4, 0.2, 0.4)

# Markov Chain Monte Carlo
n.iter = 400
n.warmup = 200
n.chains = 1
n.cores = 1
n.thin = 1
n.seed = 9000

# Data simulation ---------------------------------------------------------
set.seed(n.seed)
u <- matrix(rnorm(T.length*M, 0, 1), nrow = T.length, ncol = M)
if (u.intercept)
  u[, 1] = 1

dataset <- iohmm_sim(T.length, K, u, w, p1, obsmodel_reg, obs.pars = list(b = b, s = s))

# Data exploration --------------------------------------------------------
plot_inputoutput(x = dataset$x, u = dataset$u, z = dataset$z)
plot_inputprob(u = dataset$u, p.mat = dataset$p.mat, z = dataset$z)

# Model estimation --------------------------------------------------------
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

stan.model = 'iohmm-reg/stan/iohmm-reg.stan'
stan.data = list(
  T = T.length,
  K = K,
  M = M,
  u_tm = as.array(u),
  x_t = dataset$x
)

stan.fit <- stan(file = stan.model,
                 data = stan.data, verbose = T,
                 iter = n.iter, warmup = n.warmup,
                 thin = n.thin, chains = n.chains,
                 cores = n.cores, seed = n.seed)

n.samples = (n.iter - n.warmup) * n.chains

# MCMC Diagnostics --------------------------------------------------------
options(digits = 2)
summary(stan.fit,
        pars = c('p_1k', 'w_km', 'b_km', 's_k'),
        probs = c(0.50))$summary
launch_shinystan(stan.fit)

# Extraction --------------------------------------------------------------
alpha_tk <- extract(stan.fit, pars = 'alpha_tk')[[1]]
gamma_tk <- extract(stan.fit, pars = 'gamma_tk')[[1]]
zstar_t <- extract(stan.fit, pars = 'zstar_t')[[1]]
hatx_t <- extract(stan.fit, pars = 'hatx_t')[[1]]

# Relabelling (ugly hack edition) -----------------------------------------
dataset$zrelab <- rep(0, T)

hard <- sapply(1:T.length, function(t, med) {
  which.max(med[t, ])
}, med = apply(alpha_tk, c(2, 3),
                    function(x) {
                      quantile(x, c(0.50)) }))

tab <- table(hard = hard, original = dataset$z)

for (k in 1:K) {
  dataset$zrelab[which(dataset$z == k)] <- which.max(tab[, k])
}

print("Label re-imputation (relabeling due to switching labels)")
table(new = dataset$zrelab, original = dataset$z)

# Estimation summary ------------------------------------------------------
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

print("Observations with no imputation by the smoother (check)")
sum(apply(gamma_tk, 1, rowSums) == 0)

# Inference summary -------------------------------------------------------
# Filtered and smoothed state probability plot
plot_stateprobability(alpha_tk, gamma_tk, 0.8, dataset$zrelab)

# Confusion matrix for hard (naive) classification
print("Estimated hidden states (hard naive classification using filtered probability)")
print(table(
  estimated = apply(round(apply(alpha_tk, c(2, 3),
                                function(x) {
                                  quantile(x, c(0.50)) })), 1, which.max),
  real = dataset$zrelab))

# Jointly most likely state path (Viterbi decoding)
plot_statepath(zstar_t, dataset$zrelab)

# Confusion matrix for jointly most likely state path
print("Estimated hidden states for the jointly most likely path (Viterbi decoding)")
round(table(
  actual = rep(dataset$zrelab, each = n.samples),
  fit = zstar_t) / n.samples, 0)

# Fitted output
plot_outputfit(dataset$x, hatx_t, z = dataset$zrelab, TRUE)

