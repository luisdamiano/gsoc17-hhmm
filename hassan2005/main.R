library(quantmod)
library(rstan)
library(shinystan)
source('hassan2005/R/data.R')
source('hassan2005/R/forecast.R')
source('common/R/math.R')
source('common/R/plots.R')
source('iohmm-mix/R/iohmm-mix-init.R')

# Set up ------------------------------------------------------------------

# Model - IOHMM
K = 4
L = 3

# Model - Hyperparameters
hyperparams <- c(0, 5, 5, 0, 3, 1, 1, 0, 5);

# Markov Chain Monte Carlo
n.iter = 800
n.warmup = 400
n.chains = 1
n.cores = 1
n.thin = 1
n.seed = 9000

# Data
syms <- data.frame(
  symbol    = c("LUV", "RYA.L"),
  name      = c("Southwest Airlines Co", "Ryanair Holdings Plc"),
  train.from = c("2002-12-18", "2003-05-06"),
  train.to   = c("2004-07-23", "2004-12-06"),
  test.from  = c("2004-07-24", "2004-12-07"),
  test.to    = c("2004-11-17", "2005-03-17"),
  src        = c("yahoo", "yahoo"),
  stringsAsFactors = FALSE)

# ------------------------------------------------------------------------
# First, we analyse step-by-step our model for a given stock and date. We
# validate that the results of our sampler are reasonable. We then move
# into an automated rolling window forecasting process.
# ------------------------------------------------------------------------

# Data fetching and pre-processing ----------------------------------------
symbol <- syms[1, ] # LUV
prices   <- getSymbols(symbol$symbol,
                       env  = NULL,
                       from = symbol$train.from,
                       to   = symbol$test.to,
                       src  = symbol$src)
T.length <- nrow(prices[paste(symbol$train.from, symbol$train.to, sep = "/")])
dataset <- make_dataset(prices[1:T.length, ], TRUE)

# Data exploration --------------------------------------------------------
plot_inputoutput(x = dataset$x, u = dataset$u,
                 x.label = symbol$symbol,
                 u.label = c("Open", "High", "Low", "Close"))

# Model estimation --------------------------------------------------------
rstan_options(auto_write = TRUE)

stan.model = 'iohmm-mix/stan/iohmm-hmix.stan'
stan.data = list(
  K = K,
  L = L,
  M = ncol(dataset$u),
  T = length(dataset$x),
  u_tm = as.array(dataset$u),
  x_t = as.vector(dataset$x),
  hyperparams = as.array(hyperparams)
)

stan.fit <- stan(file = stan.model,
                 data = stan.data, verbose = T,
                 iter = n.iter, warmup = n.warmup,
                 thin = n.thin, chains = n.chains,
                 cores = n.cores, seed = n.seed,
                 init = function() {init_fun(stan.data)})

n.samples = (n.iter - n.warmup) * n.chains

# MCMC Diagnostics --------------------------------------------------------
options(digits = 4)
summary(stan.fit,
        pars = c('p_1k', 'w_km', 'lambda_kl', 'mu_kl', 's_kl'),
        probs = c(0.50))$summary
launch_shinystan(stan.fit)

# Extraction --------------------------------------------------------------
alpha_tk <- extract(stan.fit, pars = 'alpha_tk')[[1]]
gamma_tk <- extract(stan.fit, pars = 'gamma_tk')[[1]]
zstar_t  <- extract(stan.fit, pars = 'zstar_t')[[1]]
hatx_t   <- extract(stan.fit, pars = 'hatx_t')[[1]]
oblik_t  <- extract(stan.fit, pars = 'oblik_t')[[1]]
logA_ij  <- extract(stan.fit, pars = 'logA_ij')[[1]]

# Estimation summary ------------------------------------------------------
print("Estimated initial state probabilities")
summary(stan.fit,
        pars = c('p_1k'),
        probs = c(0.10, 0.50, 0.90))$summary[, c(1, 3, 4, 5, 6)]

print("Estimated regressors of hidden states")
summary(stan.fit,
        pars = c('w_km'),
        probs = c(0.10, 0.50, 0.90))$summary[, c(1, 3, 4, 5, 6)]

print("Estimated component mean and standard deviation in each state")
summary(stan.fit,
        pars = c('mu_kl', 's_kl'),
        probs = c(0.10, 0.50, 0.90))$summary[, c(1, 3, 4, 5, 6)]

print("Observations with no imputation by the smoother (check)")
sum(apply(gamma_tk, 1, rowSums) == 0)

# Inference summary -------------------------------------------------------
# Filtered and smoothed state probability plot
plot_stateprobability(alpha_tk, gamma_tk, 0.8)

# Filtered state probability and input plot
plot_inputprob(dataset$u.unscaled,
               apply(logA_ij, c(2, 3), function(x) { median(exp(x)) }),
               u.label = c("Open", "High", "Low", "Close"))

# Jointly most likely state path (Viterbi decoding)
plot_statepath(zstar_t)

# Fitted output
plot_outputfit(dataset$x, hatx_t, TRUE)

plot_inputoutputprob(x = dataset$x, u = dataset$u,
                     stateprob = alpha_tk, zstar = zstar_t,
                     x.label = symbol$symbol,
                     u.label = c("Open", "High", "Low", "Close"),
                     stateprob.label = bquote("Filtered probability" ~ p(z[t] ~ "|" ~ x[" " ~ 1:t])))

# Forecasting -------------------------------------------------------------
neighbouring_forecast(x = dataset$x.unscaled, oblik_t = oblik_t,
                      h = 1, threshold = 0.05)

