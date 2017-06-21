library(quantmod)
library(rstan)
library(shinystan)
source('common/R/data.R')
source('common/R/forecast.R')
source('common/R/math.R')
source('common/R/plots.R')

# Set up ------------------------------------------------------------------

# Data
symbols <- data.frame(
  symbol    = c("NYSE:DAL", "NYSE:DAL", "NYSE:LUV", "LON:RYA"),
  name      = c("BA", "Delta Air Lines, Inc.", "Southwest Airlines Co", "Ryanair Holdings Plc"),
  train.from = c("2002-09-17", "2002-12-27", "2002-12-18", "2003-05-06"),
  train.to   = c("2004-09-10", "2004-08-31", "2004-07-23", "2004-12-06"),
  test.from  = c("2004-09-11", "2004-09-01", "2004-07-24", "2004-12-07"),
  test.to    = c("2005-01-20", "2004-11-17", "2004-11-17", "2005-03-17"),
  stringsAsFactors = FALSE)

# Model - IOHMM
K = 4
L = 3
S = 1 # Number of walk forward forecasts (steps)

# Model - Hyperparameters
hyperparams <- c(0, 5, 10, 0, 5, 0, 10);

# Markov Chain Monte Carlo
n.iter = 400
n.warmup = 200
n.chains = 1
n.cores = 1
n.thin = 1
n.seed = 9000

# ------------------------------------------------------------------------
# First, we analyse step-by-step our model for a given stock and date. We
# validate that the results of our sampler are reasonable. We then move
# into an automated rolling window forecasting process.
# ------------------------------------------------------------------------

# Data fetching and pre-processing ----------------------------------------
i = 3
prices   <- getSymbols('LUV', NULL,
                     from = symbols[i, ]$train.from,
                     to   = symbols[i, ]$test.to,
                     src  = "yahoo")

dataset  <- make_dataset(prices[paste(symbols[i, ]$train.from, symbols[i, ]$train.to, sep = "/")])
T.length <- length(dataset$x)

# Data exploration --------------------------------------------------------
plot_inputoutput(x = dataset$x, u = dataset$u,
                 x.label = symbols[i, ]$symbol,
                 u.label = c("Open", "High", "Low", "Close"))

# Model estimation --------------------------------------------------------
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

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

# Chains are initialized close to k-means to speed up convergence
init_fun <- function(stan.data) {
  classif <- kmeans(stan.data$x_t, stan.data$K)
  init.mu <- matrix(0, nrow = stan.data$K, ncol = stan.data$L)
  init.sigma <- matrix(0, nrow = stan.data$K, ncol = stan.data$L)

  for (k in 1:stan.data$K) {
    inner.classif <- kmeans(stan.data$x_t[classif$cluster == k], stan.data$L)
    inner.mu <- as.vector(by(stan.data$x_t[classif$cluster == k], inner.classif$cluster, mean))
    inner.sigma <- as.vector(by(stan.data$x_t[classif$cluster == k], inner.classif$cluster, sd))
    inner.order <- order(inner.mu)

    init.mu[k, ] <- inner.mu[inner.order]
    init.sigma[k, ] <- inner.sigma[inner.order]
  }

  list(
    mu_k = t(init.mu),
    sigma_k = t(init.sigma)
  )
}

stan.fit <- stan(file = stan.model,
                 data = stan.data, verbose = T,
                 iter = n.iter, warmup = n.warmup,
                 thin = n.thin, chains = n.chains,
                 cores = n.cores, seed = n.seed, init = init_fun(stan.data))

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

# Jointly most likely state path (Viterbi decoding)
plot_statepath(zstar_t)

# Fitted output
plot_outputfit(dataset$x, hatx_t, TRUE)

plot_inputoutputprob(x = dataset$x, u = dataset$u,
                     stateprob = alpha_tk, zstar = zstar_t,
                     x.label = symbols[2, ]$symbol,
                     u.label = c("Open", "High", "Low", "Close"),
                     stateprob.label = bquote("Filtered probability" ~ p(z[t] ~ "|" ~ x[" " ~ 1:t])))

# Forecasting -------------------------------------------------------------
neighbouring_forecast(x = dataset$x.unscaled, oblik_t = oblik_t,
                      h = 1, threshold = 0.05)

# Walk forward forecasting ------------------------------------------------
forecast <- vector(numeric, S)
for (s in 1:S) {
  forecast.dataset  <- make_dataset(prices[1:(T.length + s), ], TRUE)

  forecast.stan.model = 'iohmm-mix/stan/iohmm-hmix-lite.stan'
  forecast.stan.data = list(
    K = K,
    L = L,
    M = ncol(forecast.dataset$u),
    T = length(forecast.dataset$x),
    u_tm = as.array(forecast.dataset$u),
    x_t = as.vector(forecast.dataset$x),
    hyperparams = as.array(hyperparams)
  )

  forecast.stan.fit <- stan(file = forecast.stan.model,
                            data = forecast.stan.data, verbose = T,
                            iter = n.iter, warmup = n.warmup,
                            thin = n.thin, chains = n.chains,
                            cores = n.cores, seed = n.seed,
                            init = function() {init_fun(forecast.stan.data)})

  forecast[s] <- neighbouring_forecast(x = forecast.dataset$x.unscaled,
                                       oblik_t = oblik_t,
                                       h = 1, threshold = 0.05)
}


