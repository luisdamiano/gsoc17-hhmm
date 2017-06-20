library(quantmod)
library(rstan)
library(shinystan)
source('common/R/math.R')
source('common/R/plots.R')

# Set up ------------------------------------------------------------------

# Data
symbols <- data.frame(
  symbol    = c("NYSE:DAL", "NYSE:DAL", "NYSE:LUV", "LON:RYA"),
  name      = c("BA", "Delta Air Lines, Inc.", "Southwest Airlines Co", "Ryanair Holdings Plc"),
  train.from = c("17-09-2002", "27-12-2002", "18-12-2002", "06-05-2003"),
  train.to   = c("10-09-2004", "31-08-2004", "23-07-2004", "06-12-2004"),
  test.from  = c("11-09-2004", "01-09-2004", "24-07-2004", "07-12-2004"),
  test.to    = c("20-01-2005", "17-11-2004", "17-11-2004", "17-03-2005"),
  stringsAsFactors = FALSE)

# Model - IOHMM
K = 2
L = 1

# Model - Hyperparameters
hyperparams <- c(0, 5, 10, 0, 5, 0, 10);

# Markov Chain Monte Carlo
n.iter = 400
n.warmup = 200
n.chains = 1
n.cores = 1
n.thin = 1
n.seed = 9000

# Data fetching and pre-processing ----------------------------------------
invisible(Sys.setlocale("LC_MESSAGES", "C"))  # Google vs date format fix
invisible(Sys.setlocale("LC_TIME", "C"))      # https://stackoverflow.com/a/20855453/2860744

get_prices <- function(symbol, train.from, train.to, test.from, test.to, ...) {
  prices <- getSymbols(symbol, env = NULL, from = train.from, to = test.to, ...)
  list(
    train = prices[paste(train.from, train.to, sep = "/")],
    test  = prices[paste(test.from, test.to, sep = "/")]
  )
}

prices <- get_prices(symbols[2, ]$symbol, symbols[2, ]$train.from, symbols[2, ]$train.to, symbols[2, ]$test.from, symbols[2, ]$test.to, src = "google")

make_dataset <- function(prices) {
  T.length = nrow(prices)

  list(
    u = as.matrix(prices)[1:(T.length - 1), 1:4],
    x = as.vector(Cl(prices))[2:T.length]
  )
}

dataset <- make_dataset(prices$train)

# Data exploration --------------------------------------------------------
plot_inputoutput(x = dataset$x, u = dataset$u,
                 x.label = symbols[2, ]$symbol,
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
  x_t = dataset$x,
  hyperparams = as.array(hyperparams)
)

# Chains are initialized close to k-means to speed up convergence
init_fun <- function() {
  classif <- kmeans(stan.data$x_t, stan.data$K)
  init.mu <- matrix(0, nrow = stan.data$K, ncol = stan.data$L)
  init.sigma <- matrix(0, nrow = stan.data$K, ncol = stan.data$L)

  for (k in 1:K) {
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
                 cores = n.cores, seed = n.seed, init = init_fun)

n.samples = (n.iter - n.warmup) * n.chains

# MCMC Diagnostics --------------------------------------------------------
options(digits = 2)
summary(stan.fit,
        pars = c('p_1k', 'w_km', 'lambda_kl', 'mu_kl', 's_kl'),
        probs = c(0.50))$summary
launch_shinystan(stan.fit)

# Extraction --------------------------------------------------------------
alpha_tk <- extract(stan.fit, pars = 'alpha_tk')[[1]]
gamma_tk <- extract(stan.fit, pars = 'gamma_tk')[[1]]
zstar_t <- extract(stan.fit, pars = 'zstar_t')[[1]]
hatx_t <- extract(stan.fit, pars = 'hatx_t')[[1]]

# Estimation summary ------------------------------------------------------
print("Estimated initial state probabilities")
summary(stan.fit,
        pars = c('p_1k'),
        probs = c(0.10, 0.50, 0.90))$summary[, c(1, 3, 4, 5, 6)]

# print("Estimated probabilities in the transition matrix")
# head(summary(stan.fit,
#              pars = c('logA_ij'),
#              probs = c(0.10, 0.50, 0.90))$summary[, c(1, 3, 4, 5, 6)])

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
plot_outputfit(dataset$x, hatx_t, z = dataset$zrelab, TRUE)

