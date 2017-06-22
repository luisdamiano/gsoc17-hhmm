library(doParallel)
library(quantmod)
library(rstan)
source('common/R/math.R')
source('common/R/plots.R')
source('hassan2005/R/data.R')
source('hassan2005/R/forecast.R')
source('iohmm-mix/R/iohmm-mix-init.R')

# Set up ------------------------------------------------------------------

# Data
symbols <- data.frame(
  symbol    = c("NYSE:DAL", "DAL", "LUV", "RYA.L"),
  name      = c("BA", "Delta Air Lines, Inc.", "Southwest Airlines Co", "Ryanair Holdings Plc"),
  train.from = c("2002-09-17", "2002-12-27", "2002-12-18", "2003-05-06"),
  train.to   = c("2004-09-10", "2004-08-31", "2004-07-23", "2004-12-06"),
  test.from  = c("2004-09-11", "2004-09-01", "2004-07-24", "2004-12-07"),
  test.to    = c("2005-01-20", "2004-11-17", "2004-11-17", "2005-03-17"),
  stringsAsFactors = FALSE)

# Model - IOHMM
K = 4
L = 3

# Model - Hyperparameters
hyperparams <- c(0, 5, 10, 0, 5, 0, 10);

# Markov Chain Monte Carlo
n.iter = 400
n.warmup = 200
n.chains = 1
n.cores = 1
n.thin = 1
n.seed = 9000

# Naive cache? NULL = NO
cache.dir = "hassan2005/stan_cache/"

# ------------------------------------------------------------------------
# An automated rolling window forecasting process.
# ------------------------------------------------------------------------
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
n.samples = (n.iter - n.warmup) * n.chains

cl <- makeCluster(4)
registerDoParallel(cl)

# Data fetching and pre-processing ----------------------------------------
# results <- vector(mode = "list", length = nrow(symbols))
# for (i in 3:nrow(symbols)) {
results <- foreach(
    i = 3:nrow(symbols),
    .packages = c("quantmod", "rstan")
  ) %do% {
    cache.symbolfile <- file.path(cache.dir, paste0(symbols[i, ]$symbol, ".RDS"))
    if (!is.null(cache.dir) && file.exists(file.path(cache.symbolfile)))
      return(readRDS(cache.symbolfile))

    prices   <- getSymbols(symbols[i, ]$symbol,
                           env  = NULL,
                           from = symbols[i, ]$train.from,
                           to   = symbols[i, ]$test.to,
                           src  = "yahoo")
    T.length <- nrow(prices[paste(symbols[i, ]$train.from, symbols[i, ]$train.to, sep = "/")])
    S <- nrow(prices) - T.length # Number of walk forward forecasts (steps)
    S <- 5

    # forecast  <- matrix(NA, nrow = n.samples, ncol = S)
    # for (s in 1:S) {
    ret <- foreach(
      s = 1:S,
      .packages = c("quantmod", "rstan")
    ) %dopar% {
      cache.stepfile <- file.path(cache.dir, paste0(symbols[i, ]$symbol, "-step", s, ".RDS"))
      if (!is.null(cache.dir) && file.exists(file.path(cache.stepfile)))
        return(readRDS(cache.stepfile))

      dataset <- make_dataset(prices[1:(T.length + s), ], TRUE)

      stan.model = 'iohmm-mix/stan/iohmm-hmix-lite.stan'
      stan.data <- list(
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

      oblik_t  <- extract(stan.fit, pars = 'oblik_t')[[1]]

      forecast <- neighbouring_forecast(x = dataset$x.unscaled,
                                           oblik_t = oblik_t,
                                           h = 1, threshold = 0.05)

      ret.step <- list(
        dataset = dataset,
        stan.model = stan.model,
        stan.data = stan.data,
        stan.fit = stan.fit,
        oblik_t = oblik_t,
        forecast = forecast
      )

      if (!is.null(cache.dir))
        return(saveRDS(ret.step, cache.stepfile))

      return(ret.step)
    } # s = 1, ..., S steps

    if (!is.null(cache.dir))
      return(saveRDS(ret, cache.symbolfile))

    ret
  } # sym

stopCluster(cl)

fore <- lapply(results, function(r) {
  sapply(1:length(r), function(i){
    r[[i]]$forecast
  })
})
