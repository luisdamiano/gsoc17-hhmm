library(doParallel)
library(quantmod)
library(rstan)
source('../common/R/math.R')
source('../common/R/plots.R')
source('../iohmm-mix/R/iohmm-mix-init.R')
source('R/data.R')
source('R/forecast.R')

# ------------------------------------------------------------------------
# An automated rolling window forecasting process.
# ------------------------------------------------------------------------
# rstan_options(auto_write = TRUE)
# n.samples = (n.iter - n.warmup) * n.chains

wf_forecast <- function(syms, K, L, hyperparams,
                        n.iter, n.warmup, n.chains, n.cores, n.thin, n.seed,
                        cache.dir = "fore_cache/") {
  cl <- makeCluster(parallel::detectCores())
  registerDoParallel(cl)

  # Data fetching and pre-processing ----------------------------------------
  results <- foreach(
    i = 1:nrow(syms),
    .packages = c("quantmod", "rstan")
  ) %do% {
    cache.symbolfile <- file.path(cache.dir,
                                  paste0(
                                    syms[i, ]$symbol,
                                    syms[i, ]$train.from,
                                    syms[i, ]$train.to,
                                    syms[i, ]$src,
                                    ".RDS"))

    if (!is.null(cache.dir) && file.exists(file.path(cache.symbolfile)))
      return(readRDS(cache.symbolfile))

    prices   <- getSymbols(syms[i, ]$symbol,
                           env  = NULL,
                           from = syms[i, ]$train.from,
                           to   = syms[i, ]$test.to,
                           src  = syms[i, ]$src)
    T.length <- nrow(prices[paste(syms[i, ]$train.from, syms[i, ]$train.to, sep = "/")])
    S <- nrow(prices) - T.length # Number of walk forward forecasts (steps)

    ret <- foreach(
      s = 1:S,
      .packages = c("quantmod", "rstan")
    ) %dopar% {
      cache.stepfile <- file.path(cache.dir,
                                  paste0(
                                    syms[i, ]$symbol,
                                    syms[i, ]$train.from,
                                    syms[i, ]$train.to,
                                    syms[i, ]$src,
                                    "-step", s,
                                    ".RDS"))

      if (!is.null(cache.dir) && file.exists(file.path(cache.stepfile)))
        return(readRDS(cache.stepfile))

      dataset <- make_dataset(prices[1:(T.length + s), ], TRUE)

      stan.model = '../iohmm-mix/stan/iohmm-hmix-lite.stan'
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
  } # symbols

  stopCluster(cl)

  results
}


