library(digest)
library(highfrequency)
library(moments)
library(rstan)
library(shinystan)
library(xts)
library(doParallel)
source('tayal2009/R/constants.R')
source('tayal2009/R/feature-extraction.R')
source('tayal2009/R/trading-rules.R')

# ------------------------------------------------------------------------
# An automated rolling window forecasting process.
# ------------------------------------------------------------------------
# rstan_options(auto_write = TRUE)
# n.samples = (n.iter - n.warmup) * n.chains

wf_trade <- function(files.list, ins.list, oos.list, features.alpha, K, L,
                        n.iter, n.warmup, n.chains, n.cores, n.thin, n.seed,
                        cache.path = "fore_cache/") {
  # cl <- makeCluster(parallel::detectCores())
  cl <- makeCluster(n.cores, outfile = "")
  clusterCall(cl, function() {
    source('tayal2009/R/constants.R')
    source('tayal2009/R/feature-extraction.R')
    source('tayal2009/R/trading-rules.R')
  })
  registerDoParallel(cl)

  # Data fetching and pre-processing ----------------------------------------
  results <- foreach(
    i = 1:length(files.list),
    .packages = c("digest", "rstan", "xts")
  ) %dopar% {
    data.files <- files.list[[i]]
    ins <- ins.list[[i]]
    oos <- oos.list[[i]]

    print(paste(Sys.info()[['nodename']], Sys.getpid(), "Start ", data.files, ins, oos))

    data.env <- new.env()
    series <- do.call(rbind, lapply(data.files, function(f) {
      data.name <- load(file = f, envir = data.env)
      data.var  <- get(data.name, data.env)
      attr(data.var, 'symbol') <- data.name
      indexTZ(data.var) <- 'America/Toronto'
      return(data.var)
    }))
    rm(data.env)

    tdata <- na.omit(series[, 1:2])
    colnames(tdata) <- c("PRICE", "SIZE")

    # Feature extraction ------------------------------------------------------
    zig <- extract_features(tdata, features.alpha)
    zig.ins <- zig[ins]
    zig.oos <- zig[oos]

    # Model estimation --------------------------------------------------------
    T.ins <- nrow(zig.ins)
    x.ins <- as.vector(zig.ins$feature)
    T.oos <- nrow(zig.oos)
    x.oos <- as.vector(zig.oos$feature)

    rstan_options(auto_write = TRUE)
    options(mc.cores = parallel::detectCores())

    stan.model = 'tayal2009/stan/hhmm-tayal2009-lite.stan'

    stan.data = list(
      T = T.ins,
      K = K,
      L = L,
      sign = ifelse(x.ins < L + 1, 1, 2),
      x = ifelse(x.ins < L + 1, x.ins, x.ins - L),
      T_oos = T.oos,
      sign_oos = ifelse(x.oos < L + 1, 1, 2),
      x_oos = ifelse(x.oos < L + 1, x.oos, x.oos - L))

    # A naive implementation for a stan cache
    cache.objects  <- list(series, features.alpha,
                           stan.model, readLines(stan.model),
                           stan.data, n.iter, n.warmup,
                           n.thin, n.chains, n.cores, n.seed)

    cache.digest   <- digest(cache.objects)
    cache.filename <- file.path(cache.path, paste0(cache.digest, ".RDS"))

    if (!is.null(cache.path) & file.exists(cache.filename)) {
      stan.fit <- readRDS(cache.filename)
    } else {
      stan.fit <- stan(file = stan.model,
                       model_name = stan.model,
                       data = stan.data, verbose = T,
                       iter = n.iter, warmup = n.warmup,
                       thin = n.thin, chains = n.chains,
                       cores = n.cores, seed = n.seed)

      if (!is.null(cache.path))
        saveRDS(stan.fit, cache.filename)
    }

    rm(cache.objects, cache.digest, cache.filename)

    # Estimates ---------------------------------------------------------------
    # Extraction
    alpha_tk.ins  <- extract(stan.fit, pars = 'alpha_tk')[[1]]
    alpha_tk.oos  <- extract(stan.fit, pars = 'alpha_tk_oos')[[1]]

    # Hard classification
    state.filtered.ins <- apply(apply(alpha_tk.ins, c(2, 3), median), 1, which.max)
    state.filtered.oos <- apply(apply(alpha_tk.oos, c(2, 3), median), 1, which.max)
    state.filtered     <- c(state.filtered.ins, state.filtered.oos)

    topstate.labels <- c("Bear", "Bull")
    topstate.pairs  <- list(c(1, 2), c(3, 4))  # bottom-node to top-node map
    topstate.index  <- c(state.bear, state.bull)

    # assign zig-zag to top states
    zig$topstate <- state.filtered
    for (i in 1:length(topstate.pairs))
      zig$topstate[state.filtered %in% topstate.pairs[[i]]] <- topstate.index[i]

    zig$topstate.chg    <- zig$topstate != lag(zig$topstate)
    zig$topstate.chg[1] <- TRUE

    # build top sequence
    top     <- zig[zig$topstate.chg == TRUE, ]
    top$end <- c(as.numeric(tail(top$start, -1)) - 1, last(zig$end))
    top$len <- top$end - top$start
    top$ret <- (as.numeric(tdata[top$end, 1]) - as.numeric(tdata[top$start, 1])) / as.numeric(tdata[top$start, 1])

    # label top nodes
    if (mean(top$ret[top$topstate == state.bear]) > mean(top$ret[top$topstate == state.bull])) {
      top$topstate <- ifelse(top$topstate == state.bear, state.bull, state.bear)
      zig$topstate <- ifelse(zig$topstate == state.bear, state.bull, state.bear)
      print(sprintf("I identified top-nodes as bears and bulls.
                Result: Bear = %0.4f%% vs Bull = %0.4f%%",
                    mean(top$ret[top$topstate == state.bear]),
                    mean(top$ret[top$topstate == state.bull])))
    }

    tdata     <- xts_expand(tdata, zig[, c('feature', 'topstate')])
    tdata.oos <- tdata[oos]

    # A naive implementation for a stan cache
    cache.objects  <- list(tdata.oos)
    cache.digest   <- digest(cache.objects)
    cache.filename <- file.path(cache.path, paste0(cache.digest, ".RDS"))

    if (!is.null(cache.path) & file.exists(cache.filename)) {
      trade.list <- readRDS(cache.filename)
    } else {
      trade.list <- list(buyandhold   = buyandhold(tdata.oos),
                         strategy0lag = topstate_trading(tdata.oos, 0),
                         strategy1lag = topstate_trading(tdata.oos, 1),
                         strategy2lag = topstate_trading(tdata.oos, 2))

      if (!is.null(cache.path))
        saveRDS(trade.list, cache.filename)
    }

    print(paste(Sys.info()[['nodename']], Sys.getpid(), "End   ", data.files, ins, oos))

    gc()
    return(list(cache.filename, trade.list))
  }

  stopCluster(cl)

  results
}
