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
wf_trade <- function(task.list,
                     features.alpha, K, L,
                     n.iter, n.warmup, n.chains, n.cores, n.thin, n.seed,
                     cache.path) {

  wd <- getwd()
  cl <- makeCluster(n.cores, outfile = file.path(cache.path, "log.txt"))
  clusterCall(cl, function() {
    source('tayal2009/R/constants.R')
    source('tayal2009/R/feature-extraction.R')
    source('tayal2009/R/trading-rules.R')
  })
  registerDoParallel(cl)

  # Data fetching and pre-processing ----------------------------------------
  results <- foreach(
    task = task.list,
    .packages = c("digest", "rstan", "xts")
  # ) %do% {
  ) %dopar% {
    setwd(wd)
    data.files <- task[[1]]
    ins <- task[[2]]
    oos <- task[[3]]

    print_debug <- function(x) {
      # print(paste(Sys.time(), Sys.info()[['nodename']], Sys.getpid(), x, ins, oos, paste(data.files, collapse = " || ")))
    }

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
    ind <- zig[ins, which.i = TRUE]
    zig.ins <- zig[ind]
    zig.oos <- zig[(last(ind) + 1):nrow(zig)]

    # Model estimation --------------------------------------------------------
    T.ins <- nrow(zig.ins)
    x.ins <- as.vector(zig.ins$feature)
    T.oos <- nrow(zig.oos)
    x.oos <- as.vector(zig.oos$feature)

    print_debug(sprintf("Found data: %d tdata, %d zigzags", nrow(tdata), nrow(zig)))

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
      print_debug(paste("Model was cached!", cache.filename))
      stan.fit <- readRDS(cache.filename)
    } else {
      print_debug(paste("Model not cached!", cache.filename))
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
    }

    tdata     <- xts_expand(tdata, zig[, c('feature', 'topstate')])
    tdata.oos <- tdata[oos]

    # A naive implementation for a stan cache
    cache.objects  <- list(tdata.oos)
    cache.digest   <- digest(cache.objects)
    cache.filename <- file.path(cache.path, paste0(cache.digest, ".RDS"))

    if (!is.null(cache.path) & file.exists(cache.filename)) {
      print_debug(paste("Trades were cached!", cache.filename))
      trade.list <- readRDS(cache.filename)
    } else {
      print_debug(paste("Trades not cached!", cache.filename))
      trade.list <- list(buyandhold   = buyandhold(tdata.oos),
                         strategy0lag = topstate_trading(tdata.oos, 0),
                         strategy1lag = topstate_trading(tdata.oos, 1),
                         strategy2lag = topstate_trading(tdata.oos, 2),
                         strategy3lag = topstate_trading(tdata.oos, 3),
                         strategy4lag = topstate_trading(tdata.oos, 4),
                         strategy5lag = topstate_trading(tdata.oos, 5))

      if (!is.null(cache.path))
        saveRDS(trade.list, cache.filename)
    }

    print_debug(paste("End", cache.filename))

    gc()
    # return(list(task, ins, oos, tdata, top, zig, trade.list, stan.fit))
    return(list(symbol = attr(tdata, 'symbol'),
                task = task, ins = ins, oos = oos,
                tdata = tdata, top = top, zig = zig,
                trades = trade.list))
  }

  stopCluster(cl)

  results
}
