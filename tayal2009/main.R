library(digest)
library(highfrequency)
library(moments)
library(rstan)
library(shinystan)
library(xts)
source('common/R/plots.R')
source('tayal2009/R/constants.R')
source('tayal2009/R/feature-extraction.R')
source('tayal2009/R/trading-rules.R')
source('tayal2009/R/state-plots.R')

# Set up! -----------------------------------------------------------------
# Data kept in a private folder as we do no have redistribution rights
data.files <- c('tayal2009/data/G.TO/2007.05.01.G.TO.RData',
                'tayal2009/data/G.TO/2007.05.02.G.TO.RData',
                'tayal2009/data/G.TO/2007.05.03.G.TO.RData',
                'tayal2009/data/G.TO/2007.05.04.G.TO.RData',
                'tayal2009/data/G.TO/2007.05.07.G.TO.RData',
                'tayal2009/data/G.TO/2007.05.08.G.TO.RData')

# Timespans to separate training and test sets
ins <- '2007-05-01 09:30:00/2007-05-07 16:30:00'
oos <- '2007-05-08 09:30:00/2007-05-08 16:30:00'

# Alpha threshold in the change of volumen setting (0.25 = Tayal 2009)
features.alpha <- 0.25

# HHMM structure: K production/emission states, L possible outcomes
K = 4
L = 9

# MCMC settings
n.iter = 500
n.warmup = 250
n.chains = 1
n.cores = 1
n.thin = 1
n.seed = 9000

cache.path = 'tayal2009/fore_cache'
              # A naive implementation to cache Stan fit objects
              # Sampler won't run twice under exactly same setup
              # NULL = No cache!

# Data loading ------------------------------------------------------------
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
system.time(zig <- extract_features(tdata, features.alpha))
zig.ins <- zig[ins]
zig.oos <- zig[oos]

plot_features(tdata, zig, which.features = 'extrema')
plot_features(tdata, zig, which.features = 'trend')
plot_features(tdata, zig, which.features = 'all')

# Model estimation --------------------------------------------------------
# O = matrix(1:L, 1, L, TRUE)
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

n.samples = (n.iter - n.warmup) * n.chains

# MCMC Diagnostics --------------------------------------------------------
summary(stan.fit,
        pars = c('p_1k', 'A_ij', 'phi_k'),
        probs = c(0.50))$summary
launch_shinystan(stan.fit)

# Estimates ---------------------------------------------------------------
# Extraction
alpha_tk.ins  <- extract(stan.fit, pars = 'alpha_tk')[[1]]
alpha_tk.oos  <- extract(stan.fit, pars = 'alpha_tk_oos')[[1]]
zstar_t.oos   <- extract(stan.fit, pars = 'zstar_t')[[1]]

# Hard classification
state.filtered.ins <- apply(apply(alpha_tk.ins, c(2, 3), median), 1, which.max)
state.filtered.oos <- apply(apply(alpha_tk.oos, c(2, 3), median), 1, which.max)
state.filtered     <- c(state.filtered.ins, state.filtered.oos)
state.viterbi.oos  <- apply(zstar_t.oos, 2, median)

# Summary -----------------------------------------------------------------
options(digits = 2)

print("Estimated initial state probabilities")
matrix(summary(stan.fit,
               pars = c('p_1k'),
               probs = c(0.10, 0.50, 0.90))$summary[, c(1, 3, 4, 5, 6)][, 4],
       1, K, TRUE)

print("Estimated probabilities in the transition matrix")
matrix(summary(stan.fit,
               pars = c('A_ij'),
               probs = c(0.10, 0.50, 0.90))$summary[, c(1, 3, 4, 5, 6)][, 4],
       K, K, TRUE)

print("Estimated event probabilities in each state")
matrix(summary(stan.fit,
               pars = c('phi_k'),
               probs = c(0.10, 0.50, 0.90))$summary[, c(1, 3, 4, 5, 6)][, 4],
       K, L, TRUE)

# Top state classification ------------------------------------------------
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
tdata.ins <- tdata[ins]
tdata.oos <- tdata[oos]
zig.ins   <- zig[ins]
zig.oos   <- zig[oos]
top.ins   <- top[ins]
top.oos   <- top[oos]

# In-sample analysis ------------------------------------------------------
print(topstate_summary(top.ins))

plot_topstate_hist(top.ins$ret, top.ins$topstate,
                main.lab = "Returns", x.lab = "Percentage return")

plot_topstate_hist(top.ins$len, top.ins$topstate,
                main.lab = "Length", x.lab = "Number of ticks",
                qs = NULL)

plot_topstate_seq(tdata.ins, top.ins$topstate,
               main.lab = sprintf("%s In-sample [%s]", attr(tdata.ins, 'symbol'), ins))

plot_topstate_seqv(tdata.ins, zig.ins,
                   main.lab = sprintf("%s In-sample [%s]", attr(tdata.oos, 'symbol'), ins))

plot_topstate_features(top.ins$feature, top.ins$topstate, L)

# Out of sample analysis --------------------------------------------------
print(topstate_summary(top.oos))

plot_topstate_hist(top.oos$ret, top.oos$topstate,
                   main.lab = "Returns", x.lab = "Percentage return")

plot_topstate_hist(top.oos$len, top.oos$topstate,
                   main.lab = "Length", x.lab = "Number of ticks",
                   qs = NULL)

plot_topstate_seq(tdata.oos, top.oos$topstate,
                  main.lab = sprintf("%s Out-of-sample [%s]", attr(tdata.oos, 'symbol'), oos))

plot_topstate_seqv(tdata.oos, zig.oos,
                   main.lab = sprintf("%s Out-of-sample [%s]", attr(tdata.oos, 'symbol'), oos))

plot_topstate_features(top.oos$feature, top.oos$topstate, L)

# Trading strategy --------------------------------------------------------
trades.oos <- topstate_trading(tdata.oos, 1)
# no-lag strategy, strategy, b&h
# equity line prod(1+trade$ret)

plot_topstate_trading(tdata.oos, zig.oos, trades.oos)


mean(trades$perchg) < mean(trades$ret)

# Out-of-sample analysis --------------------------------------------------
# Viterbi most likely path
# 1. Average percentage return per run/reversal switch after zigzag is complete (uses initial price of the zigzag)
# 2. Chisq to compare if distributions of two states are similar
