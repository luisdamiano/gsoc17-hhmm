library(xts)
library(rstan)
library(shinystan)
source('common/R/plots.R')
source('tayal2009/R/feature-extraction.R')

# Data loading ------------------------------------------------------------
# Data is kept in a private folder
files <- c('../data/2007.05.01.G.TO.RData',
           '../data/2007.05.02.G.TO.RData',
           '../data/2007.05.03.G.TO.RData',
           '../data/2007.05.04.G.TO.RData',
           '../data/2007.05.07.G.TO.RData')

# files <- c('../data/2007.05.01.G.TO.RData',
           '../data/2007.05.02.G.TO.RData')

series <- do.call(rbind, lapply(files, function(f) {
  load(f)
  indexTZ(G.TO) <- 'America/Toronto'
  G.TO
}))

tdata <- na.omit(series[, 1:2])
colnames(tdata) <- c("PRICE", "SIZE")

# Feature extraction ------------------------------------------------------
system.time(zig <- extract_features(tdata, 0.25))

plot_features(tdata, zig, which.features = 'extrema')
plot_features(tdata, zig, which.features = 'trend')
plot_features(tdata, zig, which.features = 'all')

# Model estimation --------------------------------------------------------
ss <- '2007-05-02 09:30:00/2007-05-02 12:00:00/'
ss <- ''
T.length = nrow(zig[ss])
K = 4
L = 9
O = matrix(1:L, 1, L, TRUE)
x = as.vector(zig[ss]$feature)

n.iter = 500
n.warmup = 250
n.chains = 1
n.cores = 1
n.thin = 1
n.seed = 9000

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

stan.model = 'tayal2009/stan/hhmm-tayal2009-lite.stan'
stan.data = list(
  T = T.length,
  K = K,
  L = L,
  sign = ifelse(x < L + 1, 1, 2),
  x = ifelse(x < L + 1, x, x - L))

stan.fit <- stan(file = stan.model,
                 model_name = stan.model,
                 data = stan.data, verbose = T,
                 iter = n.iter, warmup = n.warmup,
                 thin = n.thin, chains = n.chains,
                 cores = n.cores, seed = n.seed)

n.samples = (n.iter - n.warmup) * n.chains

# MCMC Diagnostics --------------------------------------------------------
summary(stan.fit,
        pars = c('p_1k', 'A_ij', 'phi_k'),
        probs = c(0.50))$summary
launch_shinystan(stan.fit)

# Estimates ---------------------------------------------------------------

# Extraction
alpha_tk <- extract(stan.fit, pars = 'alpha_tk')[[1]]
# gamma_tk <- extract(stan.fit, pars = 'gamma_tk')[[1]]
# zstar_t  <- extract(stan.fit, pars = 'zstar_t')[[1]]

class.fil <- apply(apply(alpha_tk, c(2, 3), median), 1, which.max)
# class.smo <- apply(apply(gamma_tk, c(2, 3), median), 1, which.max)
# class.vit <- apply(zstar_t, 2, function(z) { which.max(tabulate(z)) })

# Summary -----------------------------------------------------------------
options(digits = 2)

print("Estimated initial state probabilities")
summary(stan.fit,
        pars = c('p_1k'),
        probs = c(0.10, 0.50, 0.90))$summary[, c(1, 3, 4, 5, 6)]

print("Estimated probabilities in the transition matrix")
summary(stan.fit,
        pars = c('A_ij'),
        probs = c(0.10, 0.50, 0.90))$summary[, c(1, 3, 4, 5, 6)]

matrix(summary(stan.fit,
               pars = c('A_ij'),
               probs = c(0.10, 0.50, 0.90))$summary[, c(1, 3, 4, 5, 6)][, 4],
       K, K, TRUE)

print("Estimated event probabilities in each state")
summary(stan.fit,
        pars = c('phi_k'),
        probs = c(0.10, 0.50, 0.90))$summary[, c(1, 3, 4, 5, 6)]

matrix(summary(stan.fit,
               pars = c('phi_k'),
               probs = c(0.10, 0.50, 0.90))$summary[, c(1, 3, 4, 5, 6)][, 4],
       K, L, TRUE)

# In-sample analysis ------------------------------------------------------
plot_stateprobability(alpha_tk, alpha_tk, 0.8)

zig$topstate <- ifelse(class.fil == 1 | class.fil == 4, 1, -1)

topstate.chg <- zig$topstate != lag(zig$topstate)
topstate.chg[1] <- TRUE

top <- zig[topstate.chg == TRUE, ]
top$end <- c(as.numeric(tail(top$start, -1)) - 1, 1)
top$ret <- (as.numeric(tdata[top$end, 1]) - as.numeric(tdata[top$start, 1])) / as.numeric(tdata[top$start, 1])

head(top)

zigzag$start  <- c(1, head((1:nrow(price))[topstate.chg], -1))

zigzag$end <- lag(zigzag$start, -1) - 1
zigzag$end[nrow(zigzag)] <- nrow(price)

top <-

# in-sample expected trade return of runs and reversals
# Average percentage return per run/reversal switch
# Assign meaning bull R1 > R2

# hist(features) per top node


# Out-of-sample analysis --------------------------------------------------
# Viterbi most likely path
# 1. Average percentage return per run/reversal switch after zigzag is complete (uses initial price of the zigzag)
# 2. Chisq to compare if distributions of two states are similar
























# Top Node  I:  Run/Bull        States 1 (negative zigzag) and 2 (positive zigzag)
# Top Node II:  Reversal/Bear   States 3 (positive zigzag) and 4 (negative zigzag)
print("Some consistency checks")
alpha_med <- apply(alpha_tk, c(2, 3), median)

rbind(
  observed  = table(ifelse(x < L + 1, "Positive zz leg", "Negative zz leg"))    / T.length,
  filtered  = c(sum(table(class.fil)[c(1, 3)]), sum(table(class.fil)[c(2, 4)])) / T.length)

table(observed = zig[ss]$feature,
      filtered = class.fil)

table(observed = zig[ss]$feature,
      filtered = class.smo)

table(observed = zig[ss]$feature,
      viterbi  = class.vit)

table(observed = zig[ss]$feature,
      viterbi  = class.vit)
# there's some inconsistency in here as node can't have both pos and neg zigzags

print("Some applied analysis")
apply(alpha_tk, c(2, 3), median)[stan.data$sign == 1, ]
summary(rowSums(apply(alpha_tk, c(2, 3), median)[stan.data$sign == 1, 2:3]))

# Inference plots
print("Estimated hidden states (hard naive classification using filtered prob)")
print(table(
  estimated = apply(apply(alpha_tk, c(2, 3), median), 1, which.max)))
plot_stateprobability(alpha_tk, gamma_tk, 0.8)

# Most likely hidden path (Viterbi decoding) - joint states
round(table(zstar) / n.samples, 0)
plot_statepath(zstar)
