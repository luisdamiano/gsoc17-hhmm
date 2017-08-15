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

files <- c('../data/2007.05.01.G.TO.RData',
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

stan.model = 'tayal2009/stan/hhmm-tayal2009.stan'
stan.data = list(
  T = T.length,
  K = K,
  L = L,
  sign = ifelse(x < L + 1, 1, 2),
  x = ifelse(x < L + 1, x, x - L)
)

stan.fit <- stan(file = stan.model,
                 model_name = stan.model,
                 data = stan.data, verbose = T,
                 iter = n.iter, warmup = n.warmup,
                 thin = n.thin, chains = n.chains,
                 cores = n.cores, seed = n.seed)#w, init = init_fun)

n.samples = (n.iter - n.warmup) * n.chains

# MCMC Diagnostics --------------------------------------------------------
summary(stan.fit,
        pars = c('p_1k', 'A_ij', 'phi_k'),
        probs = c(0.50))$summary
launch_shinystan(stan.fit)

# Estimates ---------------------------------------------------------------

# Extraction
alpha_tk <- extract(stan.fit, pars = 'alpha_tk')[[1]]
gamma_tk <- extract(stan.fit, pars = 'gamma_tk')[[1]]

class.fil <- apply(apply(alpha_tk, c(2, 3), median), 1, which.max)
class.smo <- apply(apply(gamma_tk, c(2, 3), median), 1, which.max)

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

table(observed = stan.data$sign,
      filtered = class.fil)
# there's some inconsistency in here as node can't have both pos and neg zigzags

print("Some applied analysis")
Aij_med <- matrix(summary(stan.fit,
                          pars = c('A_ij'),
                          probs = c(0.10, 0.50, 0.90))$summary[, c(1, 3, 4, 5, 6)][, 4],
                  K, K, TRUE)

phik_med <- matrix(summary(stan.fit,
                          pars = c('phi_k'),
                          probs = c(0.10, 0.50, 0.90))$summary[, c(1, 3, 4, 5, 6)][, 4],
                  K, L, TRUE)

table(stan.data$x[stan.data$sign == 1])
opar <- par(no.readonly = TRUE)
par(mfrow = c(2, 2))
barplot()


par(opar)
table(stan.data$sign, stan.data$x, class.fil)

opar <- par(no.readonly = TRUE)
par(mfrow = c(2, 2))
for (i in 1:4) {
  barplot(phik_med[i, ])
  # hist(stan.data$x[class.fil == i], breaks = "FD")
}
par(opar)

print("Probability that bullish (Top Node I: States 1 and 2) remains bullish")
Aij_med



# Inference plots
print("Estimated hidden states (hard naive classification using filtered prob)")
print(table(
  estimated = apply(apply(alpha_tk, c(2, 3), median), 1, which.max)))
plot_stateprobability(alpha_tk, gamma_tk, 0.8)

# Most likely hidden path (Viterbi decoding) - joint states
zstar <- extract(stan.fit, pars = 'zstar_t')[[1]]
round(table(zstar) / n.samples, 0)

plot_statepath(zstar)



























price <- G.TO['T09:35:00/T09:36:30', 1]
price   <- na.omit(price)
price.x <- difftime(index(price), "2007-05-02 09:30:00",
                    tz = 'America/Toronto', units = "min")
price.y <- as.vector(price)

plot(price.x, price.y, type = 'p',
     xlab = "Time in mins from 2007-05-02 09:30:00",
     ylab = "Transaction price",
     pch = 21, bg = "lightgray", col = "black", cex = 0.8)

plot(G.TO['2007-05-02 09:30:00/2007-05-02 10:00:00'])

G.TO['T09:30:00/T09:40:00']


