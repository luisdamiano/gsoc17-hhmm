library(moments)
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
           '../data/2007.05.07.G.TO.RData',
           '../data/2007.05.08.G.TO.RData')

# files <- c('../data/2007.05.01.G.TO.RData',
#            '../data/2007.05.02.G.TO.RData')

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
ins <- '2007-05-01 09:30:00/2007-05-07 23:59:59/'
T.length = nrow(zig[ins])
K = 4
L = 9
O = matrix(1:L, 1, L, TRUE)
x = as.vector(zig[ins]$feature)

oos <- '2007-05-08 09:30:00/2007-05-08 23:59:59/'
T.oos = nrow(zig[oos])
x.oos = as.vector(zig[oos]$feature)

use.cache = TRUE

n.iter = 500
n.warmup = 250
n.chains = 1
n.cores = 1
n.thin = 1
n.seed = 9000

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

stan.model = 'tayal2009/stan/hhmm-tayal2009-oos.stan'
stan.cache = 'tayal2009/stan_cache/hhmm-tayal2009-oos.RDS'

stan.data = list(
  T = T.length,
  K = K,
  L = L,
  sign = ifelse(x < L + 1, 1, 2),
  x = ifelse(x < L + 1, x, x - L),
  T_oos = T.oos,
  sign_oos = ifelse(x.oos < L + 1, 1, 2),
  x_oos = ifelse(x.oos < L + 1, x.oos, x.oos - L)
)

if (use.cache & file.exists(stan.cache)) {
  stan.fit <- readRDS(stan.cache)
} else {
  stan.fit <- stan(file = stan.model,
                   model_name = stan.model,
                   data = stan.data, verbose = T,
                   iter = n.iter, warmup = n.warmup,
                   thin = n.thin, chains = n.chains,
                   cores = n.cores, seed = n.seed)

  if (use.cache)
    saveRDS(stan.fit, stan.cache)
}

n.samples = (n.iter - n.warmup) * n.chains

# MCMC Diagnostics --------------------------------------------------------
summary(stan.fit,
        pars = c('p_1k', 'A_ij', 'phi_k'),
        probs = c(0.50))$summary
launch_shinystan(stan.fit)

# Estimates ---------------------------------------------------------------

# Extraction
alpha_tk      <- extract(stan.fit, pars = 'alpha_tk')[[1]]
alpha_tk_oos  <- extract(stan.fit, pars = 'alpha_tk_oos')[[1]]
class.fil     <- apply(apply(alpha_tk, c(2, 3), median), 1, which.max)
class.fil_oos <- apply(apply(alpha_tk_oos, c(2, 3), median), 1, which.max)

# Summary -----------------------------------------------------------------
options(digits = 2)

print("Estimated initial state probabilities")
summary(stan.fit,
        pars = c('p_1k'),
        probs = c(0.10, 0.50, 0.90))$summary[, c(1, 3, 4, 5, 6)]

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


# Functions for analysis --------------------------------------------------
plot_state_returns <- function(x, top, state.labels = c('Bull', 'Bear'), qs = c(0.05, 0.50, 0.95), ...) {
  opar <- par(TRUE)

  par(mfrow = c(1, 2))
  my.xlim <- c(min(x), max(x))
  for (i in c(state.bear, state.bull)) {
    rx <- x[top == i]
    lb <- if (i == state.bear) {state.labels[2]} else {state.labels[1]}

    h <- hist(rx, plot = FALSE)

    plot(h, xlim = my.xlim,
         main = bquote("Returns in " * .(lb) * " top state"),
         xlab = bquote(R),
         col = 'lightgray', border = 'gray')

    if (!is.null(qs)) {
      qx <- quantile(rx, c(0.05, 0.50, 0.95))
      abline(v = qx, lwd = 1, lty = 2, col = 'gray')
      legend(x = "topright", bty = 'n', cex = 0.60, xjust = 1, yjust = 1,
             legend = bquote(.(sprintf("q[%s] ~ \"=\" ~ %0.3f%%", names(qx), 100 * qx))))
    }
  }
  invisible(par(opar))
}

plot_state_seq <- function(tdata, top, ins = '', oos = '', main.lab = NULL, ...) {
  tdata <- merge(tdata, top, join = 'left')
  price.top <- na.locf(na.locf(tdata$topstate, fromLast = TRUE))
  price.x <- index(tdata[ins])
  price.y.ins <- as.vector(tdata[ins, 1])
  price.x.ins <- index(tdata[ins])
  price.top.ins <- price.top[ins]

  x.at <- axTicksByTime(tdata[ins], format.labels = "%H:%M:%S")
  my.ylim <- c(min(price.y.ins), max(price.y.ins))
  my.xlim <- c(min(price.x.ins), max(price.x.ins))

  if (oos != '') {
    full <- rbind(tdata[ins], tdata[oos])
    price.x <- index(full)
    x.at <- axTicksByTime(full, format.labels = "%H:%M:%S")
    my.ylim <- c(min(full[, 1]), max(full[, 1]))
    my.xlim <- c(min(price.x), max(price.x))
  }

  opar <- par(TRUE)

  # Price
  par(mar = c(2.1, 5.0, 4.1, 2.1))
  if (!is.null(main.lab))
    par(mar = c(2.1, 4.1, 6.1, 2.1))

  plot(x = price.x.ins, y = price.y.ins, type = 'l',
       ylab = expression("Price" ~ p[t]), cex.axis = 0.70,
       cex.lab = 0.85, xaxt = 'n', yaxt = 's',
       lwd = 2.0, col = "lightgray",
       ylim = my.ylim,
       xlim = my.xlim,
       ...)

  if (!is.null(main.lab)) {
    title(main.lab, line = 4.5)
  }

  axis(3, at = xy.coords(price.x.ins, price.y.ins)$x[x.at],
       labels = names(x.at), cex.axis = 0.75, las = 2)

  segments(head(price.x.ins, -1), head(price.y.ins, -1),
           tail(price.x.ins, -1), tail(price.y.ins, -1),
           col = tail(ifelse(price.top.ins == state.bull, 'green3',
                             ifelse(price.top.ins == state.bear, 'red',
                                    'blue')), -1),
           lwd = 2)

  if (oos != '') {
    price.y.oos <- as.vector(tdata[oos, 1])
    price.x.oos <- index(tdata[oos])
    price.top.oos <- price.top[oos]

    print(price.y.oos)
    print(price.x.oos)
    print(price.top.oos)

    abline(v = last(price.x.ins))
    text(x = last(price.x.ins), y = par('usr')[4],
         labels = "Out of sample",
         srt = 90, pos = 4)

    segments(head(price.x.oos, -1), head(price.y.oos, -1),
             tail(price.x.oos, -1), tail(price.y.oos, -1),
             col = tail(ifelse(price.top.oos == state.bull, 'green3',
                               ifelse(price.top.oos == state.bear, 'red',
                                      'blue')), -1),
             lwd = 2)
  }

  legend(x = "topright",
         legend = c('Bullish top state', 'Bearish top state'),
         lwd = 2,
         col = c('green3', 'red'),
         bty = 'n', cex = 0.6,
         horiz = TRUE)
  invisible(par(opar))
}

zig$topstate     <- ifelse(c(class.fil, class.fil_oos) == 1 | c(class.fil, class.fil_oos) == 2, state.bull, state.bear)
zig$topstate.chg <- zig$topstate != lag(zig$topstate)
zig$topstate.chg[1] <- TRUE

top     <- zig[zig$topstate.chg == TRUE, ]
top$end <- c(as.numeric(tail(top$start, -1)) - 1, last(zig$end))
top$ret <- (as.numeric(tdata[top$end, 1]) - as.numeric(tdata[top$start, 1])) / as.numeric(tdata[top$start, 1])

ssin  <- '2007-05-07 09:30:00/2007-05-07 16:30:00'
ssout <- '2007-05-08 09:30:00/2007-05-08 10:30:00'
plot_state_seq(tdata, top, ins = ssin, oos = '')
plot_state_seq(tdata, top, ins = ssin, oos = ssout)

# In-sample analysis ------------------------------------------------------
plot_stateprobability(alpha_tk, alpha_tk, 0.8)

zig.in <- zig[ins]
zig.in$topstate     <- ifelse(class.fil == 1 | class.fil == 2, state.bull, state.bear)
zig.in$topstate.chg <- zig.in$topstate != lag(zig.in$topstate)
zig.in$topstate.chg[1] <- TRUE

top.in     <- zig.in[zig.in$topstate.chg == TRUE, ]
top.in$end <- c(as.numeric(tail(top.in$start, -1)) - 1, last(zig.in$end))
top.in$ret <- (as.numeric(tdata[top.in$end, 1]) - as.numeric(tdata[top.in$start, 1])) / as.numeric(tdata[top.in$start, 1])

plot_state_returns(top.in$ret, top.in$topstate)

plot_state_returns(top.in$end - top.in$start, top.in$topstate, qs = NULL)

ss <- '2007-05-01 09:30:00/2007-05-01 16:30:00'

plot_state_seq(tdata[ss], top.in[ss], main = paste("In sample", ss))

# Out of sample analysis --------------------------------------------------
zig.oos <- zig[oos]
zig.oos$topstate     <- ifelse(class.fil_oos == 1 | class.fil_oos == 2, state.bull, state.bear)
zig.oos$topstate.chg <- zig.oos$topstate != lag(zig.oos$topstate)
zig.oos$topstate.chg[1] <- TRUE

top.oos     <- zig.oos[zig.oos$topstate.chg == TRUE, ]
top.oos$end <- c(as.numeric(tail(top.oos$start, -1)) - 1, nrow(tdata))
top.oos$ret <- (as.numeric(tdata[top.oos$end, 1]) - as.numeric(tdata[top.oos$start, 1])) / as.numeric(tdata[top.oos$start, 1])

plot_state_returns(top.oos$ret, top.oos$topstate)
plot_state_returns(top.oos$end - top.oos$start, top.oos$topstate, qs = NULL)

sapply(unique(top.oos$topstate), function(i) {
  ind <- top.oos$topstate == i
  x <- top.oos$ret[ind]
  l <- top.oos$end[ind] - top.oos$start[ind]
  c(mean(x), sd(x), skewness(x), kurtosis(x), quantile(x, c(0.25, 0.50, 0.75)), mean(l), median(l))
})

ss <- '2007-05-08 09:30:00/2007-05-08 10:30:00'
plot_state_seq(tdata[ss], top.oos[ss])

opar <- par(TRUE)
layout(matrix(1:2, ncol = 2, nrow = 1), widths = c(2/3, 1/3))
ssin  <- '2007-05-07 09:30:00/2007-05-07 16:30:00'
ssout <- '2007-05-08 09:30:00/2007-05-08 10:30:00'
tmp <- rbind(tdata[ssin], tdata[ssout])
my.ylim <- c(min(tmp[, 1]), max(tmp[, 1]))
plot_state_seq(tdata[ssin], top.in[ssin], main.lab = paste("In sample", ssin), ylim = my.ylim)
plot_state_seq(tdata[ssout], top.oos[ssout], main.lab = paste("Out of sample", ssout), ylim = my.ylim,
               mar = c(0.0, 0, 0, 0.0))
par(opar)



state.labels <- c('Bull', 'Bear')
par(mfrow = c(1, 2))
for (i in c(state.bear, state.bull)) {
  rx <- top.in$ret[top.in$topstate == i]
  qx <- quantile(rx, c(0.05, 0.50, 0.95))
  lb <- if (i == state.bear) {state.labels[2]} else {state.labels[1]}

  h <- hist(rx, plot = FALSE)

  plot(h, xlim = c(min(top.in$ret), max(top.in$ret)),
       main = bquote("Returns in " * .(lb) * " top state"),
       xlab = bquote(R[.(i)]),
       col = 'lightgray', border = 'gray')

  abline(v = qx, lwd = 1, lty = 2, col = 'lightgray')

  text(x = qx, y = max(h$density)*0.9, srt = 90, pos = 4, cex = 0.6, col = 'black',
       labels = bquote(.(sprintf("q[%s] ~ \"=\" ~ %0.3f%%", names(qx), 100 * qx))))
}

par(mfrow = c(1, 2))
for (i in c(state.bear, state.bull)) {
  zx <- zig.in$feature[zig.in$topstate == i]
  sg <- ifelse(zx < L + 1, 1, 2)
  fe <- ifelse(zx < L + 1, zx, zx - L)
  lb <- if (i == state.bear) {state.labels[2]} else {state.labels[1]}

  barplot(t(sapply(1:2, function(i) {table(factor(fe[sg == i], levels = 1:9))})),
          main = bquote("Zig-zags in " * .(lb) * " top state"),
          xlab = "Feature", ylab = "Frequency",
          beside = TRUE, ylim = c(0, 500 * ((max(table(zig.in$feature)) %/% 500) + 1)),
          col = c('darkgreen', 'red'), border = c('darkgreen', 'red'))

  legend(x = "topright", legend = c('Positive leg', 'Negative leg'),
         bty = 'n', horiz = FALSE,
         fill = c('darkgreen', 'red'), border = c('darkgreen', 'red'))
}

table(x, class.fil)

# in-sample expected trade return of runs and reversals
# Average percentage return per run/reversal switch
# Assign meaning bull R1 > R2

# hist(features) per top node


# Out-of-sample analysis --------------------------------------------------
# Viterbi most likely path
# 1. Average percentage return per run/reversal switch after zigzag is complete (uses initial price of the zigzag)
# 2. Chisq to compare if distributions of two states are similar










# Extraction
alpha_tk     <- extract(stan.fit, pars = 'alpha_tk')[[1]]
alpha_tk_oos <- extract(stan.fit, pars = 'alpha_tk')[[1]]
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

# zig$topstate <- ifelse(class.fil == 1 | class.fil == 4, 1, -1)

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
