source('tayal2009/R/wf-trade.R')

# Set up! -----------------------------------------------------------------
# Alpha threshold in the change of volumen setting (0.25 = Tayal 2009)
features.alpha <- 0.25

# HHMM structure: K production/emission states, L possible outcomes
K = 4
L = 9

# Strategy setting
window.ins <- 5
window.oos <- 1
window.all <- window.ins + window.oos

# Parallel walking forward
n.cores = 3

# MCMC settings
n.iter = 500
n.warmup = 250
n.chains = 1
n.thin = 1
n.seed = 9000

# Cache settings
# data.path: folder where data is stored
# Expected filename: stock_name/Y.M.D.stock_name.RData
# Example: G.TO/2007.05.01.G.TO.RData
data.path  <- '../data/'
cache.path = 'tayal2009/fore_cache'

# Data loading ------------------------------------------------------------
# a helper function
filename_to_timestamp <- function(from, to,
                                  open.time = " 09:30:00/",
                                  close.time = " 16:30:00") {
  paste0(gsub(pattern = "[.]", replacement = "-", substr(from, 1, 10)),
         open.time,
         gsub(pattern = "[.]", replacement = "-", substr(to, 1, 10)),
         close.time)
}

# builds the list of files and timestamps
task.list <- do.call(c, lapply(dir(data.path), function(d) { # d = one stock
              stock.files <- dir(file.path(data.path, d))

              lapply(1:(length(stock.files) - window.all + 1), function(i) {
                list(file.path(data.path, d, stock.files[i:(i + window.all - 1)]),
                     filename_to_timestamp(stock.files[i],
                                           stock.files[i + window.ins - 1]),
                     filename_to_timestamp(stock.files[i + window.ins],
                                           stock.files[i + window.all - 1]))
              })}))

# Ready, steady... GO! ----------------------------------------------------
# runs the strategy walking forward
tr <- wf_trade(task.list, features.alpha, K, L,
         n.iter, n.warmup, n.chains, n.cores, n.thin, n.seed, cache.path)

apply(sapply(tr, function(t) {
  c(bh = prod(1 + t[[4]][[1]]) - 1,
    l0 = prod(1 + t[[4]][[2]]$ret) - 1,
    l1 = prod(1 + t[[4]][[3]]$ret) - 1,
    l2 = prod(1 + t[[4]][[4]]$ret) - 1)
}), 1, function(x) {prod(1 + x) - 1})



lapply(tr, function(t) {
  list(
    bh = as.vector(t[[4]][[1]]),
    l0 = as.vector(t[[4]][[2]]$ret),
    l1 = as.vector(t[[4]][[3]]$ret),
    l2 = as.vector(t[[4]][[4]]$ret))
})

