library(rstan)
library(shinystan)
source('common/R/plots.R')
source('hhmm/R/hhmm-sim.R')

# Set up! -----------------------------------------------------------------
T.length = 800

n.iter = 500
n.warmup = 250
n.chains = 1
n.cores = 1
n.thin = 1
n.seed = 9000

# 2x2-component Gaussian Mixture ------------------------------------------
r   <- root_node(
  pi_d     = c(0.35, 0.65, 0),
  A_d      = matrix(c(0.0, 0.0, 1.0,
                      0.0, 0.0, 1.0,
                      0.0, 0.0, 1.0),
                    nrow = 3, ncol = 3,
                    byrow = TRUE))

# Component 1
q21 <- internal_node(
  d = 2, i = 1,
  pi_d     = c(0.5, 0.5, 0),
  A_d      = matrix(c(0.0, 0.0, 1.0,
                      0.0, 0.0, 1.0,
                      0.0, 0.0, 1.0),
                    nrow = 3, ncol = 3,
                    byrow = TRUE))

q31 <- production_node(
  d = 3, i = 1,
  obs.mod  = obsmodel_gaussian,
  obs.par  = list(mu = -50, sigma = 1))

q32 <- production_node(
  d = 3, i = 2,
  obs.mod  = obsmodel_gaussian,
  obs.par  = list(mu = -25, sigma = 1))

q3e <- end_node(
  d = 3, i = 3)

# Component 2
q22 <- internal_node(
  d = 2, i = 2,
  pi_d     = c(0.8, 0.2, 0),
  A_d      = matrix(c(0.0, 0.0, 1.0,
                      0.0, 0.0, 1.0,
                      0.0, 0.0, 1.0),
                    nrow = 3, ncol = 3,
                    byrow = TRUE))

q41 <- production_node(
  d = 3, i = 1,
  obs.mod  = obsmodel_gaussian,
  obs.par  = list(mu = 50, sigma = 1))

q42 <- production_node(
  d = 3, i = 2,
  obs.mod  = obsmodel_gaussian,
  obs.par  = list(mu = 25, sigma = 1))

q4e <- end_node(
  d = 3, i = 3)

# Component 1, 2 end
q2e <- end_node(
  d = 2, i = 3)

set_children(r, list(as.ref(q21), as.ref(q22), as.ref(q2e)))

set_parent(q21, as.ref(r))
set_children(q21, list(as.ref(q31), as.ref(q32), as.ref(q3e)))

set_parent(q22, as.ref(r))
set_children(q22, list(as.ref(q41), as.ref(q42), as.ref(q4e)))

set_parent(q2e, as.ref(r))

set_parent(q31, as.ref(q21))
set_parent(q32, as.ref(q21))
set_parent(q3e, as.ref(q21))

set_parent(q41, as.ref(q22))
set_parent(q42, as.ref(q22))
set_parent(q4e, as.ref(q22))

# Check no orphans
for (i in 1:length(ls())) {
  l <- get(ls()[[i]])
  if ("node" %in% class(l)) {
    if (is.null(get_parent(l)))
      print(paste(ls()[[i]], "has no parent"))

    if (is.null(has_children(l)))
      print(paste(ls()[[i]], "has no children"))
  }
}

# Simulate ----------------------------------------------------------------
set.seed(n.seed)
options(expressions = 1e4)
x_t    <- do.call(c, lapply(1:(T.length / 100), function(i) {activate(r, T.length = 100)}))
K      <- sum(sapply(ls(), function(l){"hhmm_pnode" %in% class(get(l))}))
z.true <- kmeans(scale(x_t, TRUE, TRUE), K, nstart = 10)$cluster

# Exploratory data analysis -----------------------------------------------
par(mfrow = c(1, K + 1))
hist(x_t, breaks = "FD",
     main = bquote("Unlabeled data"),
     xlab = bquote(x[t]),
     col = 'lightgray')
for (i in 1:K)
  hist(x_t[z.true == i], breaks = "FD",
       main = bquote("True state" ~ .(i)),
       xlab = bquote(x[t]),
       col  = i)

# Semisupervised model estimation -----------------------------------------
# Soon to come!
# rstan_options(auto_write = TRUE)
# options(mc.cores = parallel::detectCores())
#
# stan.model = 'hhmm/stan/hhmm-2x2semisup.stan'
# stan.data = list(
#   T = T.length,
#   K = K,
#   x_t = x_t
# )
#
# # Chains are initialized close to k-means to speed up convergence
# init_fun <- function(stan.data, k) {
#   list(
#     mu_k = as.vector(by(stan.data$x, k, mean)),
#     sigma_k = as.vector(by(stan.data$x, k, sd))
#   )
# }
#
# stan.fit <- stan(file = stan.model,
#                  model_name = stan.model,
#                  data = stan.data, verbose = T,
#                  iter = n.iter, warmup = n.warmup,
#                  thin = n.thin, chains = n.chains,
#                  cores = n.cores, seed = n.seed,
#                  init = function() {init_fun(stan.data, z.true)})
#
# n.samples = (n.iter - n.warmup) * n.chains
#
# # MCMC Diagnostics --------------------------------------------------------
# summary(stan.fit,
#         pars = c('p_1k', 'A_ij', 'mu_k', 'sigma_k'),
#         probs = c(0.50))$summary
# launch_shinystan(stan.fit)

# Unsupervised model estimation -------------------------------------------
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

stan.model = 'hhmm/stan/hhmix2x2.stan'
stan.data = list(
  T = T.length,
  K = K,
  x_t = x_t
)

# Chains are initialized close to k-means to speed up convergence
init_fun <- function(stan.data, k) {
  list(
    mu_k = as.vector(by(stan.data$x, k, mean)),
    sigma_k = as.vector(by(stan.data$x, k, sd))
  )
}

stan.fit <- stan(file = stan.model,
                 model_name = stan.model,
                 data = stan.data, verbose = T,
                 iter = n.iter, warmup = n.warmup,
                 thin = n.thin, chains = n.chains,
                 cores = n.cores, seed = n.seed,
                 init = function() {init_fun(stan.data, z.true)})

n.samples = (n.iter - n.warmup) * n.chains

# MCMC Diagnostics --------------------------------------------------------
summary(stan.fit,
        pars = c('p_1k', 'A_ij', 'mu_k', 'sigma_k'),
        probs = c(0.50))$summary
launch_shinystan(stan.fit)

# Estimates ---------------------------------------------------------------

# Extraction
mu_k <- extract(stan.fit, pars = 'mu_k')[[1]]
alpha_tk <- extract(stan.fit, pars = 'alpha_tk')[[1]]
gamma_tk <- extract(stan.fit, pars = 'gamma_tk')[[1]]
zstar_t <- extract(stan.fit, pars = 'zstar_t')[[1]]
A_ij <- extract(stan.fit, pars = 'A_ij')[[1]]

# Inference plots
print("Estimated hidden states (hard naive classification using filtered prob)")
print(table(
  estimated = apply(round(apply(alpha_tk, c(2, 3),
                                function(x) {
                                  quantile(x, c(0.50)) })), 1, which.max),
  real = z.true))
plot_stateprobability(alpha_tk, gamma_tk, 0.8, z.true)

# Most likely hidden path (Viterbi decoding) - joint states
round(table(rep(z.true - 1, each = n.samples), zstar_t) / n.samples, 0)
plot_statepath(zstar_t, z.true)

# Marginal probabilities
hier1.true <- c(2, 4)
hier2.true <- c(3, 1)
hier1.star <- c(2, 4)
hier2.star <- c(3, 1)

data.frame(
  desc = c("Hierarchy 1", "Hierarchy 2"),
  true =
    c(sum(z.true %in% hier1.true) / T.length,
      sum(z.true %in% hier2.true) / T.length),
  star =
    c(sum(apply(alpha_tk[, , hier1], c(1, 2), sum) > 0.5) / (n.samples*T.length),
      sum(apply(alpha_tk[, , hier2], c(1, 2), sum) > 0.5) / (n.samples*T.length))
)

for (i in 1:K)
  cat(sprintf("Production node %i \t True %0.4f \t Estimate %0.4f \n",
                i,
                sum(z.true == i) / T.length,
                sum(apply(alpha_tk[, , i], c(1, 2), sum) > 0.5) / (n.samples*T.length)))

## Transition probabilities
zstar_med <- apply(zstar_t, 2, median)
zstar_lag <- tail(zstar_med, -1)

print("Transition matrix (true)")
sapply(1:K, function(j) {
  sapply(1:K, function(i) {
    sum(tail(z.true, -1) == j & head(z.true, -1) == i) / sum(head(z.true, -1) == i)
  })
})

print("Transition matrix (estimated)")
apply(A_ij, c(2, 3), median)
