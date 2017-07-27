library(rstan)
library(shinystan)
source('common/R/plots.R')
source('hhmm/R/hhmm-sim.R')

# Set up! -----------------------------------------------------------------
T.length = 200

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
l1K    <- length(get_children(r)) - 1
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
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

stan.model = 'hhmm/stan/hhmm-semisup.stan'
stan.data = list(
  T = T.length,
  K = K,
  x_t = x_t,
  l1K = l1K,
  l1z_t = ifelse(x_t >= 0, 1, 2),
  l1index = matrix(c(1, 2, 3, 4),
                   nrow = l1K, ncol = 2,
                   byrow = TRUE))

# Chains are initialized close to k-means to speed up convergence
init_fun <- function(stan.data) {
  ret <- matrix(0, nrow = stan.data$K, ncol = 2)
  for (l in 1:stan.data$l1K) {
    cl <- kmeans(scale(stan.data$x_t[stan.data$l1z_t == l], TRUE, TRUE),
                 stan.data$l1index[l, 2] - stan.data$l1index[l, 1] + 1,
                 nstart = 10)$cluster

    ret[stan.data$l1index[l, 1]:stan.data$l1index[l, 2], 1] <-
      as.vector(by(stan.data$x_t[stan.data$l1z_t == l], cl, mean))
    ret[stan.data$l1index[l, 1]:stan.data$l1index[l, 2], 2] <-
      as.vector(by(stan.data$x_t[stan.data$l1z_t == l], cl, sd))
  }

  list(
    mu_k = as.vector(ret[, 1]),
    sigma_k = as.vector(ret[, 2])
  )
}

stan.fit <- stan(file = stan.model,
                 model_name = stan.model,
                 data = stan.data, verbose = T,
                 iter = n.iter, warmup = n.warmup,
                 thin = n.thin, chains = n.chains,
                 # init = function() {init_fun(stan.data)}, # optional!
                 cores = n.cores, seed = n.seed)

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

# Relabelling (ugly hack edition) -----------------------------------------
z.relab <- rep(0, T.length)

hard <- sapply(1:T.length, function(t, med) {
  which.max(med[t, ])
}, med = apply(alpha_tk, c(2, 3),
               function(x) {
                 quantile(x, c(0.50)) }))

tab <- table(hard = hard, original = z.true)
for (k in 1:(K - 1)) {
  ptab <- prop.table(tab, 1)
  ind.swap <- which(ptab == max(ptab), arr.ind = T)[1, ]

  a <- as.numeric(dimnames(tab)$original[ind.swap[2]])
  b <- as.numeric(dimnames(tab)$hard[ind.swap[1]])
  z.relab[z.true == a] <- b

  if (k == K - 1) {
    ind.swap[1] <- if (ind.swap[1] == 1) 2 else 1
    ind.swap[2] <- if (ind.swap[2] == 1) 2 else 1

    a <- as.numeric(dimnames(tab)$original[ind.swap[2]])
    b <- as.numeric(dimnames(tab)$hard[ind.swap[1]])
    z.relab[z.true == a] <- b
  }

  tab <- tab[-ind.swap[1], -ind.swap[2]]
}

print("Label re-imputation (relabelling due to switching labels)")
table(new = z.relab, original = z.true)

# Inference plots
print("Estimated hidden states (hard naive classification using filtered prob)")
print(table(
  estimated = apply(round(apply(alpha_tk, c(2, 3),
                                function(x) {
                                  quantile(x, c(0.50)) })), 1, which.max),
  real = z.relab))
plot_stateprobability(alpha_tk, gamma_tk, 0.8, z.relab)

# Most likely hidden path (Viterbi decoding) - joint states
round(table(rep(z.relab - 1, each = n.samples), zstar_t) / n.samples, 0)
plot_statepath(zstar_t, z.relab)

# Observation model parameters
hier1.true <- c(1, 2)
hier2.true <- c(3, 4)
hier1.star <- c(1, 2)
hier2.star <- c(3, 4)

data.frame(
  true = as.vector(by(x_t, z.relab, mean)),
  star = colMeans(mu_k)
)

# Marginal probabilities
data.frame(
  desc = c("Hierarchy 1", "Hierarchy 2"),
  true =
    c(sum(z.relab %in% hier1.true) / T.length,
      sum(z.relab %in% hier2.true) / T.length),
  star =
    c(sum(apply(alpha_tk[, , hier1.star], c(1, 2), sum) > 0.5) / (n.samples*T.length),
      sum(apply(alpha_tk[, , hier2.star], c(1, 2), sum) > 0.5) / (n.samples*T.length))
)

for (i in 1:K)
  cat(sprintf("Production node %i \t True %0.4f \t Estimate %0.4f \n",
                i,
                sum(z.relab == i) / T.length,
                sum(apply(alpha_tk[, , i], c(1, 2), sum) > 0.5) / (n.samples*T.length)))

## Transition probabilities
zstar_med <- apply(zstar_t, 2, median)
zstar_lag <- tail(zstar_med, -1)

print("Transition matrix (true)")
sapply(1:K, function(j) {
  sapply(1:K, function(i) {
    sum(tail(z.relab, -1) == j & head(z.relab, -1) == i) / sum(head(z.relab, -1) == i)
  })
})

print("Transition matrix (estimated)")
apply(A_ij, c(2, 3), median)

print("Hard classification")
apply(apply(alpha_tk, c(1, 2), which.max), 2, function(x) {as.numeric(names(table(x))[1])})

# Unsupervised model estimation -------------------------------------------
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

stan.model = 'hhmm/stan/hhmm-unsup.stan'
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
