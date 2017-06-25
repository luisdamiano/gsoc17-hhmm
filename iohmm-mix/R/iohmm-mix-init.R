# Chains are initialized close to k-means to speed up convergence
init_fun <- function(stan.data) {
  outer.classif <- kmeans(stan.data$x_t, stan.data$K)
  outer.cluster <- match(outer.classif$cluster, order(outer.classif$centers))

  init.mu <- matrix(0, nrow = stan.data$K, ncol = stan.data$L)
  init.sigma <- matrix(0, nrow = stan.data$K, ncol = stan.data$L)
  for (k in 1:stan.data$K) {
    inner.classif <- kmeans(stan.data$x_t[outer.cluster == k], stan.data$L)
    inner.mu <- as.vector(by(stan.data$x_t[outer.cluster == k], inner.classif$cluster, mean))
    inner.sigma <- as.vector(by(stan.data$x_t[outer.cluster == k], inner.classif$cluster, sd))
    inner.order <- order(inner.mu)

    init.mu[k, ] <- inner.mu[inner.order]
    init.sigma[k, ] <- inner.sigma[inner.order]
  }

  list(
    mu_k = t(init.mu),
    sigma_k = t(init.sigma)
  )
}
