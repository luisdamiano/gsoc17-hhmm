#' Plots a sequence of observed values with intervals with the possibility of
#' including the hidden states.
#'
#' @param y A 3-row matrix with the upper interval, middle and lower interval
#' values of the series.
#' @param z A vector with the sequence of hidden states (optional).
#' @param k The state of interest (optional, but mandatory if z is given).
#' @param ... Other parameters to be passed to base R plot (optional).
#'
#' @return Plots the graph in current device.
#' @export
#' N = 100
#' y.mean <- rnorm(N)
#' y <- rbind(0.7 * y.mean, y.mean, 1.3 * y.mean)
#' y <- exp(y)/(1+exp(y))
#' z <- ifelse(y.mean > 0, 1, 2)
#' plot_intervals(y, z, 1, cex = 0.5)
#' @examples
plot_intervals <- function(y, z = NULL, k = NULL, ...) {
  if (is.matrix(y) && dim(y)[1] != 3)
    stop("The observation matrix must have 3 rows.")

  T.length <- ncol(y)
  x = 1:T.length

  plot(NULL, xlim = c(0, T.length), ylim = c(0, 1), type = 'l', ...)
  polygon(
    c(rev(x), x), c(rev(y[1, ]), y[3, ]),
    col = 'lightgray')

  lines(x, y[1, ], col = 'gray')
  lines(x, y[3, ], col = 'gray')

  lines(x = 1:T.length, y = y[2, ], col = 1)

  abline(h = 0.5, col = 'lightgray', lwd = 0.25)

  if (!is.null(z) && !is.null(k)) {
    if (!is.vector(z) || T.length != length(z))
      stop("The sequence of hidden states must be a vector of length equal to
           the number of rows in the observation matrix.")

    points(x = x, y = as.numeric(z == k),
       pch = 21, bg = z, col = z, cex = 0.7)
  }
}

#' Plots the sequence of inputs and output as well as their cross-sectional
#' relationship.
#'
#' @param x A vector with the sequence of observations.
#' @param u A matrix with the sequence of inputs.
#' @param z A vector with the sequence of hidden states (optional).
#'
#' @return Plots the graph in current device.
#' @export
#'
#' @examples plot_inputoutput(x, u, z)
plot_inputoutput <- function(x, u, z = NULL) {
  if (!is.matrix(u) || nrow(u) != length(x))
    stop("The sequence of inputs must be a matrix whose number of rows must
         equal the length of the output sequence.")

  M <- ncol(u)
  t <- 1:length(x)
  zcol <- if (is.null(z)) 1 else z
  opar <- par()

  layout(rbind(1, rbind(2,
                      matrix(c(3:(M + 2), rep(M + 3, M)),
                             nrow = 2, ncol = M, byrow = TRUE))),
       heights = c(0.3, 0.3, 0.35, 0.05))

  # 1. Observation sequence
  par(mar = c(0, 4.1, 4.1, 2.1))
  plot(x = t, y = x,
       type = 'l', col = 'lightgray',
       ylab = bquote("Output" ~ x), xlab = bquote("Time" ~ t))

  points(x = t, y = x,
         pch = 21, cex = 0.7,
         col = zcol, bg = zcol)

  # 2. Input sequence
  par(mar = c(5.1, 4.1, 0, 2.1))
  matplot(x = t, y = u,
          type = 'l', col = 5 + 1:M,
          lwd = 1, lty = 1,
          ylab = bquote("Input" ~ u), xlab = bquote("Time" ~ t))

  legend(x = "bottomright",
         legend = bquote(.(paste("Input", 1:M))),
         lwd = 3, lty = 1,
         col = 5 + 1:M,
         bty = 'n', horiz = TRUE)

  # 3. Output ~ Input scatterplots
  for (m in 1:M) {
    plot(x = u[, m], y = x,
         pch = 21, cex = 0.7,
         col = zcol, bg = zcol,
         ylab = bquote("Output" ~ x), xlab = bquote("Input" ~ u[.(m)]))
  }
  mtext("Input-Output relationship",
        side = 3, line = -2.5, outer = TRUE)

  # 4. Legend
  par(mai = c(0, 0, 0, 0))
  plot.new()
  legend(x = "center",
         legend = bquote(.(paste("Hidden state", 1:K))),
         lwd = 3, col = sort(unique(zcol)), horiz = TRUE, bty = 'n')
  par(opar)
}

#' Plots the relationship between the inputs and the state probability matrix.
#'
#' @param u A matrix with the sequence of inputs.
#' @param p.mat A matrix with the sequence of state probabilities.
#' @param z A vector with the sequence of hidden states (optional).
#'
#' @return Plots the graph in current device.
#' @export
#'
#' @examples plot_inputprob(u, p.mat, z)
plot_inputprob <- function(u, p.mat, z = NULL) {
  if (!is.matrix(u) || !is.matrix(p.mat) || dim(u)[1] != dim(p.mat)[1])
    stop("The sequence of inputs must be a matrix with same number of rows as
         the probability matrix.")

  M <- ncol(u)
  K <- ncol(p.mat)
  zcol <- if (is.null(z)) 1 else z
  opar <- par()

  layout(matrix(
    c(1:(M*K), rep((M*K) + 1, M)),
    nrow = K + 1, ncol = M, byrow = TRUE),
    heights = c(rep((1 - 0.03)/K, K), 0.03))

  for (m in 1:M) {
    for (k in 1:K) {
      plot(x = u[, m], y = p.mat[, k],
           ylim = c(0, 1),
           pch = 21, cex = 0.7,
           col = zcol, bg = zcol,
           ylab = bquote("Prob of state" ~ .(k) ~ p(z[t] == .(k))),
           xlab = bquote("Input" ~ u[.(m)]))
    }
  }

  par(mai = c(0, 0, 0, 0))
  plot.new()
  legend(x = "center",
         legend = bquote(.(paste("Hidden state", 1:K))),
         lwd = 3, col = sort(unique(zcol)), horiz = TRUE, bty = 'n')
  mtext("Input-State probability relationship",
        side = 3, line = -2, outer = TRUE)
  par(opar)
}

#' Plots the sequence of filtered and smoothed state probabilities as well as
#' their cross-sectional relationship.
#'
#' @param alpha Array of size N, T, K with the sampled filtered probability, where
#' N is the sample size, T is the sequence length and K is the number of
#' hidden states.
#' @param gamma Array of size N, T, K with the sampled smoothed probability.
#' @param interval The width of the sampling intervals to be plotted (optional).
#' @param z A vector with the sequence of hidden states (optional).
#'
#' @return Plots the graph in current device.
#' @export
#'
#' @examples plot_inputprob(alpha, gamma, interval, z)
plot_stateprobability <- function(alpha, gamma, interval = 0.8, z = NULL) {
  if (any(dim(alpha) != dim(gamma)))
    stop("The arrays of filtered and smoothed probabilities must have the same
         dimension.")

  K <- dim(alpha)[3]
  t <- 1:dim(alpha)[2]
  qs <- c((1 - interval)/2, 0.50, 1 - (1 - interval)/2)
  zcol <- if (is.null(z)) 1 else z

  alpha.qs <- apply(alpha, c(2, 3),
                    function(x) {
                      quantile(x, qs) })

  gamma.qs <- apply(gamma, c(2, 3),
                    function(x) {
                      quantile(x, qs) })

  layout(matrix(
    rep(c(1, 1, 2, 2, 3), K) + rep(c(0, 3, 6), each = 5),
    ncol = 5, nrow = K, byrow = TRUE))

  for (k in 1:K) {
    # 1. Filtered probability sequence (forward algoritm)
    plot_intervals(
      x = t, y = alpha.qs[, , k],
      z = z, k = k,
      xlab = bquote(t),
      ylab = bquote(p(z[t] == .(k) ~ "|" ~ x[" " ~ 1:t])),
      main = bquote("Filtered probability for Hidden State" ~ .(k))
    )

    # 2. Smoothed probability sequence (forwards-backwards algorithm)
    plot_intervals(
      x = t, y = gamma.qs[, , k],
      z = z, k = k,
      xlab = bquote(t),
      ylab = bquote(p(z[t] == .(k) ~ "|" ~ x[" " ~ 1:T])),
      main = bquote("Smoothed probability for Hidden State" ~ .(k))
    )

    # 3. Filtered ~ smoothed scatterplot
    plot(
      x = alpha.qs[2, , k], y = gamma.qs[2, , k],
      xlab = bquote(p(z[t] == .(k) ~ "|" ~ x[" " ~ 1:t])),
      ylab = bquote(p(z[t] == .(k) ~ "|" ~ x[" " ~ 1:T])),
      main = bquote("Filtered vs smoothed probability for Hidden State" ~ .(k)),
      type = 'p', pch = 21, col = zcol, bg = zcol, cex = 0.7
    )
    abline(0, 1, col = 'lightgray', lwd = 0.25)
  }
}

#' Plots the sequence corresponding to the jointly most probably state path as
#' computed by the Viterbi decoding algorithm.
#'
#' @param zstar Array of size N, T, K with the sampled hidden states, where
#' N is the sample size, T is the sequence length and K is the number of
#' hidden states.
#' @param z A vector with the sequence of hidden states (optional).
#'
#' @return Plots the graph in current device.
#' @export
#'
#' @examples plot_statepath(zstar, z)
plot_statepath <- function(zstar, z = NULL) {
  K <- length(unique(as.vector(zstar)))
  t <- 1:dim(zstar)[2]
  zcol <- if (is.null(z)) 1 else z

  plot(
    x = t,
    y = apply(zstar, 2, median),
    xlab = bquote(t),
    ylab = bquote(z),
    main = bquote("Sequence of states"),
    type = 'l', col = 'gray')

  legend(x = "top", adj = c(0, -5),
         legend = c('Jointly most probable path (Viterbi)', paste('Actual ', 1:K)),
         pch = c(NA, rep(21, K)),
         lwd = c(2, rep(NA, K)),
         col = c('lightgray', 1:K),
         pt.bg = c('lightgray', 1:K),
         bty = 'n', cex = 0.7,
         horiz = TRUE, xpd = TRUE)

  if (!is.null(z)) {
    if (dim(zstar)[2] != length(z))
      stop("The length of the vector with the sequence of hidden states (z) must
           equal the length of the sequence of the sampled jointly most probable
           hidden states (zstar).")
    points(x = t, y = z,
           pch = 21, bg = zcol, col = zcol, cex = 0.7)
  }
}
