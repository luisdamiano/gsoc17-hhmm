plot_intervals <- function(x, y, z = NULL, k = NULL, ...) {
  T.length = length(x)

  plot(NULL, xlim = c(0, T.length), ylim = c(0, 1), type = 'l', ...)
  polygon(
    c(rev(x), x), c(rev(y[1, ]), y[3, ]),
    col = 'lightgray')

  lines(x, y[1, ], col = 'gray')
  lines(x, y[3, ], col = 'gray')

  lines(x = 1:T.length, y = y[2, ], col = 1)

  if(!is.null(z) & !is.null(k)) {
    cols <- ifelse(dataset$z == k, 'green', 'red')
    points(x = x, y = as.numeric(dataset$z == k),
           pch = 21, bg = cols, col = cols, cex = 0.7)
  }
}
