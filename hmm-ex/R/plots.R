plot_intervals <- function(x, y, z = NULL, ...) {
  T.length = length(x)

  plot(NULL, xlim = c(0, T.length), ylim = c(0, 1), type = 'l', ...)
  polygon(
    c(rev(x), x), c(rev(y[1, ]), y[3, ]), 
    col = 'lightgray')
  
  lines(x, y[1, ], col = 'gray')
  lines(x, y[3, ], col = 'gray')
  
  lines(x = 1:T.length, y = y[2, ], col = 1)
  
  if(!is.null(z)) { 
    cols <- ifelse(dataset$z == 1, 'green', 'red')
    points(x = x, y = z,
           pch = 21, bg = cols, col = cols, cex = 0.7)
  }
}
