interval_ribbons <- function(x, y, ...) {
  polygon(
    c(rev(x), x), c(rev(y[, 1]), y[, 2]), 
    ...)
  
  lines(x, y[, 1], col = 'gray')
  lines(x, y[, 2], col = 'gray')
}