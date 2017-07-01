# Data adquisition and management functions
library(quantmod)
invisible(Sys.setlocale("LC_MESSAGES", "C"))  # Google vs date format fix
invisible(Sys.setlocale("LC_TIME", "C"))      # https://stackoverflow.com/a/20855453/2860744

get_prices <- function(symbol, from, to, src, ...) {
  if (
    src == "google"
    && as.Date(from, origin = '1970-01-01') < as.Date("2003-12-28", origin = '1970-01-01')
    && as.Date(to, origin = '1970-01-01') > as.Date("2003-12-30", origin = '1970-01-01')) {
    return(rbind(
      getSymbols(symbol, env = NULL,
                 from = from,
                 to = as.Date("2003-12-27", origin = '1970-01-01'),
                 src = src, ...),
      getSymbols(symbol, env = NULL,
                 from = as.Date("2003-12-31", origin = '1970-01-01'),
                 to = to,
                 src = src, ...)
    ))
  }

  getSymbols(symbol, env = NULL, from = from, to = to, src = src, ...)
}

make_dataset <- function(prices, scale = TRUE) {
  T.length = nrow(prices)

  x = as.vector(Cl(prices))[2:T.length]
  u = as.matrix(prices)[1:(T.length - 1), 1:4]

  dataset <- list(
    x = as.vector(x),
    u = as.matrix(u),
    x.unscaled = as.vector(x),
    u.unscaled = as.matrix(u),
    x.center = 0,
    x.scale  = 1,
    u.center = 0,
    u.scale  = 1
  )

  if (scale) {
    x <- scale(x, TRUE, TRUE)
    u <- scale(u, TRUE, TRUE)

    dataset$x <- as.vector(x)
    dataset$u <- as.matrix(u)
    dataset$x.center <- attr(x, "scaled:center")
    dataset$x.scale  <- attr(x, "scaled:scale")
    dataset$u.center <- attr(u, "scaled:center")
    dataset$u.scale  <- attr(u, "scaled:scale")
  }

  dataset
}
