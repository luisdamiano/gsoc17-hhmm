xts_expand <- function(long, short, ...) {
  expand <- merge(long, short, join = 'left', ...)
  expand <- na.locf(na.locf(expand, fromLast = TRUE))
  expand
}

# tdata has to be a xts with two columns price and size
extract_features <- function(tdata, alpha = 0.25) {
  # 1. Initial checks
  tdata <- highfrequency:::.check_data(tdata)
  highfrequency:::tdatacheck(tdata)
  if (is.null(tdata$PRICE) | is.null(tdata$SIZE))
    stop("tdata must include PRICE and SIZE.")

  # 2. Data extraction
  price <- tdata$PRICE
  size  <- tdata$SIZE

  # 3. Zig-zag creation
  lprice     <- lag(price)
  direction  <- ifelse(price > lprice, direction.up,
                       ifelse(price < lprice, direction.dn,
                              direction.lt))
  direction[1, ] <- direction.lt

  ldirection    <- lag(direction)
  direction.chg <- direction != direction.lt & direction != ldirection
  # direction.chg <- direction != direction.lt & ldirection != direction.lt & direction != ldirection

  zigzag <- price[which(direction.chg) - 1, ]
  colnames(zigzag) <- 'price'

  zigzag$start  <- c(1, head((1:nrow(price))[direction.chg], -1))

  zigzag$end <- lag(zigzag$start, -1) - 1
  zigzag$end[nrow(zigzag)] <- nrow(price)

  pos.mat  <- as.matrix(zigzag[, c("start", "end")])
  size.mat <- as.vector(size)
  size.ind <- index(size)
  zigzag$size.av <- sapply(1:nrow(zigzag), function(i) {
    pos_start <- pos.mat[i, 1]
    pos_end   <- pos.mat[i, 2]

    sum(size.mat[pos_start:pos_end]) /
      (as.numeric(difftime(size.ind[pos_end], size.ind[pos_start]), units = "secs") + 1)
  })

  # 4. Feature I: local extrema type
  zigzag$f0    <- ifelse(lag(zigzag$price) < zigzag$price, extrema.max, extrema.min)
  zigzag$f0[1] <- if (zigzag$f0[2] == extrema.max) extrema.min else extrema.max

  # 5. Feature II: trend direction
  price.mat <- as.matrix(zigzag[, "price"])
  zigzag$f1 <- sapply(1:nrow(zigzag), function(n) {
    if (n <= 4)
      return(trend.lt)

    e_n <- price.mat[(n - 4):n]

    if (e_n[1] < e_n[3] & e_n[3] < e_n[5]
        & e_n[2] < e_n[4])
      return(trend.up)

    if (e_n[1] > e_n[3] & e_n[3] > e_n[5]
        & e_n[2] > e_n[4])
      return(trend.dn)

    return(trend.lt)
  })

  # 6. Feature III: volume strength
  zigzag$size.ratio1    <- zigzag$size.av / lag(zigzag$size.av, 1)
  zigzag$size.ratio2    <- zigzag$size.av / lag(zigzag$size.av, 2)
  zigzag$size.ratio3    <- lag(zigzag$size.av, 1) / lag(zigzag$size.av, 2)

  discretize_sizeratio <- function(ratio, alpha) {
    ifelse(ratio - 1 > alpha, 1, ifelse(1 - ratio > alpha, -1, 0))
  }

  zigzag$size_strength1 <- discretize_sizeratio(zigzag$size.ratio1, alpha)
  zigzag$size_strength2 <- discretize_sizeratio(zigzag$size.ratio2, alpha)
  zigzag$size_strength3 <- discretize_sizeratio(zigzag$size.ratio3, alpha)

  zigzag$f2 <- rep(volume.lt, nrow(zigzag))
  zigzag$f2[zigzag$size_strength1 ==  1 & zigzag$size_strength2 > -1 & zigzag$size_strength3 <  1] <- volume.up
  zigzag$f2[zigzag$size_strength1 == -1 & zigzag$size_strength2 <  1 & zigzag$size_strength3 > -1] <- volume.dn

  zigzag$f2[1:2] <- volume.lt

  # 7. Legs
  legs <- matrix(c( 1,  1,  1,  1, # Up legs
                    1, -1,  1,  2,
                    1,  1,  0,  3,
                    1,  0,  1,  4,
                    1,  0,  0,  5,
                    1,  0, -1,  6,
                    1, -1,  0,  7,
                    1,  1, -1,  8,
                    1, -1, -1,  9,
                   -1,  1, -1, 10, # Down legs
                   -1, -1, -1, 11,
                   -1,  1,  0, 12,
                   -1,  0, -1, 13,
                   -1,  0,  0, 14,
                   -1,  0,  1, 15,
                   -1, -1,  0, 16,
                   -1,  1,  1, 17,
                   -1, -1,  1, 18),
                 nrow = 18, ncol = 4, byrow = TRUE)

  # This function is the bottleneck, find a smarter implementation
  find_leg <- function(vec, legs) {
    i = 1
    while (i <= nrow(legs)) {
      if (all(vec == legs[i, 1:3]))
        return(legs[i, 4])
      i <- i + 1
    }
    stop("Not a valid leg, watch out!")
  }

  zigzag$feature <- rollapply(zigzag, 1, function(z) {
    find_leg(z[, c("f0", "f1", "f2")], legs)
  }, by.column = FALSE)

  # 8. Trend
  zigzag$trend <- rep(trend.up, nrow(zigzag))
  zigzag$trend[zigzag$feature %in% c(6:9, 15:18)] <- trend.dn
  zigzag$trend[zigzag$feature %in% c(5, 14)] <- trend.lt

  zigzag
}
