library(highfrequency)

# Won't be needed when we get the real data
# data("sample_tdataraw")
# cldata <- tradesCleanup(tdataraw = sample_tdataraw, exchanges = list("N"))$tdata
# tdata <- cldata$PRICE
# plot(tdata)
#
#

data("sample_tdata")
tdata <- sample_tdata
# tdata <- tdata[, 3:4]
# tdata[, 1] <- as.numeric(tdata[, 3])
# tdata[, 4] <- as.numeric(tdata[, 4])

# tdata has to be a xts with two columns price and size
extract_zigzag <- function(tdata) {
  # Check
  tdata <- highfrequency:::.check_data(tdata)
  highfrequency:::tdatacheck(tdata)
  if (is.null(tdata$PRICE) | is.null(tdata$SIZE))
    stop("tdata must include PRICE and SIZE.")

  # Prices
  price <- tdata$PRICE

  lprice <- lag(price)
  trend  <- ifelse(price > lprice, 1, ifelse(price < lprice, -1, 0))
  trend[1,] <- 0
  ltrend <- lag(trend)
  trend.chg <- ifelse(trend != 0 & ltrend != 0 & trend != lag(trend), TRUE, FALSE)
  trend.chg[1, ] <- FALSE

  # zigzag <- price[trend.chg == 1, ]
  zigzag <- price[which(trend.chg) - 1, ]
  colnames(zigzag) <- 'extrema'

  zigzag$start  <- (1:nrow(price))[trend.chg]

  zigzag$end    <- lag(zigzag$start, -1) - 1
  zigzag$end[nrow(zigzag)] <- nrow(price)

  zigzag$type <- ifelse(lag(zigzag$extrema) < zigzag$extrema, "min", "max")
  zigzag$type[1] <- if(zigzag$type[2] == "min") "max" else "min"

  # zigzag$type <- rollapply(zigzag, 1, function(z) {
  #   print(z$start)
  #   # return(trend[z$start] == 1)
  #   # if (trend[z$start] == 1) { return("min") }
  #   #   return("max")
  #   # sum(as.numeric(size[z$start:z$end])) /
  #   #   (as.numeric(difftime(index(size[z$end]), index(size[z$start]), units = "secs") + 1))
  # }, by.column = FALSE)

  # Size
  size <- tdata$SIZE

  zigzag$av_size <- rollapply(zigzag, 1, function(z) {
    sum(as.numeric(size[z$start:z$end])) /
      (as.numeric(difftime(index(size[z$end]), index(size[z$start]), units = "secs") + 1))
  }, by.column = FALSE)

  zigzag
}

plot(price)
points(x = index(price[1:100]), y = price[1:100]$PRICE, pch = 21, bg = "black", col = NULL, cex = 0.5)

for(i in 1:nrow(zigzag)) {
  points(x = index(zigzag[i]), y = zigzag[i]$extrema, pch = 21, bg = "red", col = NULL)
}

lines(x = index(zigzag), y = as.numeric(zigzag$extrema), col = 1:2, lty = 1:2)
# Segments with colors
