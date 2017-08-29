topstate_summary <- function(top, topstate.label = c("Bear", "Bull")) {
  mystats <- function(top, ind) {
    x <- 100 * top$ret[ind]
    l <- top$end[ind] - top$start[ind]
    c(ret_mean     = mean(x),
      ret_stdev    = sd(x),
      ret_skewness = skewness(x),
      ret_kurtosis = kurtosis(x),
      ret_iqrange  = quantile(x, c(0.25, 0.50, 0.75)),
      len_mean     = mean(l),
      len_median   = median(l))
  }

  mat <- sapply(sort(unique(top$topstate)), function(i) {
    ind <- top$topstate == i
    mystats(top, ind)
  })
  mat <- cbind(mat, mystats(top, rep(TRUE, nrow(top))))
  colnames(mat) <- c(topstate.label, "Unconditional")
  return(mat)
}

plot_features <- function(tdata, zigzag = extract_features(tdata),
                          which.features = c('actual', 'extrema', 'trend', 'all')) {
  # 1. Initial checks
  tdata <- highfrequency:::.check_data(tdata)
  highfrequency:::tdatacheck(tdata)
  if (is.null(tdata$PRICE) | is.null(tdata$SIZE))
    stop("tdata must include PRICE and SIZE.")

  # 2. Data extraction
  price <- tdata$PRICE
  size  <- tdata$SIZE

  # 3. Plotting preamble
  # Sorry for the ugly global!
  legend.list <- list(x = "bottomright", bty = 'n',
                      cex = par()$cex * 0.75,
                      horiz = TRUE) #, cex = 0.6)

  list_add <- function(current, ...) {
    dots <- list(...)
    for (i in 1:length(dots)) {
      args.name <- names(dots)[i]
      args.val  <- dots[[i]]
      current[[args.name]] <- c(current[[args.name]], args.val)
    }
    current
  }

  opar <- par(no.readonly = TRUE)
  layout(matrix(1:2, nrow = 2, ncol = 1), heights = c(0.75, 0.25))

  # 4. Plot I: Prices, extrema and features
  price.x <- index(tdata)
  price.y <- as.vector(price)
  x.at <- axTicksByTime(tdata, format.labels = "%H:%M:%S")

  # Price
  par(mar = c(0.0, 5.0, 4.1, 2.1))
  plot(x = price.x, y = price.y, type = 'l',
       ylab = expression("Price" ~ p[t]),
       # cex.axis = 0.70, cex.lab = 0.85,
       xaxt = 'n', yaxt = 's',
       lwd = 2.0, col = "lightgray")

  axis(3, at = xy.coords(price.x, price.y)$x[x.at],
       labels = names(x.at), las = 2) #cex.axis = 0.75, )

  legend.list <- list_add(legend.list,
                          legend = 'Price',
                          pch = NA,
                          lwd = 2,
                          col = 'lightgray',
                          pt.bg = NA)

  if ('actual' %in% which.features) {
    points(x = price.x, y = price.y,
           pch = 21, cex = 0.65,
           col = NULL, bg = 'black')

    legend.list <- list_add(legend.list,
                            legend = 'Price',
                            pch = 21,
                            lwd = NA,
                            col = 'black',
                            pt.bg = 'black')
  }

  # Features
  zigzag.x <- index(zigzag)

  # Extrema
  if ('extrema' %in% which.features) {
    points(x = zigzag.x, y = zigzag$price,
           pch = 21, cex = 0.65,
           col = NULL, bg = ifelse(zigzag$f0 == extrema.min, 'red', 'green3'))

    legend.list <- list_add(legend.list,
                            legend = c('Local min', 'Local max'),
                            pch = c(21, 21),
                            lwd = c(NA, NA),
                            col = c('red', 'green3'),
                            pt.bg = c('red', 'green3'))
  }

  if ('trend' %in% which.features) {
    trend.chg <- zigzag$trend != lag(zigzag$trend)
    trend.chg[1] <- TRUE

    trend.x   <- zigzag.x[trend.chg]
    trend.y   <- zigzag[trend.chg]

    segments(head(trend.x, -1), head(trend.y$price, -1),
             tail(trend.x, -1), tail(trend.y$price, -1),
             col = tail(ifelse(trend.y$trend == trend.up, 'green3',
                               ifelse(trend.y$trend == trend.dn, 'red',
                                      'blue')), -1),
             lwd = 2)

    points(x = trend.x, y = trend.y$price,
           pch = 21, cex = 0.65,
           col = NULL, bg = ifelse(trend.y$trend == trend.up, 'green3',
                                   ifelse(trend.y$trend == trend.dn, 'red',
                                          'blue')))

    legend.list <- list_add(legend.list,
                            legend = c('Bullish', 'Lateral', 'Bearish'),
                            pch = c(NA, NA, NA),
                            lwd = c( 2,  2,  2),
                            col = c('green3', 'blue', 'red'),
                            pt.bg = c(NA, NA, NA))
  }

  if ('all' %in% which.features) {
    lexp <- cbind(expand.grid(1:9, c("U", "D")),
                  c(rep("Bull", 4), "Local vol", rep("Bear", 4)))
    labs <- sprintf("%s%s (%s)", lexp[, 2], lexp[, 1], lexp[, 3])

    all.palette <- colorRampPalette(c('lightgreen', 'darkred'))(18)
    all.palette <- all.palette[c(1:5, 15:18, 6:14)]

    segments(head(zigzag.x, -1), head(zigzag$price, -1),
             tail(zigzag.x, -1), tail(zigzag$price, -1),
             col = tail(all.palette[zigzag$feature], -1),
             lwd = 2)

    legend.list <- list_add(legend.list,
                            legend = labs,
                            # legend = c(paste("Bull", 1:9), paste("Bear", 1:9)),
                            pch = rep(NA, 18),
                            lwd = rep( 2, 18),
                            col = all.palette,
                            pt.bg = rep(NA, 18),
                            # cex = rep(NA, 0.75),
                            ncol = 3)
    legend.list$horiz <- FALSE
  }

  # Legend
  do.call(legend, legend.list)

  # Plot 2 Volume
  volume.y <- as.vector(size)
  volume.x <- na.locf(cbind(size, zigzag$f2), fromLast = TRUE)
  volume.palette <- ifelse(volume.x$f2 == volume.up, 'green3',
                           ifelse(volume.x$f2 == volume.dn, 'red',
                                  'blue'))

  par(mar = c(3.1, 5.0, 0.0, 2.1))
  par(mgp = c(3.0, 1.0, 0.0))
  barplot(height = volume.y,
          ylab = expression("Volume" ~ v[t]),
          ylim = c(0, quantile(volume.y, 0.99)),
          # cex.axis = 0.70, cex.lab = 0.85,
          xaxt = 'n', yaxt = 's',
          border = volume.palette,
          col = volume.palette)
  box()

  title(xlab = expression("Time" ~ t), mgp = c(0.5, 0, 0))

  legend(x = "topleft",
         legend = c('Volume strengthens', 'Volumen weakens', 'Indeterminant'),
         lwd = c(2,  2,  2),
         col = c('green3', 'red', 'blue'),
         bty = 'n', # cex = 0.6,
         cex = par()$cex * 0.5,
         y.intersp	= 0.0, # x.intersp = 0.2,
         # text.width = 20.0,
         horiz = TRUE)
  par(opar)
}

plot_topstate_hist <- function(x, top,
                            qs = c(0.05, 0.50, 0.95),
                            topstate.label = c('Bear', 'Bull'),
                            main.lab = "Histogram", x.lab = "x",
                            ...) {
  topstate <- sort(unique(top))
  n <- length(topstate)
  h <- list()

  for (i in 1:n) {
    h[[i]] <- hist(x[top == topstate[i]], plot = FALSE, ...)
  }

  my.xlim <- c(
    min(unlist(lapply(h, function(j) j$breaks))),
    max(unlist(lapply(h, function(j) j$breaks))))
  my.ylim <- c(0, max(unlist(lapply(h, function(j) j$counts))))

  opar <- par(TRUE)
  par(mfrow = 1:n)
  for (i in 1:n) {
    plot(h[[i]], xlim = my.xlim, ylim = my.ylim,
         main = bquote(.(main.lab) ~ "(top state " * .(topstate.label[i]) * ")"),
         xlab = bquote(.(x.lab)),
         col = 'lightgray', border = 'gray', ...)

    if (!is.null(qs)) {
      qx <- quantile(x[top == topstate[i]], c(0.05, 0.50, 0.95))
      abline(v = qx, lwd = 1, lty = 2, col = 'gray')
      legend(x = "topright",
             bty = 'n', cex = par()$cex * 0.50,
             xjust = 1, yjust = 1,
             legend = bquote(.(parse(text = sprintf("q[\"%s\"] ~ \"= %0.6f\"", names(qx), qx)))))
    }
  }
  par(opar)

  invisible()
}

plot_topstate_seq <- function(tdata, top, main.lab = NULL, ...) {
  price.x   <- index(tdata)
  price.y   <- as.vector(tdata[, 1])
  price.top <- top
  x.at      <- axTicksByTime(tdata, format.labels = "%H:%M:%S")

  # Price
  opar <- par(TRUE)
  par(mar = c(2.1, 5.0, 4.1, 2.1))
  plot(x = price.x, y = price.y, type = 'l',
       ylab = expression("Price" ~ p[t]),
       # cex.axis = 0.70, cex.lab = 0.85,
       xaxt = 'n', yaxt = 's',
       lwd = 2.0, col = "lightgray", ...)

  if (!is.null(main.lab)) {
    text(x = par('usr')[1] - strheight("f"), y = par('usr')[4] - strheight("f"),
         labels = main.lab,
         pos = 4, font = 2) #cex = 0.6,
  }

  axis(3, at = xy.coords(price.x, price.y)$x[x.at],
       labels = names(x.at), las = 2) # cex.axis = 0.75,

  segments(head(price.x, -1), head(price.y, -1),
           tail(price.x, -1), tail(price.y, -1),
           col = tail(ifelse(price.top == state.bull, 'green3',
                             ifelse(price.top == state.bear, 'red',
                                    'blue')), -1),
           lwd = 2)

  legend(x = "topright",
         legend = c('Bullish top state', 'Bearish top state'),
         lwd = 2,
         col = c('green3', 'red'),
         bty = 'n', # cex = 0.6,
         horiz = TRUE)

  par(opar)

  invisible()
}

plot_topstate_seqv <- function(tdata, zigzag, main.lab = NULL) {
  # 2. Data extraction
  price <- tdata$PRICE
  size  <- tdata$SIZE

  opar <- par(no.readonly = TRUE)
  layout(matrix(1:2, nrow = 2, ncol = 1), heights = c(0.75, 0.25))

  # 4. Plot I: Prices, extrema and features
  price.x <- index(tdata)
  price.y <- as.vector(price)
  price.top <- tdata$topstate
  x.at <- axTicksByTime(tdata, format.labels = "%H:%M:%S")

  # Price
  par(mar = c(0.0, 5.0, 4.1, 2.1))
  plot(x = price.x, y = price.y, type = 'l',
       ylab = expression("Price" ~ p[t]),
       # cex.axis = 0.70, cex.lab = 0.85,
       xaxt = 'n', yaxt = 's',
       lwd = 2.0, col = "lightgray")

  axis(3, at = xy.coords(price.x, price.y)$x[x.at],
       labels = names(x.at), las = 2) # cex.axis = 0.75,

  all.palette <- tail(ifelse(price.top == state.bull, 'green3',
                             ifelse(price.top == state.bear, 'red',
                                    'blue')), -1)

  segments(head(price.x, -1), head(price.y, -1),
           tail(price.x, -1), tail(price.y, -1),
           col = tail(all.palette, -1),
           lwd = 2)

  if (!is.null(main.lab)) {
    text(x = par('usr')[1] - strheight("f"), y = par('usr')[4] - strheight("f"),
         labels = main.lab, cex = par()$cex * 0.5,
         pos = 4, font = 2) # cex = 0.6,
  }

  legend(x = "topright",
         legend = c('Bullish top state', 'Bearish top state'),
         lwd = 2,
         col = c('green3', 'red'),
         bty = 'n', cex = par()$cex * 0.5,
         horiz = TRUE)

  # Plot 2 Volume
  volume.y <- as.vector(size)
  volume.x <- na.locf(cbind(size, zigzag$f2), fromLast = TRUE)
  volume.palette <- ifelse(volume.x$f2 == volume.up, 'green3',
                           ifelse(volume.x$f2 == volume.dn, 'red',
                                  'blue'))

  par(mar = c(3.1, 5.0, 0.0, 2.1))
  par(mgp = c(3.0, 1.0, 0.0))
  barplot(height = volume.y,
          ylab = expression("Volume" ~ v[t]),
          ylim = c(0, quantile(volume.y, 0.99)),
          # cex.axis = 0.70, cex.lab = 0.85,
          xaxt = 'n', yaxt = 's',
          border = volume.palette,
          col = volume.palette)
  box()

  title(xlab = expression("Time" ~ t), mgp = c(0.5, 0, 0))

  legend(x = "topleft",
         legend = c('Volume strengthens', 'Volumen weakens', 'Indeterminant'),
         lwd = c(2,  2,  2),
         col = c('green3', 'red', 'blue'),
         bty = 'n', cex = par()$cex * 0.5,
         y.intersp	= 0.0, # x.intersp = 0.2,
         # text.width = 20.0,
         horiz = TRUE)
  par(opar)
}

plot_topstate_features <- function(features, top, L, topstate.label = c("Bear", "Bull"), ...) {
  topstate <- sort(unique(top))
  n <- length(topstate)
  my.ylim <- c(0, 500 * ((max(table(features)) %/% 500) + 1))

  opar <- par(TRUE)
  par(mar = c(2.5, 4.1, 1.0, 2.1))
  par(mfrow = c(1, 2))
  for (i in 1:n) {
    zx <- features[top == topstate[i]]
    sg <- ifelse(zx < L + 1, 1, 2)
    fe <- ifelse(zx < L + 1, zx, zx - L)
    tab <- t(sapply(1:2, function(j) {table(factor(fe[sg == j], levels = 1:L))}))

    barplot(tab,
            main = bquote("Zig-zags (top state " * .(topstate.label[i]) * ")"),
            ylab = "Frequency",
            beside = TRUE, ylim = my.ylim,
            col = c('green3', 'red'), border = c('green3', 'red'), ...)

    legend(x = "topright",
           legend = c(expression('Positive leg' ~ U[i]),
                      expression('Negative leg' ~ D[i])),
           bty = 'n', horiz = FALSE,
           cex = par()$cex * 0.5,
           fill = c('green3', 'red'),
           border = c('green3', 'red'))
  }

  par(opar)
  invisible()
}

plot_topstate_trading <- function(tdata, zigzag, trades, main.lab = NULL) {
  # 2. Data extraction
  price <- tdata$PRICE
  size  <- tdata$SIZE

  if (!is.list(trades)) { trades <- list(trades) }

  opar <- par(no.readonly = TRUE)
  layout(matrix(1:3, nrow = 3, ncol = 1), heights = c(0.60, 0.25, 0.15))

  # 4. Plot I: Prices and features
  price.x    <- index(tdata)
  price.y    <- as.vector(price)
  price.top  <- tdata$topstate
  x.at <- axTicksByTime(tdata, format.labels = "%H:%M:%S")

  # Plot I Equity line
  my.ylim = c(min(sapply(trades, function(t){min(c(cumprod(1 + t$perchg), cumprod(1 + t$ret)))})),
              max(sapply(trades, function(t){max(c(cumprod(1 + t$perchg), cumprod(1 + t$ret)))})))
  par(mar = c(0.0, 5.0, 4.1, 2.1))
  # plot(x = equity.x, y = cumprod(1 + trades[[1]]$ret), type = 'l',
  plot(NULL,
       ylab = expression("Equity line"), # cex.axis = 1.00,
       xaxt = 'n', yaxt = 's', # cex.lab = 1.00,
       xlim = c(min(price.x), max(price.x)),
       ylim = my.ylim,
       lwd = 2.0, col = "lightgray")

  axis(3, at = xy.coords(price.x, price.y)$x[x.at],
       labels = names(x.at), las = 2) # cex.axis = 0.75,

  for (i in 1:length(trades)) {
    t <- trades[[i]]
    t.lty <- rep(1:6, ceiling(length(trades) / 6))[i]
    equity.x   <- index(t)
    equity.str <- cumprod(1 + t$ret)
    equity.top <- t$topstate
    equity.bnh <- cumprod(1 + t$perchg)

    all.palette <- tail(ifelse(equity.top == state.bull, 'green3',
                               ifelse(equity.top == state.bear, 'red',
                                      'blue')), -1)

    lines(equity.x, equity.str, col = i, lwd = 1)

    lines(equity.x, equity.bnh, col = "gray", lty = t.lty)

    text(x = last(equity.x), y = last(equity.str),
         pos = 4, labels = bquote("Trading lag = "*.(attr(t, 'lag'))*" ticks"),
         col = i, cex = par()$cex * 0.5) #cex = 1.0,
  }

  text(x = last(index(trades[[1]])), y = last(cumprod(1 + trades[[1]]$perchg)),
       pos = 4, labels = bquote("Buy and hold"),
       col = 'gray', cex = par()$cex * 0.5) #cex = 1.0,

  abline(h = 1, col = 'lightgray')

  if (!is.null(main.lab)) {
    text(x = par('usr')[1] - strheight("f"), y = par('usr')[4] - strheight("f"),
         labels = main.lab,
         pos = 4, font = 2, cex = par()$cex * 0.5) # cex = 1.0
  }

  # Plot II Price and features
  par(mar = c(0.0, 5.0, 0.0, 2.1))
  plot(x = price.x, y = price.y, type = 'l',
       ylab = expression("Price" ~ p[t]),
       # cex.axis = 1.00, cex.lab = 1.00,
       xaxt = 'n', yaxt = 's',
       lwd = 2.0, col = "lightgray")

  all.palette <- tail(ifelse(price.top == state.bull, 'green3',
                             ifelse(price.top == state.bear, 'red',
                                    'blue')), -1)

  segments(head(price.x, -1), head(price.y, -1),
           tail(price.x, -1), tail(price.y, -1),
           col = tail(all.palette, -1),
           lwd = 2)

  if (!is.null(main.lab)) {
    text(x = par('usr')[1] - strheight("f"), y = par('usr')[4] - strheight("f"),
         labels = main.lab,
         pos = 4, font = 2, cex = par()$cex * 0.5) # cex = 1.0,
  }

  legend(x = "topright",
         legend = c('Bullish top state', 'Bearish top state'),
         lwd = 2, y.intersp	= 0.0,
         col = c('green3', 'red'),
         bty = 'n', cex = par()$cex * 0.5, # cex = 1.0,
         horiz = TRUE)

  # Plot III Volume
  volume.y <- as.vector(size)
  volume.x <- na.locf(cbind(size, zigzag$f2), fromLast = TRUE)
  volume.palette <- ifelse(volume.x$f2 == volume.up, 'green3',
                           ifelse(volume.x$f2 == volume.dn, 'red',
                                  'blue'))

  par(mar = c(3.1, 5.0, 0.0, 2.1))
  par(mgp = c(3.0, 1.0, 0.0))
  barplot(height = volume.y,
          ylab = expression("Volume" ~ v[t]),
          ylim = c(0, quantile(volume.y, 0.99)),
          # cex.axis = 0.70, cex.lab = 0.85,
          xaxt = 'n', yaxt = 's',
          border = volume.palette,
          col = volume.palette)
  box()

  title(xlab = expression("Time" ~ t), mgp = c(0.5, 0, 0))

  legend(x = "topleft",
         legend = c('Volume strengthens', 'Volumen weakens', 'Indeterminant'),
         lwd = c(2,  2,  2),
         col = c('green3', 'red', 'blue'),
         bty = 'n', cex = par()$cex * 0.5, # cex = 0.85,
         y.intersp	= 0.0,
         horiz = TRUE)

  par(opar)
}
