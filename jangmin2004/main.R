library(quantmod)

# Set up ------------------------------------------------------------------
# Model
l1.Klab = c('Strong bear', 'Weak bear', 'Random walk', 'Weak bull', 'Strong bull')
l1.W = 22

# Data
syms <- data.frame(
  symbol     = c("006400.KS", "006400.KS"),
  name       = c("Samsung SDI", "Samsung SDI Co., Ltd."),
  train.from = c("1998-01-01", "1998-01-01"),
  train.to   = c("2003-06-30", "2003-06-30"),
  test.from  = c("2003-07-01", "2003-07-01"),
  test.to    = c("2004-01-01", "2004-01-01"),
  src        = c("yahoo", "yahoo"),
  stringsAsFactors = FALSE)

# Data fetching and pre-processing ----------------------------------------
sym      <- syms[1, ] # Samsung SDI
prices   <- getSymbols(sym$symbol,
                       env  = NULL,
                       from = sym$train.from,
                       to   = sym$test.to,
                       src  = sym$src)
prices   <- na.omit(prices)

T.length <- nrow(prices[paste(sym$train.from, sym$train.to, sep = "/")])

dataset  <- list()
dataset$trainset <- as.numeric(prices[1:T.length, 4])

# Level 1 (semi-supervision) ----------------------------------------------
l1.K = length(l1.Klab)
l1.Wlength <- T.length - l1.W

dataset$ma_t <- vector("numeric", l1.Wlength)
for (i in 1:l1.Wlength) {
  dataset$ma_t[i] = mean(dataset$trainset[i:(i + l1.W - 1)])
}

dataset$magrad_t <- diff(dataset$ma_t)
dataset$z_1 <- rep(0, length(dataset$magrad_t))

qinf <- quantile(dataset$magrad_t, 0.4)
qsup <- quantile(dataset$magrad_t, 0.6)

dataset$z_1[dataset$magrad_t > quantile(dataset$magrad_t, 0.4)
            & dataset$magrad_t < quantile(dataset$magrad_t, 0.6)] <- 3




dataset$z_1      <- kmeans(dataset$magrad_t, l1.K)$cluster

# Relabelling (ugly hack edition)
relabel <- function(data, old) {
  rep.ord <- order(by(data, old, mean))

  new <- rep(0, length(data))
  for (k in 1:length(unique(old)))
    new[which(old == rep.ord[k])] <- k

  new
}

dataset$z_1relab <- relabel(dataset$magrad_t, dataset$z_1)

# rep.ord <- order(by(dataset$magrad_t, dataset$z_1, mean))
#
# dataset$z_1relab <- rep(0, l1.Wlength - 1)
# for (k in 1:l1.K)
#   dataset$z_1relab[which(dataset$z_1 == rep.ord[k])] <- k

print("Label re-imputation (ascending order)")
as.numeric(by(dataset$magrad_t, dataset$z_1relab, mean))

opar <- par(no.readonly = TRUE)

layout(matrix(c(1, 2), nrow = 2, ncol = 1, byrow = TRUE), heights = c(0.95, 0.05))
plot(x = 1:T.length, y = dataset$trainset,
     main = bquote("Price (" * .(sym$symbol) * ")"),
     xlab = bquote(t), ylab = bquote(x[t]),
     type = 'p', pch = 21, cex = 0.7, col = 'lightgray', bg = 'lightgray')

segments(x0 = head(1:length(dataset$ma_t), -1), y0 = head(dataset$ma_t, -1),
         x1 = 2:length(dataset$ma_t), y1 = tail(dataset$ma_t, -1),
         col = dataset$z_1relab, lwd = 4)

par(mai = c(0, 0, 0, 0))
plot.new()

legend(x = "center",
       legend = l1.Klab,
       lwd = 2, col = 1:5,
       bty = 'n', horiz = TRUE)

par(opar)

layout(matrix(c(1, 2, 3, 3), nrow = 2, ncol = 2, byrow = TRUE), heights = c(0.95, 0.05))
plot(x = 1:T.length, y = dataset$trainset[1:T.length],
     main = bquote("Price (" * .(sym$symbol) * ")"),
     xlab = bquote(t), ylab = bquote(x[t]),
     type = 'p', pch = 21, cex = 0.7,
     bg = dataset$z_1relab, col = dataset$z_1relab)

plot(x = 1:length(dataset$magrad_t), y = dataset$magrad_t,
     main = bquote("Trend (" * .(sym$symbol) * ")"),
     xlab = bquote(t), ylab = bquote(nabla ~ "MA(" * t * ")"^22),
     type = 'p', pch = 21, cex = 0.7,
     bg = dataset$z_1relab, col = dataset$z_1relab)

for (k in 1:(l1.K - 1))
  abline(h = max(dataset$magrad_t[dataset$z_1relab == k]), col = 'lightgray')

par(mai = c(0, 0, 0, 0))
plot.new()

legend(x = "center",
       legend = l1.Klab,
       pch = 21, pt.bg = 1:5, col = 1:5,
       pt.cex = 1.5, bty = 'n', horiz = TRUE)

par(opar)


