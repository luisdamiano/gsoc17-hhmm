# From https://gist.github.com/aufrank/83572
logsumexp <- function(x) {
  y = max(x)
  y + log(sum(exp(x - y)))
}

softmax <- function(x) {
  exp(x - logsumexp(x))
}
