topstate_trading <- function(tdata, lag) {
  signal <- ((1:nrow(tdata))[tdata$topstate != lag(tdata$topstate)])[-1]
  start  <- ifelse(signal + lag > nrow(tdata), nrow(tdata), signal + lag)
  end    <- c(tail(start, -1), nrow(tdata))
  action <- ifelse(tdata$topstate[signal] == state.bear, -1, 1)
  entryp <- as.numeric(tdata[start, 1])
  exitp  <- as.numeric(tdata[end, 1])
  perchg <- (exitp - entryp) / entryp
  ret <- cbind(action  = action,
               signal  = signal,
               start   = start,
               end     = end,
               entryp  = entryp,
               exitp   = exitp,
               perchg  = perchg,
               ret     = as.numeric(action) * perchg)
  attr(ret, 'lag') <- lag
  return(ret)
}

buyandhold <- function(tdata) {
  entryp <- as.numeric(head(tdata[, 1], -1))
  exitp  <- as.numeric(tail(tdata[, 1], -1))
  return((exitp - entryp) / entryp)
}
