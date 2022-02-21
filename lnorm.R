lnorm <- function(x, m = 4.5, s = 1) {
  
  # TODO: make the endpoints not fixed
  
  x <- unique(x)
  y <- numeric(length(x))
  ends <- numeric(length(x))
  idx <- which(x %% 1 == 0)
  x <- x[idx]
  
  pmf <- pnorm(x + 1 / 2, m, s) - pnorm(x - 1 / 2, m, s)
  ends[which(x == 1)] <- pnorm(1 / 2, m, s)
  ends[which(x == 10)] <- 1 - pnorm(21 / 2, m, s)
  y[idx] <- pmf +  ends
  
  return(y)
  
}
