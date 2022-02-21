tnorm <- function(x, m, s, a, b) {
  
  ifelse(x > a & x < b,
         dnorm(x, m, s) / (pnorm(b, m, s) - pnorm(a, m, s)),
         0)
  
}

discnorm <- function(x, m, s) {
  
  ifelse(x %% 1 == 0,
         pnorm(x + 1 / 2, m, s) - pnorm(x - 1 / 2, m, s),
         0)
  
}

dtnorm <- function(x, m, s, a, b) {
  
  ifelse(x > a & x < b,
         discnorm(x, m, s) / sum(discnorm((floor(a) + 1):(ceiling(b) - 1), m, s)),
         0)
  
}

tdnorm <- function(x, m, s, a, b) {
  
  # check if same as above -- where to cut off?
  
}

fnorm <- function(x, m, s, a = 0, b = 11, p, q) {
  if (!(a < b - 3)) stop("a < b - 3 necessary (H)")
  
  x <- sort(unique(x))
  
  sc <- numeric(length(x))
  
  if (any(x==1 | x==10)) {
    
    sc[which(x==1)]  <- p
    sc[which(x==10)] <- q
    
  }
  
  return((dtnorm(x, m, s, a, b) + sc) / (1 + p + q))
  
}