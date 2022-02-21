fnorm <- function(x, m = 4.5, s = 1, a = 0, b = 11, p = 0, q = 0) {
  
  # p, q are added to account for people overly giving a 1 or 10,
  # but the params of course should be able to be added
  # (H) reminds it is a human created error msg
  if ((p != 0 | q != 0) & !any(x == 1 | x == 10)) stop("Cannot add p, q to x. (H)")
  
  # necessary to have two values to add p,q to and one in between
  if (!(a < b - 3)) stop("a < b - 3 necessary. (H)")
  
  # convert continuous normal pdf into discrete
  # it is centered at integers, since ratings are 1:10
  discnorm <- function(x, m, s) {
    
    ifelse(x %% 1 == 0,
           pnorm(x + 1 / 2, m, s) - pnorm(x - 1 / 2, m, s),
           0)
    
  }
  
  # truncate the above discrete normal dist to be >a and <b
  # a = 0, b = 11 would then give the appropriate range for voting distributions
  dtnorm <- ifelse(x > a & x < b,
                   discnorm(x, m, s) / sum(discnorm((floor(a) + 1):(ceiling(b) - 1), m, s)),
                   0)
  # figure out where to add p, q
  x <- sort(unique(x))
  sc <- numeric(length(x))
  if (any(x==1 | x==10)) {
    
    sc[which(x==1)] <- p
    sc[which(x==10)] <- q
    
  }
  
  return((dtnorm + sc) / (1 + p + q))
  
}
