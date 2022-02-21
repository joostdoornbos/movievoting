dist_convert <- function(df) {
  require(magrittr)
  
  # convert data into something useable
  x <- df$Distribution %>% strsplit(split = "") %>% unlist()
  x <- replace(x, x==".", "10")
  x <- replace(x, x=="*", "11")
  
  # replace values given in the string by midbpoints of histogram bars
  # e.g., something getting 1-10% of votes is said to have gotten 4.5%
  old <- c(10, 0:9, 11)
  new <- c(0, seq(4.5, 94.5, 10), 100)
  
  # ugly but effective way of converting and replacing the values
  x <- x %>% as.integer() %>% factor(old, new) %>%
    as.character() %>% as.double()
  
  # rescaled to sum to 1 -- it is a pmf
  # I think it would not automatically due to rounding stuff with my 4.5's.
  x <- t(matrix(x, byrow = T, ncol = 10))
  y <- scale(x, F, colSums(x)) %>% t() %>% as.data.frame()
  
  return(y)
  
}