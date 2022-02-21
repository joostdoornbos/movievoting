fit_fun_fnorm <- function(df, ps, opt_type = "constr_mean",
                    min_type = "L2") {
  
  # list of pars, with starting values required
  
  # TODO: add distr_fun = not just fnorm,
  #       add stop for invalid starting parameters
  #       check overlap in opt_types
  #       regular tnorm fit to compare p,q
  #       check if a!=0, b!=11 works -- actually change it first
  #       check if min_types stuff can be done nicer
  #       allow for non-1:10 support input in distr_fun
  #       split into multiple functions depending on what distr_fun is being used
  
  opt_types <- c("constr_mean", "endpoint_inflation_negative",
                 "endpoint_inflation_positive")
  if (!(opt_type %in% opt_types)) stop("Wrong optim type (H)")
  
  min_types <- c("L2", "L1", "CE")
  if (!(min_type %in% min_types)) stop("Invalid loss function (H)")
  
  source("fnorm.R")
  
  min_type <- which(min_types == min_type)
  
  # L1, L2 norms and cross-entropy.
  min_funs <- list(
    L2 = function(ps, values) {
      sum((fnorm(1:10, ps[1], ps[2], 0, 11, ps[3], ps[4]) - values)^2)
    },
    L1 = function(ps, values) {
      sum(abs(fnorm(1:10, ps[1], ps[2], 0, 11, ps[3], ps[4]) - values))
    },
    CE = function(ps, values) {
      -1 * sum(values * log(fnorm(1:10, ps[1], ps[2], 0, 11, ps[3], ps[4])))
    }
  )
  
  min_fun <- min_funs[[min_type]]
  
  if (opt_type == "constr_mean") {
    
    # Constrain the mean to be between 1 and 10.
    ui <- rbind(matrix(c(-1, numeric(3)), nrow = 1), diag(4))
    ci <- c(-10, numeric(4))
    return(constrOptim(ps, min_fun, NULL, ui, ci, values = df))
    
  } else if (opt_type == "endpoint_inflation_negative") {
    
    # Will allow p, q to be negative, as long as f(1)+p and f(10)+q are positive.
    # Not sure if it will ever be useful, and the bounds would be dynamic.
    stop("WIP (H)")
    
  } else if (opt_type == "endpoint_inflation_positive") {
    
    # Constrain the added endpoint inflation >=0.
    ui <- cbind(numeric(3), diag(3))
    ci <- numeric(3)
      return(constrOptim(ps, min_fun, NULL, ui, ci, values = df))
    
  }
  
}
