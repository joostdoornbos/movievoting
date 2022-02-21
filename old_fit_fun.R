fit_fun <- function(df, params = list(), opt_type = "constr_mean",
                    min_type = "L2", distr_fun = "fnorm") {
  
  # list of pars, with starting values required
  
  # TODO: add distr_fun = not just fnorm,
  #       add stop for invalid starting parameters
  #       check overlap in opt_types
  #       regular tnorm fit to compare p,q
  #       check if a!=0, b!=11 works
  #       check if min_types stuff can be done nicer
  #       allow for non-1:10 support input in distr_fun
  #       split into multiple functions depending on what distr_fun is being used
  
  # starting values if not input
  if (!("m" %in% names(params))) {
    
    params$m <- 4.5
    
  }
  if (!("s" %in% names(params))) {
    
    params$s <- 1
    
  }
  if (!("a" %in% names(params))) {
    
    params$a <- 0
    
  }
  if (!("b" %in% names(params))) {
    
    params$b <- 11
    
  }
  if (!("p" %in% names(params))) {
    
    params$p <- 0.1
    
  }
  if (!("q" %in% names(params))) {
    
    params$q <- 0.1
    
  }
  
  opt_types <- c("constr_mean", "endpoint_inflation_negative",
                 "endpoint_inflation_positive")
  if (!(opt_type %in% opt_types)) stop("Wrong optim type (H)")
  
  distr_funs <- c("fnorm", "lnorm")
  if (!(distr_fun %in% distr_funs)) stop("Invalid fitting distribution (H)")
  source(paste0(distr_fun, ".R"))
  
  min_types <- c("L1", "L2", "CE")
  if (!(min_type %in% min_types)) stop("Invalid loss function (H)")
  
  min_type <- which(min_types == min_type)
  
  if (distr_fun == "fnorm") {
    
    params <- c(params$m, params$s, params$a, params$b, params$p, params$q)
    
  } else if (distr_fun == "lnorm") {
    
    params <- c(params$m, params$s)
    
  }
  
  distr_fun <- get(distr_fun)
  
  # L1, L2 norms and cross-entropy.
  min_funs <- list(
    L2 = function(params, values) {
      sum((distr_fun(1:10, params) - values)^2)
    },
    L1 = function(params, values) {
      sum(abs(distr_fun(1:10, params) - values))
    },
    CE = function(params, values) {
      -1 * sum(values * log(distr_fun(1:10, params)))
    }
  )
  
  min_fun <- min_funs[[min_type]]
  
  if (opt_type == "constr_mean") {
    
    # Constrain the mean to be between 1 and 10.
    ui <- cbind(rbind(matrix(c(-1, rep(0, 3)), nrow = 1), diag(4)),
                matrix(rep(0, 10), ncol = 2))
    ci <- c(-10, rep(0, 4))
    return(constrOptim(params, min_fun, NULL, ui, ci, values = df))
    
  } else if (opt_type == "endpoint_inflation_negative") {
    
    # Will allow p, q to be negative, as long as f(1)+p and f(10)+q are positive.
    # Not sure if it will ever be useful, and the bounds would be dynamic.
    stop("WIP (H)")
    
  } else if (opt_type == "endpoint_inflation_positive") {
    
    # Constrain the added endpoint inflation >=0.
    ui <- cbind(rep(0,3), diag(3))
    ci <- 
    return(constrOptim(params, min_fun, NULL, ,
                          c(0, 0, 0), values = df))
    
  }
  
}
