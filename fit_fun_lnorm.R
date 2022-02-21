fit_fun_lnorm <- function(df, ps, opt_type = "constr_mean",
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
  
  opt_types <- c("constr_mean")
  if (!(opt_type %in% opt_types)) stop("Wrong optim type (H)")
  
  min_types <- c("L2", "L1", "CE")
  if (!(min_type %in% min_types)) stop("Invalid loss function (H)")
  
  source("lnorm.R")
  
  min_type <- which(min_types == min_type)
  
  # L1, L2 norms and cross-entropy.
  min_funs <- list(
    L2 = function(ps, values) {
      sum((lnorm(1:10, ps[1], ps[2]) - values)^2)
    },
    L1 = function(ps, values) {
      sum(abs(lnorm(1:10, ps[1], ps[2]) - values))
    },
    CE = function(ps, values) {
      -1 * sum(values * log(lnorm(1:10, ps[1], ps[2])))
    }
  )
  
  min_fun <- min_funs[[min_type]]
  
  if (opt_type == "constr_mean") {
    
    # Constrain the mean to be between 1 and 10.
    ui <- matrix(c(-1, 1, numeric(2)), nrow = 2)
    ci <- c(-10, 1)
    return(constrOptim(ps, min_fun, NULL, ui, ci, values = df))
    
  }
  
}
