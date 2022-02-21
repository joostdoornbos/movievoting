fit_fun <- function(df, params = list(), distr, opt_type, min_type) {
  
  # rough for now
  
  source(paste0("fit_fun_", distr, ".R"))
  
  if (distr == "fnorm") {
    
    all_params <- c("m", "s", "p", "q")
    idx = which(names(params) %in% all_params)
    ps <- unlist(params[idx])
    ps <- ps[order(factor(names(ps), levels = c("m", "s", "p", "q")))]
    
    return(fit_fun_fnorm(df, ps, opt_type, min_type))
    
  }
  if (distr == "lnorm") {
    
    all_params <- c("m", "s")
    idx = which(names(params) %in% all_params)
    ps <- unlist(params[idx])
    ps <- ps[order(factor(names(ps), levels = c("m", "s")))]
    
    return(fit_fun_lnorm(df, ps, opt_type, min_type))
    
  }
  
}
