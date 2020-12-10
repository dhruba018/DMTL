## Function for performing distribution matching between source and reference 
## datasets- extended from the idea of histogram matching (discrete) to matching
## continuous density estimates 
## 
## Dependency: stats, ks 
## Dependency_own: lambda_functions, match_func 
################################################################################

source("lambda_functions.R")


## Define matching function...
match_func <- function(knots, vals, new_vals, lims) {
  
  ## Limits for function inputs...
  if (missing(lims)) 
    lims <- c(min(knots), max(knots))
  
  ## Inverse CDF mapping...
  map <- stats::approxfun(x = vals, y = knots, yleft = lims[1], yright = lims[2], method = "linear", ties = "ordered", rule = 2)
  
  ## Get matched values...
  new_knots <- confined(map(new_vals), lims)
  new_knots
  
}


## Distribution matching for two CDFs...
dist_match <- function(src, ref, src_dist, ref_dist, lims, match_method = "hist", samp_size = 1e6, rand_seed = NULL) {
  
  ## Get distributions...
  source("get_dist_est.R")
  
  if (missing(ref_dist)) {
    ref_dist <- get_dist_est(ref, sample_size = samp_size, x_range = "unit", dist_method = match_method, 
                             grid_size = 1e3, random_seed = rand_seed)
  }
  
  if (missing(src_dist)) {
    src_dist <- get_dist_est(src, sample_size = samp.size, x_range = "unit", dist_method = match_method, 
                             grid_size = 1e3, random_seed = rand_seed)
  }
  
  
  ## Mapping parameters...
  match_method <- tolower(match_method)
  
  if (match_method == "hist") {                           # Using histogram
    kn_vals <- knots(ref_dist);              fn_vals <- ref_dist(kn_vals)
    vals_to_match <- src_dist(src)
  } 
  
  else if (match_method == "dens") {                      # Using kernel density
    kn_vals <- ref_dist$eval.points;         fn_vals <- ref_dist$estimate
    vals_to_match <- predict(src_dist, x = src)
  } 
  
  else {
    stop("Invalid option for estimating distribution! Please use 'hist' for histogram or 'dens' for kernel density.")
  }
  
  ## Perform mapping...
  # source("match_func.R")
  
  if (missing(lims))
    lims <- c(min(src), max(src))
  
  matched <- match_func(knots = kn_vals, vals = fn_vals, new_vals = vals_to_match, lims)
  matched
  
}