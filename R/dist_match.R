#' Estimate Inverse Map
#'
#' This function estimates an inverse map \eqn{g} for a given set of knots
#' (input) and values (output) corresponding to a certain map \eqn{f} _i.e._,
#' given \eqn{x, y | f: x --> y}, `match_func()` estimates \eqn{g: y --> x}
#' using linear interpolation.
#'
#' @param knots Vector containing knots for the distribution estimate.
#' @param vals Vector containing distribution values corresponding to the knots.
#' @param new_vals Vector containing distribution values for which the knots
#' are unknown. If missing, `match_func()` simply returns the map function.
#' @param lims Vector providing the range of the knot values for mapping.
#' If missing, these values are estimated from the given knots.
#' @param get_func Flag for returning the map function if `new_vals` is
#' provided. If `TRUE`, returns a named list with two components-  `mapped`
#' and `func` (mapped knots for `new_vals` and the mapping function,
#' respectively). Defaults to `FALSE`.
#'
#' @keywords inverse-map
#' @export
#' @examples
#' set.seed(654321)
#' x <- rnorm(100, 1, 0.5)
#' F <- ecdf(x)
#' fval <- F(x)
#' map <- match_func(knots = x, vals = fval)
#'
#' x2 <- rnorm(20, 0.8, 0.5)
#' F2 <- ecdf(x2)
#' fval2 <- F2(x2)
#' matched <- match_func(knots = x, vals = fval, new_vals = fval2)
#'
##
## Dependency: stats, ks
## Dependency_own: lambda_functions
################################################################################


## Define matching function...
match_func <- function(knots, vals, new_vals, lims, get_func = FALSE) {

  ## Limits for function inputs...
  if (missing(lims))
    lims <- range(knots)

  ## Inverse CDF mapping...
  map <- stats::approxfun(x = vals, y = knots, yleft = lims[1], yright = lims[2], method = "linear", ties = "ordered", rule = 2)

  if (missing(new_vals))        # Return map function
    return( map )

  ## Matched values...
  new_knots <- confined(map(new_vals), lims)

  if (get_func)                 # Return function & mapped values
    return( list("mapped" = new_knots, "func" = map) )

  new_knots                     # Return mapped values only

}


#' Distribution Matching for Source and Reference Datasets
#'
#' This function matches the source distribution to a reference distribution
#' so that the source data can effectively transferred to the reference space.
#'
#' @param src Vector containing the source data to be matched.
#' @param ref Vector containing the reference data to estimate the reference
#' distribution for matching.
#' @param src_dist Estimation of the source distribution. If missing, it is
#' estimated from the provided source data.
#' @param ref_dist Estimation of the reference distribution. If missing, it is
#' estimated from the provided reference data.
#' @param lims Vector giving the range for the knot values. If missing, these
#' values are estimated from the given source data.
#' @param density Flag for using kernel density estimates for matching
#' distributions instead of histogram counts. Defaults to `False`.
#' @param samples Sample size for estimating distributions if `src_dist` and/or
#' `ref_dist` are missing. Defaults to `1e6`.
#' @param seed Seed for random number generator (for reproducible outcomes).
#' Defaults to `NULL`.
#'
#' @keywords distribution-matching domain-transfer
#' @export
#' @examples
#'
##
## Dependency: stats, ks
## Dependency_own: lambda_functions, match_func
################################################################################


## Distribution matching for two CDFs...
dist_match <- function(src, ref, src_dist, ref_dist, lims, density = FALSE, samples = 1e6, seed = NULL) {

  ## Get distributions...
  if (missing(ref_dist))
    ref_cdf <- estimate_cdf(ref, bootstrap = TRUE, samples, density, binned = TRUE, grids = 1e3, unit_range = TRUE, seed)

  if (missing(src_dist))
    src_cdf <- estimate_cdf(src, bootstrap = TRUE, samples, density, binned = TRUE, grids = 1e3, unit_range = TRUE, seed)


  ## Mapping parameters...
  if (density) {                        # Using kernel density
    kn_vals <- ref_dist$eval.points;        fn_vals <- ref_dist$estimate
    vals_to_match <- predict(src_dist, x = src)
  }

  else {                                # Using histogram
    kn_vals <- knots(ref_dist);             fn_vals <- ref_dist(kn_vals)
    vals_to_match <- src_dist(src)
  }


  ## Perform mapping...
  if (missing(lims))
    lims <- range(src)

  matched <- match_func(knots = kn_vals, vals = fn_vals, new_vals = vals_to_match, lims)
  matched

}
