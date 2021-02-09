#' @export
#'
## Contains the anonymous functions for various usage
##
## Dependency: base, stats
################################################################################

## Normalization functions...
#' @export
# Normalize in [0, 1]...
norm01 <- function(z) (z - min(z)) / (max(z) - min(z))

#' @export
# Normalize dataframe/matrix per column...
norm_data <- function(Z) as.data.frame(apply(Z, MARGIN = 2, FUN = norm01))

#' @export
# Standardize dataframe/matrix per column...
zscore <- function(Z) as.data.frame(apply(Z, MARGIN = 2, FUN = scale))


#' @export
## Restrict data in a given interval (default = [0, 1])...
confined <- function(y, lims = c(0, 1)) {
  y[y < lims[1]] <- lims[1];      y[y > lims[2]] <- lims[2]
  y
}

#' @export
## Evaluate regression model performance...
get_perf <- function(y_obs, y_pred, measures = c("NRMSE", "NMAE", "SCC")) {

  ## Initialize results array...
  perf_vals <- c()

  for (mes in measures) {

    ## Calculate squared error...
    if (grepl(pattern = "MSE", mes, ignore.case = TRUE)) {
      num <- mean((y_obs - y_pred)^2)
      den <- if (mes == "NRMSE") mean((y_obs - mean(y_obs))^2) else 1
      pow <- if (mes == "MSE") 1 else 0.5
      perf_vals[mes] <- (num / den)^pow
    }

    ## Calculate absolute error...
    else if (grepl(pattern = "MAE", mes, ignore.case = TRUE)) {
      num <- mean(abs(y_obs - y_pred))
      den <- if (mes == "NMAE") mean(abs(y_obs - mean(y_obs))) else 1
      perf_vals[mes] <- num / den
    }

    ## Calculate similarity measures...
    else if (grepl(pattern = "CC", mes, ignore.case = TRUE)) {
      alg <- if (mes == "SCC") "spearman" else "pearson"
      perf_vals[mes] <- stats::cor(y_obs, y_pred, method = alg)
    }

    ## Doesn't match any...
    else
      stop("Invalid measure! Please use common variants of MSE, MAE or correlation coefficients.")
  }

  perf_vals
}
