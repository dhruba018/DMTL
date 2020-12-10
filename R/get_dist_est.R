#' Estimate cumulative distribution
#'
#' This function estimates the values of the cumulative distribution function
#' (CDF) for a vector.
#' @param x Vector containing data.
#' @param size_tol The size tolerance for performing bootstrapping on `x` to
#' estimate distribution values. Defaults to `1e3`.
#' @param sample_size Sample size used for bootstrapping. Defaults to `1e6`.
#' @param density Flag for calculating kernel density estimates instead of
#' histogram counts. Uses functions from the `ks` package to estimate kernel
#' density values. Defaults to `FALSE`.
#' @param binned Flag for calculating binned estimates of kernel density if
#' `density = TRUE`.
#' @param grid_size Size of estimation grid. Defaults to `1e4`.
#' @param x_range Range of data values. Options are either `"unit"` (normalized
#' in [0, 1]) or `"est"` (estimated from data). Defaults to `"est"`.
#' @param random_seed Seed for random number generator (for reproducibility).
#' Defaults to `NULL`.
#' @param ... Other options relevant for distribution estimation.
#'
#' @keywords cumulative-distribution CDF
#' @export
#' @examples
#' x <- runif(100)
#' x_cdf <- get_dist_est(x, density = FALSE, x_range = "unit")
#'

## Estimate cumulative distributions for a vector to be used in the
## distribution matching step
##
## Dependency: stats, ks
################################################################################

get_dist_est <- function(x, size_tol = 1e3, sample_size = 1e6, density = FALSE, binned = TRUE,
                         grid_size = 1e4, x_range = "est", random_seed = NULL, ...) {

    ## Get data range...
    x_range <- switch (x_range,
        "unit" = c(min = 0, max = 1),
        "est"  = c(min = min(x), max = max(x))
    )

    ## Bootstrapping...
    set.seed(random_seed)                       # For reproducibility
    xx <- if (length(x) < size_tol) sample(x, size = sample_size, replace = TRUE) else x


    ## Calculate cumulative distribution...
    dist_method <- tolower(dist_method)

    if (dist_method == "hist") {                # Use histogram
        x_cdf <- stats::ecdf(xx, ...)
    }

    else if (dist_method == "dens") {           # Use kernel density
        # if (!require(ks))                       # Load package
        #     library(ks)

        bw <- ks::hscv(x, nstage = 2, binned = TRUE, bgridsize = grid_size * 10)

        x_cdf <- switch (binned,

            ## Binned estimate for faster computation...
            "TRUE"  = ks::kcde(xx, h = bw, binned = TRUE, bgridsize = grid_size,
                               xmin = x_range[1], xmax = x_range[2], ...),

            ## Continuous estimate for increased accuracy...
            "FALSE" = ks::kcde(xx, h = bw, binned = FALSE, gridsize = grid_size / 10,
                               xmin = x_range[1], xmax = x_range[2], ...)
        )
    }

    else {
        stop("Invalid option for estimating distribution! Please use 'hist' for histogram or 'dens' for kernel density.")
    }

    x_cdf

}
