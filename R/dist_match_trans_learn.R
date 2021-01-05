#' Distribution Matching based Transfer Learning
#'
#' This function performs distribution matching based transfer learning (DMTL)
#' regression for given target (primary) and source (secondary) datasets. The
#' data available in the source domain are used to design an appropriate
#' predictive model. The target features with unknown response values are
#' transferred to the source domain _via_ distribution matching and then the
#' corresponding response values in the source domain are predicted using the
#' aforementioned predictive model. The response values are then transferred to
#' the original target space by applying distribution matching again. Hence,
#' this function needs an **unmatched** pair of target datasets (features and
#' response values) and a **matched** pair of source datasets.
#'
#' @param target_set List containing the target datasets. A named list with
#' components `X` (features) and `y` (response). The predictions are performed
#' to estimate the response values corresponding to `X` while `y` is only used
#' to estimate the response distribution parameters.
#' @param source_set List containing the source datasets. A named list with
#' components `X` (features) and `y` (response). These two sets must be matched
#' and used in both distribution estimation and predictive modeling.
#' @param use_density Flag for using kernel density as distribution estimate
#' instead of histogram counts. Defaults to `FALSE`.
#' @param sample_size Sample size for estimating distributions of target and
#' source datasets. Defaults to `1e3`.
#' @param random_seed Seed for random number generator (for reproducible
#' outcomes). Defaults to `NULL`.
#' @param all_pred Flag for returning the prediction values in the source space.
#' If `TRUE`, `DMTL()` returns a named list with two components- `target` and
#' `source` (predictions in the target space and source space, respectively).
#' Defaults to `FALSE`.
#'
#' @return
#' If `all_pred = FALSE`, a vector containing the final prediction values.
#'
#' If `all_pred = TRUE`, a named list with two components `target` and `source`
#' _i.e._, predictions in the original target space and in source space,
#' respectively.
#'
#' @note The datasets in `target_set` (_i.e._, `X` and `y`) do not need to be
#' matched (_i.e._, have the same number of rows) since the response values are
#' used only to estimate distribution for mapping while the feature values are
#' used for both mapping and final prediction. In contrast, the datasets in
#' `source_set` (_i.e._, `X` and `y`) must have matched samples.
#'
#' @keywords distribution-matching transfer-learning domain-transfer
#' histogram-matching density-matching
#' @export
#' @examples
#'

## Dependency: stats, ks
## Dependency_own: lambda_functions, estimate_cdf, dist_match,
##                  predictive_modeling
##
## Author: SR Dhruba, Dec 2020
################################################################################

DMTL <- function(target_set, source_set, use_density = FALSE, sample_size = 1e3, random_seed = NULL, all_pred = FALSE) {

    ## Initial check...
    if (ncol(target_set[["X"]]) != ncol(source_set[["X"]]))
        stop("Source and target set covariates must have the same number of features!")

    if (nrow(source_set[["X"]]) != length(source_set[["y"]]))
        stop("Source sets must have the same number of samples!")

    out01 <- function(y) (min(y) < 0) & (max(y) > 1)
    if (out01(target_set[["y"]]) | out01(source_set[["y"]]))
        stop("Response data must be normalized in [0, 1].")


    ######## MAIN ##############################################################

    ## Define datasets...
    X1 <- norm_data(target_set[["X"]]);      y1 <- target_set[["y"]]
    X2 <- norm_data(source_set[["X"]]);      y2 <- source_set[["y"]]
    n_feat <- ncol(X1);                      data_lims <- c(0, 1)


    ## Distribution matching for predictors...
    X2_map <- lapply(1:n_feat, function(j) {
        dist_match(X1[, j], ref = X2[, j], density = use_density, lims = data_lims, samples = sample_size, seed = random_seed)
        })
    X2_map <- as.data.frame(X2_map);    dimnames(X2_map) <- dimnames(X1)


    ## Perform prediction & map back to original space...
    y2_pred_map <- RF_predict(x_train = X2, y_train = y2, x_test = X2_map, lims = data_lims,
                              n_tree = 200, m_try = 0.4, seed = random_seed)
    y2_cdf  <- estimate_cdf(y2, samples = sample_size, unit_range = TRUE, density = use_density, grids = 1e3, seed = random_seed)

    y1_pred <- dist_match(y2_pred_map, ref = y1, src_cdf = y2_cdf, density = use_density, samples = sample_size,
                          lims = data_lims, seed = random_seed)

    y1_pred <- confined(y1_pred, lims = data_lims);      names(y1_pred) <- names(y2_pred_map)


    ## Return output objects...
    if (all_pred)
        return( list("target" = y1_pred, "source" = y2_pred_map) )

    y1_pred

}
