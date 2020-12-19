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
#' @param target_set List containing the target datasets. A named list with two
#' components- `x` (features) and `y` (response). These two sets do not need to
#' be matched and the response values are only used to estimate distribution
#' while the corresponding response values for the features are predicted.
#' @param source_set List containing the source datasets. A named list with two
#' components- `x` (features) and `y` (response). These two sets must be matched
#' and used in both distribution estimation and training a predictive model.
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
#' @keywords distribution-matching transfer-learning domain-transfer
#' @export
#' @examples
#'
##
## Dependency: stats, ks, randomForest
## Dependency_own: lambda_functions, get_dist_est, dist_match, RF_predict
##
## Author: SR Dhruba, Nov 2020
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
        # dist_match(X1[, j], ref = X2[, j], match_method = method, samp_size = size, lims = data_lims, rand_seed = seed)
        dist_match(X1[, j], ref = X2[, j], density = use_density, lims = data_lims, samples = sample_size, seed = random_seed)
        })
    X2_map <- as.data.frame(X2_map);    dimnames(X2_map) <- dimnames(X1)
    # rownames(X2_map) <- rownames(X1);   colnames(X2_map) <- colnames(X1)


    ## Perform prediction & map back to original space...
    y2_pred_map <- RF_predict(x_train = X2, y_train = y2, x_test = X2_map, y_lims = data_lims,
                              n_tree = 200, m_try = 0.4, random_seed = seed)
    # y2_cdf  <- get_dist_est(y2, sample_size = size, x_range = "unit", dist_method = method, grid_size = 1e3, random_seed = seed)
    #
    # y1_pred <- dist_match(y2_pred_map, ref = y1, src_dist = y2_cdf, match_method = method, samp_size = size,
    #                       lims = data_lims, rand_seed = seed)
    y2_cdf  <- estimate_cdf(y2, samples = sample_size, unit_range = TRUE, density = use_density, grids = 1e3, seed = random_seed)

    y1_pred <- dist_match(y2_pred_map, ref = y1, src_dist = y2_cdf, density = use_density, samples = sample_size,
                          lims = data_lims, seed = random_seed)

    y1_pred <- confined(y1_pred, lims = data_lims);      names(y1_pred) <- names(y2_pred_map)


    ## Return output objects...
    if (all_pred)
        return( list("target" = y1_pred, "source" = y2_pred_map) )

    y1_pred

}
