## Perform distribution mapping based transfer learning regression 
## 
## Dependency: stats, ks, randomForest
## Dependency_own: lambda_functions, get_dist_est, dist_match, RF_predict
##
## Author: SR Dhruba, Nov 2020
################################################################################

DMTL <- function(target_set, source_set, method = "hist", size = 1e3, seed = NULL, pred_all = FALSE) {
    
    ## Initial check...
    if (ncol(target_set[["X"]]) != ncol(source_set[["X"]]))
        stop("Source and target set covariates must have the same number of features!")
    
    if (nrow(source_set[["X"]]) != length(source_set[["y"]]))
        stop("Source sets must have the same number of samples!")
    
    out01 <- function(y) (min(y) < 0) & (max(y) > 1)
    if (out01(target_set[["y"]]) | out01(source_set[["y"]]))
        stop("Response data must be normalized in [0, 1].")
    
    
    ## Load function files...
    source("lambda_functions.R")
    source("get_dist_est.R")
    source("dist_match.R")
    source("RF_predict.R")
    
    
    ######## MAIN ##############################################################
    
    ## Define datasets...
    X1 <- norm_data(target_set[["X"]]);      y1 <- target_set[["y"]]
    X2 <- norm_data(source_set[["X"]]);      y2 <- source_set[["y"]]
    n_feat <- ncol(X1);                      data_lims <- c(0, 1)
    
    
    ## Distribution matching for predictors...
    X2_map <- lapply(1:n_feat, function(j) { 
        dist_match(X1[, j], ref = X2[, j], match_method = method, samp_size = size, lims = data_lims, rand_seed = seed)
        })
    X2_map <- as.data.frame(X2_map);    dimnames(X2_map) <- dimnames(X1)
    # rownames(X2_map) <- rownames(X1);   colnames(X2_map) <- colnames(X1)
    
    
    ## Perform prediction & map back to original space...
    y2_pred_map <- RF_predict(x_train = X2, y_train = y2, x_test = X2_map, y_lims = data_lims, 
                              n_tree = 200, m_try = 0.4, random_seed = seed)
    y2_cdf  <- get_dist_est(y2, sample_size = size, x_range = "unit", dist_method = method, grid_size = 1e3, random_seed = seed)
    
    y1_pred <- dist_match(y2_pred_map, ref = y1, src_dist = y2_cdf, match_method = method, samp_size = size, 
                          lims = data_lims, rand_seed = seed)
    y1_pred <- confined(y1_pred, lims = data_lims);      names(y1_pred) <- names(y2_pred_map)
    
    
    ## Return output objects...
    if (pred_all) {
        return( list("mapped" = y1_pred, "unmapped" = y2_pred_map) )
    } else {
        return( y1_pred )
    }

}