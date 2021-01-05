#' Predictive Modeling using Random Forest Regression
#'
#' This function trains a Random Forest regressor using the training data
#' provided and predict response for the test features. Depends on the
#' `randomForest` package.
#'
#' @param x_train Training features for designing the RF regressor.
#' @param y_train Training response for designing the RF regressor.
#' @param x_test Test features for which response values are to be predicted.
#' @param lims Vector providing the range of the response values for modeling.
#' If missing, these values are estimated from the training response.
#' @param n_tree Number of decision trees to be built in the forest. Defaults
#' to `300`.
#' @param m_try Fraction of the features to be used for building each tree.
#' Defaults to `0.3333` (or 33.33%).
#' @param seed Seed for random number generator (for reproducible outcomes).
#' Defaults to `NULL`.
#' @param ... Other options relevant for RF modeling.
#'
#' @return Predicted values for `x_test` using the RF regressor.
#'
#' @keywords random-forest decision-tree
#' @export
#'

## Dependency: stats, randomForest
## Dependency_own: lambda_functions
##
## Author: SR Dhruba, Dec 2020
################################################################################

RF_predict <- function(x_train, y_train, x_test, lims, n_tree = 300, m_try = 0.3333, seed = NULL, ...) {

    if (missing(lims))
        lims <- range(y_train)

    if (m_try > 1 | m_try < 0)
        stop("Invalid value! Please choose a value between 0 and 1 (fraction of the features)!")


    ## Define model & perform prediction...
    m_try  <- round(m_try * ncol(x_train))

    set.seed(seed)                       # For reproducibility
    Forest <- randomForest::randomForest(x = x_train, y = y_train, ntree = n_tree, mtry = m_try, replace = TRUE, ...)
    y_pred <- confined(stats::predict(Forest, x_test), lims)

    y_pred

}


################################################################################

## Dependency: stats, e1071
## Dependency_own: lambda_functions
##
## Author: SR Dhruba, Dec 2020
################################################################################

SVM_predict <- function(x_train, y_train, x_test, lims,
                        n_tree = 300, m_try = 0.3333, seed = NULL, ...) {

    if (missing(lims))
        lims <- range(y_train)

    if (m_try > 1 | m_try < 0)
        stop("Invalid value! Please choose a value between 0 and 1 (fraction of the features)!")


    ## Define model & perform prediction...
    m_try  <- round(m_try * ncol(x_train))

    set.seed(seed)                       # For reproducibility
    Forest <- randomForest::randomForest(x = x_train, y = y_train, ntree = n_tree, mtry = m_try, replace = TRUE, ...)
    y_pred <- confined(stats::predict(Forest, x_test), lims)

    y_pred

}
