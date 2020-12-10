## Perform predictive modeling using Random Forest regression 
## 
## Dependency: randomForest 
## Dependency_own: lambda_functions 
################################################################################

RF_predict <- function(x_train, y_train, x_test, y_lims, n_tree = 300, m_try = 0.3333, random_seed = NULL, ...) {
    
    source("lambda_functions.R")
    
    ## Initital checks...
    # if (!require(randomForest))                 # Load package
    #     library(randomForest)
    
    if (missing(y_lims))
        y_lims <- c(min(y_train), max(y_train))
    
    if (m_try > 1 | m_try < 0)
        stop("Invalid value! Please choose a value between 0 and 1 (fraction of the features)!")
    
    
    ## Define model & perform prediction...
    set.seed(random_seed)                       # For reproducibility
    
    m_try  <- round(m_try * ncol(x_train))
    Forest <- randomForest::randomForest(x = x_train, y = y_train, ntree = n_tree, mtry = m_try, replace = TRUE, ...)
    
    y_pred <- confined(predict(Forest, x_test), lims = y_lims)
    y_pred
    
}