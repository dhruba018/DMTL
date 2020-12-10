## Estimate cumulative distributions for a vector to be used in the 
## distribution matching step 
## 
## Dependency: stats, ks 
################################################################################

get_dist_est <- function(x, sample_size = 1e6, dist_method = "hist", size_tol = 1e3, binned = TRUE, 
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
