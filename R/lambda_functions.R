## Contains the anonymous functions for various usage 
## 
## Dependency: base 
################################################################################

## Normalize in [0, 1]...
norm01 <- function(z) { 
  if (min(z) != 0) { z <- z - min(z) }
  z <- z / max(z)
  z
}

## Normalize dataframe (or matrix) columns in [0, 1]...
norm_data <- function(Z) as.data.frame(apply(Z, MARGIN = 2, FUN = norm01))

## Standardize dataframe (or matrix) columns...
zscore <- function(Z) as.data.frame(apply(Z, MARGIN = 2, FUN = scale))

## Restrict data in [0, 1]...
confined <- function(y, lims = c(0, 1)) {
  y[y < lims[1]] <- lims[1];      y[y > lims[2]] <- lims[2]
  y
}

