% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/estimate_cdf.R
\name{estimate_cdf}
\alias{estimate_cdf}
\title{Estimate Cumulative Distribution}
\usage{
estimate_cdf(
  x,
  bootstrap = TRUE,
  samples = 1e+06,
  density = FALSE,
  binned = TRUE,
  grids = 10000,
  unit_range = FALSE,
  seed = NULL,
  ...
)
}
\arguments{
\item{x}{Vector containing data.}

\item{bootstrap}{Flag for performing bootstrapping on \code{x} to get a better
estimate of the CDF. Defaults to \code{TRUE}.}

\item{samples}{Sample size for bootstrapping. Defaults to \code{1e6}. Ignored
when \code{bootstrap = FALSE}.}

\item{density}{Flag for calculating kernel density estimates (KDE) instead
of histogram counts. Depends on the \code{ks} package for density estimation.
Defaults to \code{FALSE}.}

\item{binned}{Flag for calculating binned KDE. Defaults to \code{TRUE}. Ignored
when \code{density = FALSE}.}

\item{grids}{Size parameter for the estimation grid when \code{density = TRUE}.
Used to calculate the grid sizes for KDE bandwidth estimation (\code{grids*10}),
and grid size KDE estimation (\code{bgridsize = grids} if \code{binned = TRUE} else
\code{gridsize = grids/10}). Defaults to \code{1e4}.}

\item{unit_range}{Flag for unity data range (\emph{i.e.}, data is normalized
between 0 and 1). Defaults to \code{FALSE}.}

\item{seed}{Seed for random number generator (for reproducible outcomes).
Defaults to \code{NULL}.}

\item{...}{Other options relevant for distribution estimation.}
}
\value{
If \code{density = FALSE}, a function of class \code{ecdf}, inheriting from the
\code{stepfun} class, and hence inheriting a \code{knots()} method.

If \code{density = TRUE}, an object of class \code{kcde} which has the fields
\code{eval.points} and \code{estimate} necessary for calculating a map.
}
\description{
This function estimates the values of the cumulative distribution function
(CDF) for a vector.
}
\examples{
x <- runif(100)
x_hist_cdf <- estimate_cdf(x, samples = 1000, unit_range = TRUE)
x_kde_cdf <- estimate_cdf(x, density = TRUE, unit_range = TRUE)

}
\keyword{CDF}
\keyword{KDE}
\keyword{cumulative-distribution}
\keyword{cumulative-histogram}
\keyword{kernel-CDF}
\keyword{kernel-density}
\keyword{kernel-density-estimate}
