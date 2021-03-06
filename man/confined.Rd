% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lambda_functions.R
\name{confined}
\alias{confined}
\title{Restrict data in a given interval}
\usage{
confined(x, lims = c(0, 1))
}
\arguments{
\item{x}{Vector containing data.}

\item{lims}{Limit for the values. Values falling within this limit will pass
without any change. Any value \code{x < lims[1]} will get replaced by \code{lims[1]},
and any value \code{x > lims[2]} will get replaced by \code{lims[2]}. Defaults to
\code{c(0, 1)}.}
}
\value{
The filtered vector.
}
\description{
This function filters a data vector using a given interval so that only the
values falling inside the interval remains and any value that is less than
the leftmost end gets replaced by that end-value, and similarly, any value
greater than the rightmost end gets replaced by that end-value.
}
\examples{
x <- rnorm(100, 0, 1)
x_filt <- confined(x, lims = c(-0.5, 0.5))
print(range(x_filt))

}
\keyword{filter}
