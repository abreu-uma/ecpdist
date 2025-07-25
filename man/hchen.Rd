% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hchen.R
\name{hchen}
\alias{hchen}
\title{Hazard function of the Chen distribution}
\usage{
hchen(x, lambda, gamma, log = FALSE)
}
\arguments{
\item{x}{vector of quantiles.}

\item{lambda, gamma}{parameter values > 0.}

\item{log}{logical value}
}
\value{
Numeric value of the hazard function.

If log = TRUE, numeric value of the logarithm of the function.
}
\description{
Compute the hazard function of the Chen distribution.
}
\examples{
hchen(2, 1, 1, log = FALSE) # hazard function

}
