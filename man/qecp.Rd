% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/qecp.R
\name{qecp}
\alias{qecp}
\title{Quantile function of the ecp distribution}
\usage{
qecp(p, lambda, gamma, phi, lower_tail = TRUE, log_p = FALSE)
}
\arguments{
\item{p}{vector of probabilities.}

\item{lambda, gamma}{parameter values > 0.}

\item{phi}{parameter value != 0.}

\item{lower_tail}{similar to lower.tail.}

\item{log_p}{logical value.}
}
\value{
Numeric value of the quantile function.

If lower_tail = FALSE, numeric value of
the logarithm of the function.

If log_p = TRUE, numeric value of the logarithm of the function.
}
\description{
Compute the quantile function of the extended Chen-Poisson (ecp)
distribution.
}
\examples{
qecp(0.5, 2, 1, 1, lower_tail = TRUE, log_p = FALSE)
# quantile function

}
