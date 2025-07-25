% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cond_moments_chen.R
\name{chen_kmoment_cond}
\alias{chen_kmoment_cond}
\title{The conditional k-th moment of the Chen distribution}
\usage{
chen_kmoment_cond(x, k, lambda, gamma)
}
\arguments{
\item{x}{vector of quantiles.}

\item{k}{a positive integer.}

\item{lambda, gamma}{parameter values > 0.}
}
\value{
Estimated value of conditional k-th moment, based on numerical
integration, using the function integrate.
}
\description{
Computes the conditional k-th moment of the Chen
distribution.
}
\details{
The conditional k-th moment of a distribution is given by E(X^k | X > x). To
obtain this value for the Chen distribution, it is necessary to use numerical
integration. For that purpose, the R function 'integrate()'can be used (see
details about function 'integrate()'). Note that when k = 1 and x = 0, the
conditional moment is equal to the expected value, i.e., E(X | X > 0) = E(X).
}
\examples{
chen_kmoment_cond(x = 0, k = 1, lambda = .1, gamma = .5)

}
