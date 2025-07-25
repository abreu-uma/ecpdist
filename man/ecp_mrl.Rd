% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mrl_ecp.R
\name{ecp_mrl}
\alias{ecp_mrl}
\title{Mean residual life function of the ecp distribution}
\usage{
ecp_mrl(x, lambda, gamma, phi)
}
\arguments{
\item{x}{vector of quantiles.}

\item{lambda, gamma}{parameter values > 0.}

\item{phi}{parameter value != 0.}
}
\value{
Estimated value of mean residual life function, based on numerical
integration.
}
\description{
Computes the mean residual life function of the extended Chen-Poisson (ecp)
distribution.
}
\examples{
ecp_mrl(x = 5, lambda = .1, gamma = .5, phi = - .2)

}
