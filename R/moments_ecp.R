#### The k-th raw moment of extended Chen-Poisson distribution ####

#' The k-th raw moment of extended Chen-Poisson distribution
#'
#' @description
#' Computes the k-th raw moment of the extended Chen-Poisson (ecp) distribution.
#'
#' @param k a positive integer.
#'
#' @param lambda,gamma  parameter values > 0.
#'
#' @param phi parameter value != 0.
#'
#' @return Estimated value of k-th raw moment, based on numerical integration,
#' as well as integral absolute error obtained from the function integrate.
#'
#' @details
#' To obtain the value of k-th raw moment of the Extended
#' Chen-Poisson distribution, it is necessary to use numerical integration. For
#' that purpose, the R function integrate can be used, which returns the
#' estimated value of the integral and also the integral absolute error
#' (see details about function integrate). Therefore, to obtain the variance,
#' the first component of each k-th raw moments must be selected.
#'
#' @examples
#' ecp_kmoment(k = 1, lambda = .1, gamma = .5, phi = - .2) # First raw moment.
#' ecp_kmoment(k = 2, lambda = .1, gamma = .5, phi = - .2) # Second raw moment.
#' ecp_kmoment(k = 2, lambda = .1, gamma = .5, phi = - .2)[1] -
#' ecp_kmoment(k = 1, lambda = .1, gamma = .5, phi = - .2)[1]^2 # Variance.
#'
#' @export
#'
ecp_kmoment <- function(k, lambda, gamma, phi) {

  # Check if arguments are numeric
  if (!all(sapply(list(k, lambda, gamma, phi), is.numeric))) {
    stop("non-numeric argument")
  }

  # Check if k is a positive integer
  if (!is.numeric(k) || k != as.integer(k) || k < 1) {
    stop("Parameter k must be a positive integer (1, 2, 3, ...).")
  }

  # Check for invalid arguments
  if (min(lambda <= 0) || min(gamma <= 0) ||
        phi == 0) {
    stop("Invalid arguments")
  }

  # Compute k-th raw moment

  func <- function(y) {
    (phi * exp(- phi * y) * (log(1 - lambda^(-1) * log(y))) ^ (k / gamma)) /
      (1 - exp(- phi))
  }
  integral <- stats::integrate(Vectorize(func), lower = 0, upper = 1)
  arr <- array(c(integral$value, integral$abs.error), dim = c(1, 2))
  dimnames(arr) <- list("", c("estimate ", "integral abs. error <"))
  return(arr)
}
