#### The conditional k-th raw moment of extended Chen-Poisson distribution ####

#' The conditional k-th raw moment of extended Chen-Poisson distribution
#'
#' @description
#' Computes the conditional k-th raw moment of the extended Chen-Poisson (ecp)
#' distribution.
#'
#' @param x vector of quantiles.
#'
#' @param k a positive integer.
#'
#' @param lambda,gamma  parameter values > 0.
#'
#' @param phi parameter value != 0.
#'
#' @return Estimated value of conditional k-th raw moment, based on numerical
#' integration, using the function integrate.
#'
#' @details
#' To obtain the value of conditional k-th raw moment of the Extended
#' Chen-Poisson distribution, it is necessary to use numerical integration. For
#' that purpose, the R function integrate can be used (see details about
#' function integrate).
#'
#' @examples
#' # First conditional moment for x = 0. Note that, in this case, E(X|X>0)=E(X).
#' ecp_kmoment_cond(x = 0, k = 1, lambda = .1, gamma = .5, phi = - .2)
#'
#' @export
#'
ecp_kmoment_cond <- function(x, k, lambda, gamma, phi) {
  # Check if arguments are numeric
  if (!all(sapply(list(x, lambda, gamma, phi), is.numeric))) {
    stop("non-numeric argument")
  }

  # Check for invalid arguments
  if ((min(x) < 0) || min(lambda <= 0) || min(gamma <= 0) || phi == 0) {
    stop("Invalid arguments")
  }

  # Define the function to integrate
  func <- function(y) {
    exp(- phi * y) * (log(1 - lambda^(- 1) * log(y)))^(k / gamma)
  }

  # Estimate the integral
  integral <- stats::integrate(Vectorize(func), lower = 0, upper =
                                 exp(lambda * (1 - exp(x^gamma))))

  # Compute conditional k-th raw moment
  totalfunc <- (phi * integral$value) /
    (1 - exp(- phi * exp(lambda * (1 - exp(x^gamma)))))
  arr <- array(c(totalfunc, integral$abs.error), dim = c(1, 2))
  dimnames(arr) <- list("", c("estimate ", "integral abs. error <"))
  return(arr)
}
