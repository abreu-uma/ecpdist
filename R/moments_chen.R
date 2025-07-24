#### The k-th raw moment of the Chen distribution ####

#' The k-th raw moment of the Chen distribution
#'
#' @description
#' Computes the k-th raw moment of the Chen distribution.
#'
#' @param k a positive integer.
#'
#' @param lambda,gamma  parameter values > 0.
#'
#' @return Estimated value of k-th raw moment, based on numerical integration,
#' as well as integral absolute error obtained from the function integrate.
#'
#' @details
#' To obtain the value of k-th raw moment of the Chen distribution, it is
#' necessary to use numerical integration. For that purpose, the R function
#' 'integrate()' can be used, which returns the estimated value of the integral
#' and also the integral absolute error (see details about function
#' 'integrate()'). Therefore, to obtain the variance, the first component of
#' each k-th raw moments must be selected.
#'
#' @examples
#' chen_kmoment(k = 1, lambda = .1, gamma = .5) # First raw moment.
#' chen_kmoment(k = 2, lambda = .1, gamma = .5) # Second raw moment.
#' chen_kmoment(k = 2, lambda = .1, gamma = .5)[1] -
#' chen_kmoment(k = 1, lambda = .1, gamma = .5)[1]^2 # Variance.
#'
#' @export
#'
chen_kmoment <- function(k, lambda, gamma) {

  # Check if arguments are numeric

  if (!all(sapply(list(k, lambda, gamma), is.numeric))) {
    stop("non-numeric argument")
  }

  # Check if k is a positive integer

  if (!is.numeric(k) || k != as.integer(k) || k < 1) {
    stop("Parameter k must be a positive integer (1, 2, 3, ...).")
  }

  # Check for invalid

  if (min(lambda <= 0) || min(gamma <= 0)) {
    stop("Invalid arguments")
  }

  # Define the function to integrate

  func <- function(y) {
    (lambda * gamma * y^(gamma + k - 1) * exp(y^gamma +
                                                lambda * (1 - exp(y^gamma))))
  }

  # Estimate the integral

  integral <- stats::integrate(Vectorize(func), lower = 0, upper = Inf)

  # Compute k-th raw moment

  arr <- array(c(integral$value, integral$abs.error), dim = c(1, 2))
  dimnames(arr) <- list("", c("estimate ", "integral abs. error <"))
  return(arr)
}
