#### Mean residual life function ####

#' Mean residual life function
#'
#' @description
#' Computes the mean residual life function of the extended Chen-Poisson (ecp)
#' distribution.
#'
#'@param x vector of quantiles.
#'
#' @param lambda,gamma  parameter values > 0.
#'
#' @param phi parameter value != 0.
#'
#' @return Estimated value of mean residual life function, based on numerical
#' integration.
#'
#' @examples
#' ecp_mrl(x = 5, lambda = .1, gamma = .5, phi = - .2)
#'
#' @export
#'
ecp_mrl <- function(x, lambda, gamma, phi) {

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
    exp(-phi * y) * (log(1 - lambda^(-1) * log(y)))^(1 / gamma)
  }

  # Apply the integration for each element of the vector x
  int_results <- sapply(x, function(xi) {
    result <- integrate(Vectorize(func), lower = 0,
                        upper = exp(lambda * (1 - exp(xi^gamma))))
    c(value = result$value, abs.error = result$abs.error)
  })

  # Compute mean residual life function for each element in x
  totalfunc <- sapply(seq_along(x), function(i) {
    (phi * int_results[1, i]) /
      (1 - exp(-phi * exp(lambda * (1 - exp(x[i]^gamma))))) - x[i]
  })

  # Prepare the output array
  arr <- array(c(totalfunc, int_results[2, ]),
               dim = c(length(x), 2))
  dimnames(arr) <- list(NULL, c("estimate", "integral abs. error <"))

  # The arr array now contains the final results
  arr

}
