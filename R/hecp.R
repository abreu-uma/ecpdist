#### Hazard function ####

#' Hazard function
#'
#' @description
#' Compute the hazard function of the extended Chen-Poisson (ecp) distribution.
#'
#' @param x vector of quantiles.
#'
#' @param lambda,gamma  parameter values > 0.
#'
#' @param phi parameter value > 0 or < 0.
#'
#' @param log logical value
#'
#' @return Numeric value of the hazard function.
#'
#' @return If log = TRUE, numeric value of the logarithm of the function.
#'
#' @examples
#' hecp(2, 1, 1, 1, log = FALSE) # hazard function
#'
#' @export
hecp <- function(x, lambda, gamma, phi, log = FALSE) {

  # Check if arguments are numeric
  if (!all(sapply(list(x, lambda, gamma, phi), is.numeric))) {
    stop("non-numeric argument")
  }

  # Check for invalid arguments
  if (any(x < 0) || min(lambda <= 0) || min(gamma <= 0) ||
        min(phi == 0)) {
    stop("Invalid arguments")
  }

  # Compute the hazard function

  hf <- (lambda * gamma * phi * x^(gamma - 1) *
           exp(x^gamma + lambda * (1 - exp(x^gamma)))) /
    (exp(phi * exp(lambda * (1 - exp(x^gamma)))) - 1)

  # Convert hazard function to log scale if log is TRUE
  if (log)
    hf <- log(hf)

  return(hf)

}
