#### Hazard function of the Chen distribution ####

#' Hazard function of the Chen distribution
#'
#' @description
#' Compute the hazard function of the Chen distribution.
#'
#' @param x vector of quantiles.
#'
#' @param lambda,gamma  parameter values > 0.
#'
#' @param log logical value
#'
#' @return Numeric value of the hazard function.
#'
#' @return If log = TRUE, numeric value of the logarithm of the function.
#'
#' @examples
#' hchen(2, 1, 1, log = FALSE) # hazard function
#'
#' @export
hchen <- function(x, lambda, gamma, log = FALSE) {

  # Check if arguments are numeric

  if (!all(sapply(list(x, lambda, gamma), is.numeric))) {
    stop("non-numeric argument")
  }

  # Check for invalid arguments

  if (any(x < 0) || min(lambda <= 0) || min(gamma <= 0)) {
    stop("Invalid arguments")
  }

  # Compute the hazard function

  hf <- (lambda * gamma * x^(gamma - 1) * exp(x^gamma))

  # Convert hazard function to log scale if log is TRUE

  if (log)
    hf <- log(hf)

  return(hf)

}
