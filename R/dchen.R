#### Density function of the Chen distribution ####

#' Density function of the Chen distribution
#'
#' @description
#' Compute the density function of the Chen distribution.
#'
#' @param x vector of quantiles.
#'
#' @param lambda,gamma  parameter values > 0.
#'
#' @param log logical value
#'
#' @return Numeric value of the density function.
#'
#' @return If log = TRUE, numeric value of the logarithm of the function.
#'
#' @examples
#' dchen(2, 1, 1, log = FALSE) # density function
#'
#' @export
dchen <- function(x, lambda, gamma, log = FALSE) {

  # Check if arguments are numeric

  if (!all(sapply(list(x, lambda, gamma), is.numeric))) {
    stop("non-numeric argument")
  }

  # Check for invalid arguments

  if (any(x < 0) || min(lambda <= 0) || min(gamma <= 0)) {
    stop("Invalid arguments")
  }

  # Compute density function

  pdf <- (lambda * gamma * x^(gamma - 1) *
            exp(x^gamma + lambda * (1 - exp(x^gamma))))

  # Convert density function to log scale if log is TRUE

  if (log)
    pdf <- log(pdf)

  return(pdf)

}
