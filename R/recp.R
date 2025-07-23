#### Function to generate a pseudo-random sample of the ecp distribution ####

#' Function to generate a pseudo-random sample of the ecp distribution
#'
#' @description
#' Generate a pseudo-random sample, without censoring, from the
#' extended Chen-Poisson (ecp) distribution.
#'
#' @param n sample size.
#'
#' @param lambda,gamma  parameter values > 0.
#'
#' @param phi parameter value != 0.
#'
#' @return A vector of randomly generated numbers from the
#' extended Chen-Poisson distribution.
#'
#' @examples
#' recp(10,1,1,1) # random sample of size 10
#'
#' @export
recp <- function(n, lambda, gamma, phi) {

  # Check if arguments are numeric

  if (!all(sapply(list(n, lambda, gamma, phi), is.numeric))) {
    stop("non-numeric argument")
  }

  # Check if arguments are valid

  if (any(c(n, lambda, gamma) <= 0) || phi == 0) {
    stop("Invalid arguments")
  }

  # Generate pseudo-random sample

  rd <- (log(1 - lambda^(-1) *
               log(1 - phi^(-1) * log(1 + (exp(phi) - 1) *
                                        stats::runif(n)))))^(1 / gamma)

  return(rd)
}
