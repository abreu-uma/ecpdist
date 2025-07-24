#### Function to generate a pseudo-random sample of the Chen distribution ####

#' Function to generate a pseudo-random sample of the Chen distribution
#'
#' @description
#' Generate a pseudo-random sample, without censoring, from the Chen
#' distribution.
#'
#' @param n sample size.
#'
#' @param lambda,gamma  parameter values > 0.
#'
#' @return A vector of randomly generated numbers from the
#' extended Chen-Poisson distribution.
#'
#' @examples
#' rchen(10,1,1) # random sample of size 10
#'
#' @export
rchen <- function(n, lambda, gamma) {

  # Check if arguments are numeric

  if (!all(sapply(list(n, lambda, gamma), is.numeric))) {
    stop("non-numeric argument")
  }

  # Check if arguments are valid

  if (any(c(n, lambda, gamma) <= 0)) {
    stop("Invalid arguments")
  }

  # Generate pseudo-random sample

  rd <- (log(1 - log(1 - stats::runif(n)) / lambda))^(1 / gamma)

  return(rd)
}
