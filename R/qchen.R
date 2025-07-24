#### Quantile function of the Chen distribution ####

#' Quantile function of the Chen distribution
#'
#' @description
#' Compute the quantile function of the Chen distribution.
#'
#' @param p vector of probabilities.
#'
#' @param lambda,gamma  parameter values > 0.
#'
#' @param lower_tail similar to lower.tail.
#'
#' @param log_p  logical value.
#'
#' @return Numeric value of the quantile function.
#'
#' @return If lower_tail = FALSE, numeric value of
#' the logarithm of the function.
#'
#' @return If log_p = TRUE, numeric value of the logarithm of the function.
#'
#' @examples
#' qchen(0.5, 2, 1, lower_tail = TRUE, log_p = FALSE) # quantile function
#'
#' @export
qchen <- function(p, lambda, gamma, lower_tail = TRUE, log_p = FALSE) {

  # Check if arguments are numeric

  if (!all(sapply(list(p, lambda, gamma), is.numeric))) {
    stop("non-numeric argument")
  }

  # Check for invalid arguments

  if (any(p < 0, p > 1) || min(lambda <= 0) ||  min(gamma <= 0)) {
    stop("Invalid arguments")
  }

  # Compute qf

  qf <- (log(1 - log(1 - p) / lambda))^(1 / gamma)

  # Adjust qf based on lower_tail and log_p

  if (!lower_tail)
    p <- 1 - p
  if (log_p)
    qf <- log(qf)
  return(qf)
}
