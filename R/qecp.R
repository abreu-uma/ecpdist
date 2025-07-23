#### Quantile function of the ecp distribution ####

#' Quantile function of the ecp distribution
#'
#' @description
#' Compute the quantile function of the extended Chen-Poisson (ecp)
#' distribution.
#'
#' @param p vector of probabilities.
#'
#' @param lambda,gamma  parameter values > 0.
#'
#' @param phi parameter value != 0.
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
#' qecp(0.5, 2, 1, 1, lower_tail = TRUE, log_p = FALSE)
#' # quantile function
#'
#' @export
qecp <- function(p, lambda, gamma, phi, lower_tail = TRUE, log_p = FALSE) {

  # Check if arguments are numeric

  if (!all(sapply(list(p, lambda, gamma, phi), is.numeric))) {
    stop("non-numeric argument")
  }

  # Check for invalid arguments

  if (any(p < 0, p > 1) || min(lambda <= 0) ||
        min(gamma <= 0) || phi == 0) {
    stop("Invalid arguments")
  }

  # Compute qf

  qf <- (log(1 - lambda^(-1) * log(1 - phi^(-1) *
                                     log(1 + (exp(phi) - 1) * p))))^
    (1 / gamma)

  # Adjust qf based on lower_tail and log_p

  if (!lower_tail)
    p <- 1 - p
  if (log_p)
    qf <- log(qf)
  return(qf)
}
