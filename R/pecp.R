#### Distribution function ####

#' Distribution function
#'
#' @description
#' Compute the extended Chen-Poisson (ecp) distribution function.
#'
#' @param q vector of quantiles.
#'
#' @param lambda,gamma  parameter values > 0.
#'
#' @param phi parameter value > 0 or < 0.
#'
#' @param lower_tail similar to lower.tail
#'
#' @param log_p logical value
#'
#' @return Numeric value of the distribution function.
#'
#' @return If lower_tail = FALSE, numeric value of the survival function.
#'
#' @return If log_p = TRUE, numeric value of the logarithm of the function.
#'
#' @examples
#' pecp(2, 1, 1, 1, lower_tail = TRUE, log_p = FALSE) # distribution function
#' pecp(2, 1, 1, 1, lower_tail = FALSE, log_p = FALSE) # survival function
#'
#' @export
pecp <- function(q, lambda, gamma, phi, lower_tail = TRUE,
                 log_p = FALSE) {
  # Check if arguments are numeric
  if (!all(sapply(list(q, lambda, gamma, phi), is.numeric))) {
    stop("non-numeric argument")
  }

  # Check for invalid arguments
  if (any(q < 0) || min(lambda <= 0) || min(gamma <= 0) ||
        min(phi == 0)) {
    stop("Invalid arguments")
  }

  # Calculate CDF #
  cdf <- 1 -  (1 - exp(-phi * exp(lambda * (1 - exp(q^gamma))))) /
    (1 - exp(-phi))

  # Adjust CDF if lower_tail is FALSE
  if (!lower_tail) {
    cdf <- 1 - cdf
  }

  # Convert CDF to log scale if log_p is TRUE
  if (log_p) {
    cdf <- log(cdf)
  }

  return(cdf)
}
