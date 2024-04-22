#### Survival function ####

#' Survival function
#'
#' @description
#' Compute the survival function of the extended Chen-Poisson (ecp)
#' distribution.
#'
#' @param q vector of quantiles.
#'
#' @param lambda,gamma  parameter values > 0.
#'
#' @param phi parameter value > 0 or < 0.
#'
#' @param lower_tail similar to lower.tail
#'
#' @param cum_haz logical value
#'
#' @return Numeric value of the survival function.
#'
#' @return If lower_tail = TRUE, numeric value of the distribution function.
#'
#' @return If cum_haz = TRUE, numeric value of the cumulative hazard function.
#'
#' @examples
#' secp(2, 1, 1, 1, lower_tail = FALSE, cum_haz = FALSE) # survival function
#' secp(2, 1, 1, 1, lower_tail = TRUE, cum_haz = FALSE) # distribution function
#' secp(2, 1, 1, 1, lower_tail = FALSE, cum_haz = TRUE) # cumulative
#' # hazard function
#'
#' @export
secp <- function(q, lambda, gamma, phi, lower_tail = FALSE,
                 cum_haz = FALSE) {
  # Check if arguments are numeric
  if (!all(sapply(list(q, lambda, gamma, phi), is.numeric))) {
    stop("non-numeric argument")
  }

  # Check for invalid arguments
  if (any(q < 0) || min(lambda <= 0) || min(gamma <= 0) ||
        min(phi == 0)) {
    stop("Invalid arguments")
  }

  # Calculate survival function
  sf <- (1 - exp(-phi * exp(lambda * (1 - exp(q^gamma))))) /
    (1 - exp(-phi))

  # Adjust survival function if lower_tail is TRUE
  if (lower_tail)
    sf <- 1 - sf

  # Calculate cumulative hazard function if cum_haz is TRUE

  if (cum_haz)
    sf <- -log(sf)

  return(sf)

}
