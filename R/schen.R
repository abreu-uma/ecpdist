#### Survival function of the Chen distribution ####

#' Survival function of the Chen distribution
#'
#' @description
#' Compute the survival function of the Chen distribution.
#'
#' @param q vector of quantiles.
#'
#' @param lambda,gamma  parameter values > 0.
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
#'
#' schen(2, 1, 1, lower_tail = FALSE, cum_haz = FALSE) # survival function
#' schen(2, 1, 1, lower_tail = TRUE, cum_haz = FALSE) # distribution function
#' schen(2, 1, 1, lower_tail = FALSE, cum_haz = TRUE) # cumulative hazard
#' # function
#'
#' @export
schen <- function(q, lambda, gamma, lower_tail = FALSE, cum_haz = FALSE) {

  # Check if arguments are numeric

  if (!all(sapply(list(q, lambda, gamma), is.numeric))) {
    stop("non-numeric argument")
  }

  # Check for invalid arguments

  if (any(q < 0) || min(lambda <= 0) || min(gamma <= 0)) {
    stop("Invalid arguments")
  }

  # Compute survival function

  sf <- exp(lambda * (1 - exp(q^gamma)))

  # Adjust survival function if lower_tail is TRUE

  if (lower_tail)
    sf <- 1 - sf

  # Compute cumulative hazard function if cum_haz is TRUE

  if (cum_haz)
    sf <- -log(sf)

  return(sf)

}
