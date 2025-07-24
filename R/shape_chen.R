#### Bowley skewness and Moors kurtosis of the Chen distribution ####

#' Bowley skewness and Moors kurtosis of the Chen distribution
#'
#' @description
#' Computes robust skewness and kurtosis measures based on quantile function of
#' the Chen distribution.
#'
#' @param lambda,gamma  parameter values > 0.
#'
#' @param measure Specifies the type of measure to be computed. Possible
#' types are:
#'
#'  - 'bowley' for Bowley skewness,
#'  - 'moors' for Moors kurtosis.
#'
#' @return Numeric value of the Bowley skewness or the Moors kurtosis.
#'
#' @details
#' The Chen distribution has no closed-form expression for the moments.
#' Therefore, the classical measures of skewness and kurtosis based on moments
#' are intractable. In such cases, quantile-based measures are often considered,
#' namely the Bowley skewness and Moors kurtosis.
#'
#' @examples
#' chen_shape(2, 0.3, measure = 'bowley') # Bowley skewness
#'
#' @export
#'
chen_shape <- function(lambda, gamma, measure) {

  # Check if arguments are numeric

  if (!all(sapply(list(lambda, gamma), is.numeric))) {
    stop("non-numeric argument")
  }

  # Check for invalid arguments

  if (min(lambda <= 0) || min(gamma <= 0)) {
    stop("Invalid arguments")
  }

  # Compute skewness or kurtosis measure

  q <- qchen(p = seq(1 / 8, 7 / 8, 1 / 8), lambda, gamma)
  func <- switch(measure, bowley = (q[2] - 2 * q[4] + q[6]) / (q[6] - q[2]),
                 moors = (q[7] - q[5] - q[3] + q[1]) / (q[6] - q[2]),
                 stop("Invalid measure type. Use 'bowley' or 'moors'."))
  return(func)
}
