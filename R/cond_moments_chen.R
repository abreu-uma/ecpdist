#### The conditional k-th moment of the Chen distribution ####

#' The conditional k-th moment of the Chen distribution
#'
#' @description
#' Computes the conditional k-th moment of the Chen
#' distribution.
#'
#' @param x vector of quantiles.
#'
#' @param k a positive integer.
#'
#' @param lambda,gamma  parameter values > 0.
#'
#' @return Estimated value of conditional k-th moment, based on numerical
#' integration, using the function integrate.
#'
#' @details
#' The conditional k-th moment of a distribution is given by E(X^k | X > x). To
#' obtain this value for the Chen distribution, it is necessary to use numerical
#' integration. For that purpose, the R function 'integrate()'can be used (see
#' details about function 'integrate()'). Note that when k = 1 and x = 0, the
#' conditional moment is equal to the expected value, i.e., E(X | X > 0) = E(X).
#'
#' @examples
#' chen_kmoment_cond(x = 0, k = 1, lambda = .1, gamma = .5)
#'
#' @export
#'
chen_kmoment_cond <- function(x, k, lambda, gamma) {

  # Check if arguments are numeric

  if (!all(sapply(list(x, lambda, gamma), is.numeric))) {
    stop("non-numeric argument")
  }

  # Check for invalid arguments

  if ((min(x) < 0) || min(lambda <= 0) || min(gamma <= 0)) {
    stop("Invalid arguments")
  }

  # Define the function to integrate

  func <- function(y) {
    y^k * dchen(x = y, lambda, gamma)
  }

  # Apply the integration for each element of the vector x

  int_results <- sapply(x, function(xi) {
    result <- stats::integrate(Vectorize(func), lower = xi, upper = Inf)
    c(value = result$value, abs.error = result$abs.error)
  })

  # Compute conditional k-th raw moment for each element in x

  totalfunc <- sapply(seq_along(x), function(i) {
    int_results[1, i] / schen(q = x[i], lambda, gamma)
  })

  # Prepare the output array with x as row names

  arr <- array(c(totalfunc, int_results[2, ]), dim = c(length(x), 2))
  dimnames(arr) <- list(as.character(x),
                        c("estimate ", "integral abs. error <"))

  # Add a label "x" as a column header for row names

  colnames(arr) <- c("estimate", "  integral abs. error <")
  rownames(arr) <- paste0("x = ", rownames(arr), "   ")

  # The arr array now contains the final results

  arr
}
