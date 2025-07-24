#### Mean residual life function of the Chen distribution ####

#' Mean residual life function of the Chen distribution
#'
#' @description
#' Computes the mean residual life function of the Chen distribution.
#'
#'@param x vector of quantiles.
#'
#' @param lambda,gamma  parameter values > 0.
#'
#' @return Estimated value of mean residual life function, based on numerical
#' integration.
#'
#' @examples
#' chen_mrl(x = 5, lambda = .1, gamma = .5)
#'
#' @export
#'
chen_mrl <- function(x, lambda, gamma) {

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
    y * dchen(x = y, lambda, gamma)
  }

  # Apply the integration for each element of the vector x

  int_results <- sapply(x, function(xi) {
    result <- stats::integrate(Vectorize(func), lower = 0, upper = Inf)
    c(value = result$value, abs.error = result$abs.error)
  })

  # Compute mean residual life function for each element in x

  totalfunc <- sapply(seq_along(x), function(i) {
    int_results[1, i] / schen(q = x[i], lambda, gamma) - x[i]
  })

  # Prepare the output array with x as row names

  arr <- array(c(totalfunc, int_results[2, ]), dim = c(length(x), 2))
  dimnames(arr) <- list(as.character(x), c("estimate", "integral abs. error <"))

  # Add a label "x" as a column header for row names

  colnames(arr) <- c("estimate", "integral abs. error <")
  rownames(arr) <- paste0("x = ", rownames(arr), "  ")

  # The arr array now contains the final results

  arr

}
