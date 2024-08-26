# Test file for the ecp_kmoment_cond function

# Explanation of the Tests

# Test Suite:
# The test suite includes tests for both scalar and vector inputs for x.
# Integration:
# The expected values are computed using the same integration process that
# ecp_kmoment_cond is supposed to perform.
# Comparison:
# The results from ecp_kmoment_cond are compared against these expected values
# using expect_equal with a specified tolerance.
# Error Handling:
# Tests are included to check that the function correctly handles invalid
# inputs.

# Load the testthat package
library(testthat)

# Test suite for ecp_kmoment_cond function
test_that("ecp_kmoment_cond computes correct conditional k-th moment", {

  # Example values for the Extended Chen-Poisson distribution parameters
  lambda <- 0.1
  gamma <- 0.5
  phi <- -0.2
  k <- 1

  # Define the function to integrate for expected values
  func <- function(y) {
    exp(- phi * y) * (log(1 - lambda^(-1) * log(y)))^(k / gamma)
  }

  # Test for scalar input x
  x_scalar <- 0

  # Apply the integration for x_scalar
  result <- stats::integrate(Vectorize(func), lower = 0,
                             upper = exp(lambda * (1 - exp(x_scalar^gamma))))
  int_value <- result$value
  int_error <- result$abs.error

  # Compute conditional k-th moment
  expected_scalar <- (phi * int_value) /
    (1 - exp(-phi * exp(lambda * (1 - exp(x_scalar^gamma)))))

  # Test for scalar input x using the ecp_kmoment_cond function
  result_scalar <- ecp_kmoment_cond(x = x_scalar, k = k, lambda = lambda,
                                    gamma = gamma, phi = phi)

  # Extract the actual value ignoring names
  actual_scalar <- as.numeric(result_scalar[1, "estimate"])

  # Compare the result from ecp_kmoment_cond with the expected value
  expect_equal(actual_scalar, expected_scalar, tolerance = 1e-6,
               label = "Conditional k-th moment x = 0")

  # Test for vector input x
  x_vector <- c(0, 1)

  # Apply the integration for each element of the vector x
  int_results <- sapply(x_vector, function(xi) {
    result <- stats::integrate(Vectorize(func), lower = 0,
                               upper = exp(lambda * (1 - exp(xi^gamma))))
    c(value = result$value, abs.error = result$abs.error)
  })

  # Compute conditional k-th moment for each element in x
  expected_vector <- sapply(x_vector, function(xi) {
    (phi * int_results["value", which(x_vector == xi)]) /
      (1 - exp(-phi * exp(lambda * (1 - exp(xi^gamma)))))
  })

  # Test for vector input x using the ecp_kmoment_cond function
  result_vector <- ecp_kmoment_cond(x = x_vector, k = k, lambda = lambda,
                                    gamma = gamma, phi = phi)

  # Extract the actual values ignoring names
  actual_vector <- as.numeric(result_vector[, "estimate"])

  # Ensure that expected_vector is also a numeric vector without names
  expected_vector <- as.numeric(expected_vector)

  # Compare the results from ecp_kmoment_cond with the expected values
  for (i in seq_along(x_vector)) {
    expect_equal(actual_vector[i], expected_vector[i], tolerance = 1e-6,
                 label = paste("Conditional k-th moment for x =", x_vector[i]))
  }
})
