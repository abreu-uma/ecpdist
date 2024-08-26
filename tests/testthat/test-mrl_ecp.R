# Test file for the ecp_mrl function

# Explanation of the Tests

# The tests will cover both correct behavior with valid inputs (Positive tests)
# and error handling for invalid inputs (negative tests.
# Positive tests:
#  The function is tested with the expression to ensure it produces the correct
# output. For x = 5, the function's output is compared with the value obtained
# by its expression (expected_value).
# The function is also tested with a vector input for x to ensure it handles
# vectors correctly.
# Negative tests:
# Non-numeric inputs: Test that the function returns an error when any of the
# parameters (x, lambda, gamma, phi) are not numeric.
# Invalid x values: Test that the function returns an error when x is negative.
# Invalid lambda, gamma, or phi: Ensure the function returns an error when
# lambda or gamma are less than or equal to zero, or when phi is equal to zero.

# Load the testthat package
library(testthat)

# Test suite for ecp_mrl function
test_that("ecp_mrl computes correct mean residual life function", {

  # Example values for the Extended Chen-Poisson distribution parameters
  lambda <- 0.1
  gamma <- 0.5
  phi <- -0.2

  # Define the function to integrate
  func <- function(y) {
    exp(- phi * y) * (log(1 - lambda^(- 1) * log(y)))^(1 / gamma)
  }

  # Test for scalar input x
  x_scalar <- 5

  # Apply the integration for x_scalar
  result <- integrate(Vectorize(func), lower = 0,
                      upper = exp(lambda * (1 - exp(x_scalar^gamma))))
  int_value <- result$value
  int_error <- result$abs.error

  # Compute mean residual life function
  expected_scalar <- (phi * int_value) /
    (1 - exp(-phi * exp(lambda * (1 - exp(x_scalar^gamma))))) - x_scalar

  # Test for scalar input x using the ecp_mrl function
  result_scalar <- ecp_mrl(x = x_scalar, lambda = lambda, gamma = gamma,
                           phi = phi)

  # Compare the result from ecp_mrl with the expected value
  expect_equal(result_scalar[1], expected_scalar, tolerance = 1e-6,
               label = "Mean residual life function calculation")

  # Test for vector input x
  x <- c(2, 5)

  # Apply the integration for each element of the vector x
  int_results <- sapply(x, function(xi) {
    result <- integrate(Vectorize(func), lower = 0,
                        upper = exp(lambda * (1 - exp(xi^gamma))))
    c(value = result$value, abs.error = result$abs.error)
  })

  # Compute mean residual life function for each element in x
  totalfunc <- sapply(seq_along(x), function(i) {
    (phi * int_results[1, i]) /
      (1 - exp(-phi * exp(lambda * (1 - exp(x[i]^gamma))))) - x[i]
  })
  expected_value_vec1 <- unname(totalfunc[1])
  expected_value_vec2 <- unname(totalfunc[2])

  # Test for vector input x using the ecp_mrl function
  result_vec <- ecp_mrl(x = c(2, 5), lambda = lambda, gamma = gamma,
                        phi = phi)

  # Compare the result from ecp_mrl with the expected value
  expect_equal(result_vec[1], expected_value_vec1, tolerance = 1e-6,
               label = "Mean residual life function for vector x[1]")
  expect_equal(result_vec[2], expected_value_vec2, tolerance = 1e-6,
               label = "Mean residual life function for vector x[2]")
})

test_that("ecp_mrl handles invalid inputs correctly", {

  # Non-numeric x
  expect_error(ecp_mrl(x = "a", lambda = 0.1, gamma = 0.5, phi = -0.2),
               "non-numeric argument")

  # Negative x value
  expect_error(ecp_mrl(x = -1, lambda = 0.1, gamma = 0.5, phi = -0.2),
               "Invalid arguments")

  # Non-numeric lambda
  expect_error(ecp_mrl(x = 5, lambda = "a", gamma = 0.5, phi = -0.2),
               "non-numeric argument")

  # lambda or gamma <= 0, phi = 0
  expect_error(ecp_mrl(x = 5, lambda = 0, gamma = 0.5, phi = -0.2),
               "Invalid arguments")
  expect_error(ecp_mrl(x = 5, lambda = 0.1, gamma = 0, phi = -0.2),
               "Invalid arguments")

  expect_error(ecp_mrl(x = 5, lambda = 0.1, gamma = 0.5, phi = 0),
               "Invalid arguments")
})
