# Test file for the ecp_kmoment function

# Explanation of the Tests

# The following unit tests will verify the function's behavior under various
# scenarios.  It will be checked that:

# The function returns correct values for known cases (Positive tests).
# The function throws appropriate errors when given invalid input (Negative
# tests).

# Positive tests:
# The tests verify that the function produces the expected output (within a
# specified tolerance) for known values of the parameters.

# Negative tests:
# These tests ensure that the function correctly handles invalid inputs, such as
# non-integer k, zero or negative values for lambda or gamma, phi = 0, and
# non-numeric input.

# Load the testthat package
library(testthat)

# Expected values
expected_value_1 <-
  stats::integrate(Vectorize(function(y) {
                                          ((-0.2) * exp(- (- 0.2) * y) *
                                             (log(1 - 0.1^(-1) * log(y))) ^
                                               (1 / 0.5)) /
                                            (1 - exp(- (-0.2)))}), lower = 0,
  upper = 1)$value
expected_value_2 <-
  stats::integrate(Vectorize(function(y) {
                                          ((-0.2) * exp(- (- 0.2) * y) *
                                             (log(1 - 0.1^(-1) * log(y))) ^
                                               (2 / 0.5)) /
                                            (1 - exp(- (-0.2)))}), lower = 0,
  upper = 1)$value
expected_variance <- expected_value_2 - (expected_value_1)^2


# Test suite for ecp_kmoment function
test_that("ecp_kmoment computes correct k-th raw moment", {

  # Test for k = 1 (first raw moment)
  result_1 <- ecp_kmoment(k = 1, lambda = 0.1, gamma = 0.5, phi = -0.2)
  expect_equal(result_1[1], expected_value_1, tolerance = 1e-6,
               label = "First raw moment calculation")

  # Test for k = 2 (second raw moment)
  result_2 <- ecp_kmoment(k = 2, lambda = 0.1, gamma = 0.5, phi = -0.2)
  expect_equal(result_2[1], expected_value_2, tolerance = 1e-6,
               label = "Second raw moment calculation")

  # Test for variance calculation
  variance <- ecp_kmoment(k = 2, lambda = 0.1, gamma = 0.5, phi = -0.2)[1] -
    ecp_kmoment(k = 1, lambda = 0.1, gamma = 0.5, phi = -0.2)[1]^2
  expect_equal(variance, expected_variance, tolerance = 1e-6,
               label = "Variance calculation")
})

test_that("ecp_kmoment handles invalid inputs correctly", {

  # k is not a positive integer
  expect_error(ecp_kmoment(k = 0, lambda = 0.1, gamma = 0.5, phi = -0.2),
               "Parameter k must be a positive integer")

  expect_error(ecp_kmoment(k = -1, lambda = 0.1, gamma = 0.5, phi = -0.2),
               "Parameter k must be a positive integer")

  expect_error(ecp_kmoment(k = 1.5, lambda = 0.1, gamma = 0.5, phi = -0.2),
               "Parameter k must be a positive integer")

  # Non-numeric inputs
  expect_error(ecp_kmoment(k = 1, lambda = "a", gamma = 0.5, phi = -0.2),
               "non-numeric argument")

  # lambda or gamma <= 0, phi = 0
  expect_error(ecp_kmoment(k = 1, lambda = 0, gamma = 0.5, phi = -0.2),
               "Invalid arguments")

  expect_error(ecp_kmoment(k = 1, lambda = 0.1, gamma = 0, phi = -0.2),
               "Invalid arguments")

  expect_error(ecp_kmoment(k = 1, lambda = 0.1, gamma = 0.5, phi = 0),
               "Invalid arguments")
})
