# Test file for the ecp_shape function

# Explanation of the Tests

# Basic Valid Input:


# Load testthat package for unit testing
library(testthat)

# Define test cases
test_that("ecp_shape returns correct values for valid inputs", {
  # Example values for the Extended Chen-Poisson distribution parameters
  lambda <- 2
  gamma <- 0.3
  phi <- 30

  # Expected values
  q <- qecp(p = seq(1 / 8, 7 / 8, 1 / 8), 2, 0.3, 30)
  expected_bowley <- (q[2] - 2 * q[4] + q[6]) / (q[6] - q[2])
  expected_moors <- (q[7] - q[5] - q[3] + q[1]) / (q[6] - q[2])

  # Test Bowley skewness
  result_bowley <- ecp_shape(lambda, gamma, phi, measure = "bowley")
  expect_equal(result_bowley, expected_bowley, tolerance = 1e-2)

  # Test Moors kurtosis
  result_moors <- ecp_shape(lambda, gamma, phi, measure = "moors")
  expect_equal(result_moors, expected_moors, tolerance = 1e-2)
})

test_that("ecp_shape handles invalid inputs", {
  # Test non-numeric arguments
  expect_error(ecp_shape("a", 0.3, 30, measure = "bowley"),
               "non-numeric argument")
  expect_error(ecp_shape(2, "b", 30, measure = "bowley"),
               "non-numeric argument")
  expect_error(ecp_shape(2, 0.3, "c", measure = "bowley"),
               "non-numeric argument")

  # Test invalid parameter values
  expect_error(ecp_shape(-2, 0.3, 30, measure = "bowley"), "Invalid arguments")
  expect_error(ecp_shape(2, -0.3, 30, measure = "bowley"), "Invalid arguments")
  expect_error(ecp_shape(2, 0.3, 0, measure = "bowley"), "Invalid arguments")

  # Test invalid measure type
  expect_error(ecp_shape(2, 0.3, 30, measure = "invalid"),
               "Invalid measure type. Use 'bowley' or 'moors'.")
})
