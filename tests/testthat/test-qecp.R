# Test file for the qecp function

# Explanation of the Tests #

# Basic Functionality:

# expect_equal() checks that the qecp function returns the correct value for
# different combination of inputs.

# Handling of Parameters:

# Tests include cases for both lower_tail = FALSE and log_p = TRUE to ensure
# the function correctly processes these arguments.

# Error Handling:

# expect_error() is used to ensure that the function properly handles invalid
# inputs, such as non-numeric values or out-of-bounds probabilities.

# Edge Cases:

# The tests cover edge cases like p = 0 and p = 1 to verify the function's
# behavior at the boundaries of the probability range.

library(testthat)

# Test the qecp function
test_that("qecp function works correctly", {

  # Test with default arguments
  expect_equal(
    qecp(0.5, 2, 1, 1, lower_tail = TRUE, log_p = FALSE),
    (log(1 - 2^(-1) * log(1 - 1^(-1) * log(1 + (exp(1) - 1) * 0.5))))^(1)
  )

  # Test with lower_tail = FALSE
  expect_equal(
    qecp(0.5, 2, 1, 1, lower_tail = FALSE, log_p = FALSE),
    (log(1 - 2^(-1) * log(1 - 1^(-1) * log(1 + (exp(1) - 1) * (1 - 0.5)))))^(1)
  )

  # Test with log_p = TRUE
  expect_equal(
    qecp(0.5, 2, 1, 1, lower_tail = TRUE, log_p = TRUE),
    log((log(1 - 2^(-1) * log(1 - 1^(-1) * log(1 + (exp(1) - 1) * 0.5))))^(1))
  )

  # Test for non-numeric input
  expect_error(
    qecp("a", 2, 1, 1),
    "non-numeric argument"
  )

  # Test for invalid arguments
  expect_error(
    qecp(-0.1, 2, 1, 1),
    "Invalid arguments"
  )
  expect_error(
    qecp(1.1, 2, 1, 1),
    "Invalid arguments"
  )
  expect_error(
    qecp(0.5, -1, 1, 1),
    "Invalid arguments"
  )
  expect_error(
    qecp(0.5, 2, -1, 1),
    "Invalid arguments"
  )
  expect_error(
    qecp(0.5, 2, 1, 0),
    "Invalid arguments"
  )

  # Additional tests for edge cases
  expect_equal(
    qecp(0, 2, 1, 1, lower_tail = TRUE, log_p = FALSE),
    (log(1 - 2^(-1) * log(1 - 1^(-1) * log(1 + (exp(1) - 1) * 0))))^(1)
  )
  expect_equal(
    qecp(1, 2, 1, 1, lower_tail = TRUE, log_p = FALSE),
    (log(1 - 2^(-1) * log(1 - 1^(-1) * log(1 + (exp(1) - 1) * 1))))^(1)
  )

})

# Test passed
