# Test file for the pecp function

# Explanation of the Tests

# Basic Functionality:

# expect_equal() checks that the pecp function returns the correct value for
# different inputs.

# Handling of Parameters:

# Tests are included for both lower_tail = FALSE and log_p = TRUE.

# Error Handling:

# expect_error() is used to ensure that the function properly handles
# non-numeric and invalid arguments.

# Edge Cases:

# Additional tests ensure the function behaves correctly for edge cases like
# q = 0 and q = Inf.

library(testthat)

# Test the pecp function
test_that("pecp function works correctly", {

  # Test with default arguments
  expect_equal(
    pecp(2, 1, 1, 1, lower_tail = TRUE, log_p = FALSE),
    1 - (1 - exp(-1 * exp(1 * (1 - exp(2^1))))) / (1 - exp(-1))
  )

  # Test with lower_tail = FALSE
  expect_equal(
    pecp(2, 1, 1, 1, lower_tail = FALSE, log_p = FALSE),
    1 - (1 - (1 - exp(-1 * exp(1 * (1 - exp(2^1))))) / (1 - exp(-1)))
  )

  # Test with log_p = TRUE
  expect_equal(
    pecp(2, 1, 1, 1, lower_tail = TRUE, log_p = TRUE),
    log(1 - (1 - exp(-1 * exp(1 * (1 - exp(2^1))))) / (1 - exp(-1)))
  )

  # Test for non-numeric input
  expect_error(
    pecp("a", 1, 1, 1),
    "non-numeric argument"
  )

  # Test for invalid arguments
  expect_error(
    pecp(-1, 1, 1, 1),
    "Invalid arguments"
  )
  expect_error(
    pecp(2, -1, 1, 1),
    "Invalid arguments"
  )
  expect_error(
    pecp(2, 1, -1, 1),
    "Invalid arguments"
  )
  expect_error(
    pecp(2, 1, 1, 0),
    "Invalid arguments"
  )

  # Additional tests for edge cases
  expect_equal(
    pecp(0, 1, 1, 1, lower_tail = TRUE, log_p = FALSE),
    0
  )
  expect_equal(
    pecp(0, 1, 1, 1, lower_tail = FALSE, log_p = FALSE),
    1
  )
  expect_equal(
    pecp(0, 1, 1, 1, lower_tail = TRUE, log_p = TRUE),
    -Inf
  )
  expect_equal(
    pecp(Inf, 1, 1, 1, lower_tail = TRUE, log_p = FALSE),
    1
  )
  expect_equal(
    pecp(Inf, 1, 1, 1, lower_tail = FALSE, log_p = FALSE),
    0
  )

})

# Test passed
