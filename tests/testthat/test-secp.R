# Test file for the secp function

# Explanation of Tests #

# Basic Functionality:

# expect_equal() checks that the secp function returns the correct values for
# various combinations of inputs.

# Handling of Parameters:

# Tests are included for lower_tail = TRUE, cum_haz = TRUE, and combinations of
# both to ensure the function handles these parameters correctly.

# Error Handling:

# expect_error() ensures the function properly handles non-numeric inputs and
# invalid argument values.

# Edge Cases:

# Tests for q = 0 and q = Infensure the function behaves correctly at the
# boundary of the quantile range.

library(testthat)

# Test the secp function
test_that("secp function works correctly", {

  # Test with default arguments
  expect_equal(
    secp(2, 1, 1, 1, lower_tail = FALSE, cum_haz = FALSE),
    (1 - exp(-1 * exp(1 * (1 - exp(2^1))))) / (1 - exp(-1))
  )

  # Test with lower_tail = TRUE
  expect_equal(
    secp(2, 1, 1, 1, lower_tail = TRUE, cum_haz = FALSE),
    1 - (1 - exp(-1 * exp(1 * (1 - exp(2^1))))) / (1 - exp(-1))
  )

  # Test with cum_haz = TRUE
  expect_equal(
    secp(2, 1, 1, 1, lower_tail = FALSE, cum_haz = TRUE),
    -log((1 - exp(-1 * exp(1 * (1 - exp(2^1))))) / (1 - exp(-1)))
  )

  # Test with lower_tail = TRUE and cum_haz = TRUE
  expect_equal(
    secp(2, 1, 1, 1, lower_tail = TRUE, cum_haz = TRUE),
    -log(1 - (1 - exp(-1 * exp(1 * (1 - exp(2^1))))) / (1 - exp(-1)))
  )

  # Test for non-numeric input
  expect_error(
    secp("a", 1, 1, 1),
    "non-numeric argument"
  )

  # Test for invalid arguments
  expect_error(
    secp(-1, 1, 1, 1),
    "Invalid arguments"
  )
  expect_error(
    secp(2, -1, 1, 1),
    "Invalid arguments"
  )
  expect_error(
    secp(2, 1, -1, 1),
    "Invalid arguments"
  )
  expect_error(
    secp(2, 1, 1, 0),
    "Invalid arguments"
  )

  # Additional tests for edge cases
  expect_equal(
    secp(0, 1, 1, 1, lower_tail = FALSE, cum_haz = FALSE),
    (1 - exp(-1 * exp(1 * (1 - exp(0^1))))) / (1 - exp(-1))
  )
  expect_equal(
    secp(0, 1, 1, 1, lower_tail = TRUE, cum_haz = FALSE),
    1 - (1 - exp(-1 * exp(1 * (1 - exp(0^1))))) / (1 - exp(-1))
  )
  expect_equal(
    secp(0, 1, 1, 1, lower_tail = FALSE, cum_haz = TRUE),
    -log((1 - exp(-1 * exp(1 * (1 - exp(0^1))))) / (1 - exp(-1)))
  )
  expect_equal(
    secp(Inf, 1, 1, 1, lower_tail = FALSE, cum_haz = FALSE),
    (1 - exp(-1 * exp(1 * (1 - exp(Inf^1))))) / (1 - exp(-1))
  )
  expect_equal(
    secp(Inf, 1, 1, 1, lower_tail = TRUE, cum_haz = FALSE),
    1 - (1 - exp(-1 * exp(1 * (1 - exp(Inf^1))))) / (1 - exp(-1))
  )
  expect_equal(
    secp(Inf, 1, 1, 1, lower_tail = FALSE, cum_haz = TRUE),
    -log((1 - exp(-1 * exp(1 * (1 - exp(Inf^1))))) / (1 - exp(-1)))
  )

})

# Test passed
