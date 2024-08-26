# Test file for the hecp function

# Explanation of Tests #

# Basic Functionality: The first two tests check if the function returns the
# correct value for valid inputs, both with and without the log parameter.

# Error Handling: The next two tests ensure that the function correctly throws
# errors when given non-numeric or invalid input values.

# Edge Cases: A test to handle the special case on the edge, x = 0.

test_that("hecp function works correctly with valid inputs", {
  result <- hecp(2, 1, 1, 1, log = FALSE)
  expect_equal(result, (1 * 1 * 1 * 2^(1 - 1) * exp(2^1 + 1 * (1 - exp(2^1)))) /
                 (exp(1 * exp(1 * (1 - exp(2^1)))) - 1))
})
# Test passed

test_that("hecp function works correctly with log scale", {
  result <- hecp(2, 1, 1, 1, log = TRUE)
  expected <- log((1 * 1 * 1 * 2^(1 - 1) * exp(2^1 + 1 * (1 - exp(2^1)))) /
                    (exp(1 * exp(1 * (1 - exp(2^1)))) - 1))
  expect_equal(result, expected)
})
# Test passed

test_that("hecp function handles non-numeric input", {
  expect_error(hecp("a", 1, 1, 1), "non-numeric argument")
  expect_error(hecp(2, "b", 1, 1), "non-numeric argument")
})
# Test passed

test_that("hecp function handles invalid input parameters", {
  expect_error(hecp(-1, 1, 1, 1), "Invalid arguments")
  expect_error(hecp(2, -1, 1, 1), "Invalid arguments")
  expect_error(hecp(2, 1, -1, 1), "Invalid arguments")
  expect_error(hecp(2, 1, 1, 0), "Invalid arguments")
})
# Test passed

test_that("hecp returns expected results for edge cases", {
  # Example edge case
  result <- hecp(0, 1, 1, 1, log = FALSE)
  expect_equal(result, 0.58,  tolerance = 1e-2)  # Assuming that 0^0=1, as is
  # common practice in programming context, and therefore the hazard function
  # should be 0.58 at x = 0
})
# Test passed
