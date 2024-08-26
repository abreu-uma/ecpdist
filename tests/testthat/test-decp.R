# Test file for the decp function

# Explanation of the Tests

# Basic Valid Input:

# Tests a standard case where all inputs are valid and checks the result against
# a known output.

# Logarithm Option:

# Verifies the function correctly returns the log of the density when log =
# TRUE.

# Edge Case (Zero x Value):

# Checks the behavior of the function when x = 0, an edge case for many
# distributions.

# Invalid Lambda, Gamma and Phi:

# Ensures the function correctly handles a negative lambda, a negative gamma and
# phi = 0, by throwing an error.

# Non-Numeric Input:

# Ensures the function throws an error when a non-numeric value is passed.

# Vectorized x Input:

# Tests the function's ability to handle vectorized input for x.

# Large Parameters:

# Checks how the function behaves with large values for lambda and gamma.

# Large Phi Value:

# Tests the function's behavior when phi is very large.

# Load the testthat package
library(testthat)

# Test Suite for decp function
test_that("decp function works correctly", {

  # Test 1: Basic valid input
  expect_equal(decp(2, 1, 1, 1, log = FALSE), (1 * 1 * 1 * 2^(1 - 1) *
                                                 exp(-1 * exp(1 *
                                                                (1 - exp(2^1)))
                                                     + 1 * (1 - exp(2^1)) +
                                                       2^1)) / (1 - exp(-1)),
               tolerance = 1e-7)

  # Test 2: Basic valid input with log = TRUE (note that (1 * 1 * 1 * 2^(1 - 1)
  # * exp(-1 * exp(1 * (1 - exp(2^1))) + 1 * (1 - exp(2^1)) + 2^1)) /
  # (1 - exp(-1))) = 0.01960323 so, for simplicity, i just put
  # log(0.01960323) instead of all expression).
  expect_equal(decp(2, 1, 1, 1, log = TRUE), log(0.01960323), tolerance = 1e-7)

  # Test 3: Zero x value (edge case)
  expect_equal(decp(0, 1, 1, 1, log = FALSE), 0.5819767, tolerance = 1e-7)

  # Test 4: Invalid lambda (should stop)
  expect_error(decp(2, -1, 1, 1, log = FALSE), "Invalid arguments")

  # Test 5: Invalid gamma (should stop)
  expect_error(decp(2, 1, -1, 1, log = FALSE), "Invalid arguments")

  # Test 6: Invalid phi (should stop)
  expect_error(decp(2, 1, 1, 0, log = FALSE), "Invalid arguments")

  # Test 7: Non-numeric input (should stop)
  expect_error(decp("a", 1, 1, 1, log = FALSE), "non-numeric argument")

  # Test 8: Vectorized x input
  expect_equal(decp(c(2, 3), 1, 1, 1, log = FALSE),
               c(0.01960323,  1.634332e-07), tolerance = 1e-6)

  # Test 9: Large lambda and gamma values
  expect_equal(decp(2, 10, 10, 1, log = FALSE), 2.450000e-10, tolerance = 1e-7)

  # Test 10: Check output when phi is very large
  expect_equal(decp(2, 1, 1, 100, log = FALSE), 1.049307, tolerance = 1e-7)

})
