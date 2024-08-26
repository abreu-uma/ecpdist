# Test file for the recp function

# Test Scenarios #

# 1. Basic Functionality: Check if the function returns a numeric vector of the
# specified length.

test_that("recp function returns a numeric vector of correct length", {
  set.seed(123)  # For reproducibility
  result <- recp(10, 1, 1, 1)
  expect_type(result, "double") # double is a special type of numeric data;
  # Using “double” can be beneficial in cases where precision is critical, such
  # as scientific computations or when working with very large or very small
  # numbers.
  expect_equal(length(result), 10)
})
# Test passed

# 2. Input Validation: Ensure the function throws errors for invalid inputs
# (e.g., non-numeric or negative values).

test_that("recp function handles invalid inputs", {
  expect_error(recp("a", 1, 1, 1), "non-numeric argument")
  expect_error(recp(10, -1, 1, 1), "Invalid arguments")
  expect_error(recp(10, 1, -1, 1), "Invalid arguments")
  expect_error(recp(10, 1, 1, 0), "Invalid arguments")
})
# Test passed

# 3. Edge Cases: Verify behavior for edge cases or extreme values.

test_that("recp function handles edge cases", {
  set.seed(123)  # For reproducibility

  # Testing with large sample size
  result_large_sample <- recp(1000, 1, 1, 1)
  expect_type(result_large_sample, "double")
  expect_equal(length(result_large_sample), 1000)

  # Testing with small parameter values
  result_small_params <- recp(10, 0.1, 0.1, 0.1)
  expect_type(result_small_params, "double")
  expect_equal(length(result_small_params), 10)
})
# Test passed
