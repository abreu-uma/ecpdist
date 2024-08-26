# Test file for the ecp_plot function

# Explanation of Tests #

# The following tests cover various scenarios and ensure that the ecp_plot
# function behaves as expected. The tests verify that the plots are generated
# without errors, handle invalid input gracefully, and check specific
# functionality such as log transformations and different data types.

# Test for Density Plot with Expression Data Type
# This test checks if the function can plot a density curve using the
# expression type:

test_that("ecp_plot generates a density plot with expression data type", {
  expect_silent(
    ecp_plot(
      data_type = "expression",
      from = 0,
      to = 5,
      lambda = 1,
      gamma = 1,
      phi = 1,
      func_type = "density",
      title = "Density Plot (Expression)"
    )
  )
})

# Test for Cumulative Distribution Plot with Expression Data Type
# This test ensures that the function can generate a cumulative distribution
# plot using the expression type:

test_that("ecp_plot generates a cumulative distribution plot with expression
          data type", {
            expect_silent(
              ecp_plot(data_type = "expression", from = 0, to = 5, lambda = 2,
                gamma = 0.5, phi = 2, func_type = "cumulative distribution",
                title = "Cumulative Distribution Plot (Expression)"
              )
            )
          })

# Test for Hazard Function Plot with Data Type
# This test checks if the function correctly plots the hazard function using
# data points:

test_that("ecp_plot generates a hazard plot with data points", {
  x_data <- seq(0.1, 1, by = 0.01)
  expect_silent(
    ecp_plot(
      data_type = "data",
      x = x_data,
      lambda = 2,
      gamma = 0.3,
      phi = 30,
      func_type = "hazard",
      title = "Hazard Function (Data Points)"
    )
  )
})

# Test for Invalid Function Type
# This test ensures that an error is thrown when an invalid function type is
# provided:

test_that("ecp_plot throws an error for an invalid function type", {
  expect_error(
    ecp_plot(
      data_type = "expression",
      from = 0,
      to = 5,
      lambda = 1,
      gamma = 1,
      phi = 1,
      func_type = "invalid_type",
      title = "Invalid Function Type"
    ),
    "Invalid function type"
  )
})

# Test for Missing Data Points
# This test checks if an error is thrown when data_type = "data" is used without
# providing x values:

test_that("ecp_plot throws an error when data type is 'data' but no data points
          are provided", {
            expect_error(
              ecp_plot(data_type = "data", lambda = 1, gamma = 1, phi = 1,
                func_type = "density",
                title = "Missing Data Points"
              ),
              "Data points must be provided for data type 'data'."
            )
          })

# Test for Plot with Log Transformation
# This test ensures that the plot function correctly handles the log = TRUE
# parameter:

test_that("ecp_plot generates a density plot with log transformation", {
  expect_silent(
    ecp_plot(
      data_type = "expression",
      from = 0,
      to = 5,
      lambda = 1,
      gamma = 1,
      phi = 1,
      func_type = "density",
      log = TRUE,
      title = "Density Plot with Log Transformation"
    )
  )
})
