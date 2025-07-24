#### Plot function of the Chen distribution ####

#' Plot function of the Chen distribution
#'
#' @description
#'
#' Plots the density, cumulative distribution, hazard, cumulative hazard,
#' survival and quantile functions of the Chen distribution.
#'
#' @param data_type specifies whether the input is a x vector of data values or
#' an expression. Possible types are
#'
#' - 'data' for data values,
#'
#' - 'expression' for expression.
#'
#' @param from lower x axis limit, by default from = 0.
#'
#' @param to upper x axis limit, by default to = 1.
#'
#' @param xlim x axis limits, by default xlim = c(from, to).
#'
#' @param ylim y axis limits.
#'
#' @param x vector of data values when data_type = "data".
#'
#' @param lambda,gamma  parameter values > 0.
#'
#' @param log logical value.
#'
#' @param func_type specifies the type of function to be plotted. Possible
#' types are
#'
#' - 'density' for density plot,
#'
#' - 'hazard' for hazard plot,
#'
#' - 'cumulative hazard' for cumulative hazard plot,
#'
#' - 'survival' for survival plot,
#'
#' - 'cumulative distribution' for cumulative distribution plot,
#'
#' - 'quantile' for quantile plot.
#'
#' @param title title of the graphic.
#'
#' @param col to set the color of the graphic.
#'
#' @param lty to set the line type.
#'
#' @return If log = TRUE, numeric value of the logarithm of the function.
#'
#' @return If cum_haz = TRUE, numeric value of the cumulative hazard function.
#'
#' @return graphic of the chosen chen function.
#'
#' @examples
#'
#'#  Example of plotting cumulative distribution using an expression
#'
#' chen_plot(data_type = "expression", from = 0, to = 6, lambda = 2,
#' gamma = 0.3, func_type = "cumulative distribution",
#' title = "Chen cumulative distribution function (expression)")
#'
#'#  Example of plotting a bathtub-shaped hazard function using an expression
#'
#' chen_plot(data_type = "expression", lambda = 2, gamma = 0.3,
#' func_type = "hazard", title = "Chen hazard function (expression)", to = 100,
#' ylim = c(0.8, 1.8))
#'
#'# Example of plotting a bathtub-shaped hazard function using data points
#'
#' x_data <- seq(0.0000001, 100, by = 0.1)
#' chen_plot(data_type = "data", x = x_data, lambda = 2, gamma = 0.3,
#' func_type = "hazard", title = "Chen hazard function (data points)",
#' to = 100, ylim = c(0.8, 1.8))
#'
#' @export
#'

# Define the function for plotting

chen_plot <- function(data_type, from = NULL, to = NULL, xlim = NULL,
                      ylim = NULL, x = NULL, lambda, gamma, log = FALSE,
                      func_type, title, col = "blue", lty = 1) {

  # Set default plotting range if not provided

  if (is.null(from)) from <- 0
  if (is.null(to)) to <- 1
  if (is.null(xlim)) xlim <- c(from, to)

  # Define the expressions based on func_type

  func <- switch(func_type,
                 "density" = function(x) dchen(x, lambda, gamma, log),
                 "hazard" = function(x) hchen(x, lambda, gamma, log),
                 "cumulative hazard" = function(x) {
                                                    schen(x, lambda, gamma,
                                                          lower_tail = FALSE,
                                                          cum_haz = TRUE)},
                 "survival" = function(x) {
                                           schen(x, lambda, gamma,
                                                 lower_tail = FALSE,
                                                 cum_haz = FALSE)},
                 "cumulative distribution" = function(x) {
                                                          pchen(x, lambda,
                                                                gamma,
                                                                lower_tail =
                                                                  TRUE,
                                                                log_p = FALSE)},
                 "quantile" = function(x) {
                                           qchen(x, lambda, gamma,
                                                 lower_tail = TRUE,
                                                 log_p = FALSE)},
                 stop("Invalid function type. Use 'density', 'hazard',
          'cumulative hazard', 'survival', 'cumulative distribution'
                      or 'quantile'."))

  # Define y-axis label based on func_type

  ylab <- switch(func_type,
                 "density" = "Density",
                 "hazard" = "Hazard",
                 "cumulative hazard" = "Cumulative hazard",
                 "survival" = "Survival",
                 "cumulative distribution" = "Cumulative distribution",
                 "quantile" = "Quantile: Chen")

  if (data_type == "expression") {

    # Plot based on expression

    ylab <- switch(func_type,
                   "density" = "Density",
                   "hazard" = "Hazard",
                   "cumulative hazard" = "Cumulative hazard",
                   "survival" = "Survival",
                   "cumulative distribution" = "Cumulative distribution",
                   "quantile" = "Quantile")

    graphics::curve(expr = func, from = from, to = to, xlim = xlim, ylim = ylim,
                    col = col, lty = lty, main = title, xlab = "x", ylab = ylab)

  } else if (data_type == "data") {
    if (is.null(x)) {
      stop("Data points must be provided for data type 'data'.")
    }
    y_values <- switch(func_type,
                       "density" = dchen(x, lambda, gamma, log),
                       "hazard" = hchen(x, lambda, gamma, log),
                       "cumulative hazard" = secp(x, lambda, gamma,
                                                  lower_tail = FALSE,
                                                  cum_haz = TRUE),
                       "survival" = schen(x, lambda, gamma, lower_tail = FALSE,
                                          cum_haz = FALSE),
                       "cumulative distribution" = pecp(x, lambda, gamma,
                                                        lower_tail = TRUE,
                                                        log_p = FALSE),
                       "quantile" = qchen(x, lambda, gamma, lower_tail = TRUE,
                                          log_p = FALSE),
                       stop("Invalid function type. Use 'density', 'hazard',
                            'cumulative hazard', 'survival',
                            'cumulative distribution' or 'quantile'."))
    plot(x, y_values, type = "l", xlim = xlim, ylim = ylim, col = col,
         lty = lty, main = title, xlab = "x", ylab = ylab)

  } else {
    stop("Invalid data type. Use 'expression' or 'data'.")
  }
}
