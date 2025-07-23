#### Plot function of the ecp distribution ####

#' Plot function of the ecp distribution
#'
#' @description
#'
#' Plots the density, cumulative distribution, hazard, cumulative hazard,
#' survival and quantile functions of the extended Chen-Poisson (ecp)
#' distribution.
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
#' @param phi parameter value != 0.
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
#' @return graphic of the chosen ecp function.
#'
#' @examples
#'
#'#  Example of plotting cumulative distribution using an expression
#'
#' ecp_plot(data_type = "expression", from = 0, to = 6, lambda = 2, gamma = 0.3,
#' phi = 30, func_type = "cumulative distribution",
#' title = "ecp Cumulative Distribution Function (Expression)")
#'
#'#  Example of plotting an unimodal hazard function using an expression
#'
#' ecp_plot(data_type = "expression", lambda = 2, gamma = 0.3, phi = 30,
#' func_type = "hazard", title = "ecp Hazard Function (Expression)")
#'
#'#  Example of plotting an unimodal hazard function using data points
#'
#' x_data <- seq(0.0000001, 1, by = 0.0001)
#' ecp_plot(data_type = "data", x = x_data, lambda = 2, gamma = 0.3, phi = 30,
#' func_type = "hazard", title = "ecp Hazard Function (Data Points)")
#'
#' @export
#'

# Define the function for plotting

ecp_plot <- function(data_type, from = NULL, to = NULL, xlim = NULL,
                     ylim = NULL, x = NULL, lambda, gamma, phi,
                     log = FALSE, func_type, title, col = "black", lty = 1) {

  # Set default plotting range if not provided

  if (is.null(from)) from <- 0
  if (is.null(to)) to <- 1
  if (is.null(xlim)) xlim <- c(from, to)

  # Define the expressions based on func_type

  func <- switch(func_type,
                 "density" = function(x) decp(x, lambda, gamma, phi, log),
                 "hazard" = function(x) hecp(x, lambda, gamma, phi, log),
                 "cumulative hazard" = function(x) {
                                                    secp(x, lambda, gamma, phi,
                                                         lower_tail = FALSE,
                                                         cum_haz = TRUE)},
                 "survival" = function(x) {
                                           secp(x, lambda, gamma, phi,
                                                lower_tail = FALSE,
                                                cum_haz = FALSE)},
                 "cumulative distribution" = function(x) {
                                                          pecp(x, lambda, gamma,
                                                               phi,
                                                               lower_tail =
                                                                 TRUE,
                                                               log_p = FALSE)},
                 "quantile" = function(x) {
                                           qecp(x, lambda, gamma, phi,
                                                lower_tail = TRUE,
                                                log_p = FALSE)},
                 stop("Invalid function type. Use 'density', 'hazard',
                      'cumulative hazard', 'survival', 'cumulative distribution'
                      or 'quantile'."))

  # Define y-axis label based on func_type

  ylab <- switch(func_type,
                 "density" = "Density",
                 "hazard" = "Hazard",
                 "cumulative hazard" = "Cumulative Hazard",
                 "survival" = "Survival",
                 "cumulative distribution" = "Cumulative Distribution",
                 "quantile" = "Quantile")

  if (data_type == "expression") {

    # Plot based on expression

    ylab <- switch(func_type,
                   "density" = "Density",
                   "hazard" = "Hazard",
                   "cumulative hazard" = "Cumulative Hazard",
                   "survival" = "Survival",
                   "cumulative distribution" = "Cumulative Distribution",
                   "quantile" = "Quantile")

    graphics::curve(expr = func, from = from, to = to, xlim = xlim, ylim = ylim,
                    col = col, lty = lty, main = title, xlab = "x", ylab = ylab)

  } else if (data_type == "data") {
    if (is.null(x)) {
      stop("Data points must be provided for data type 'data'.")
    }
    y_values <- switch(func_type,
                       "density" = decp(x, lambda, gamma, phi, log),
                       "hazard" = hecp(x, lambda, gamma, phi, log),
                       "cumulative hazard" = secp(x, lambda, gamma, phi,
                                                  lower_tail = FALSE,
                                                  cum_haz = TRUE),
                       "survival" = secp(x, lambda, gamma, phi,
                                         lower_tail = FALSE, cum_haz = FALSE),
                       "cumulative distribution" = pecp(x, lambda, gamma, phi,
                                                        lower_tail = TRUE,
                                                        log_p = FALSE),
                       "quantile" = qecp(x, lambda, gamma, phi,
                                         lower_tail = TRUE, log_p = FALSE),
                       stop("Invalid function type. Use 'density', 'hazard',
                            'cumulative hazard', 'survival',
                            'cumulative distribution' or 'quantile'."))
    plot(x, y_values, type = "l", xlim = xlim, ylim = ylim, col = col,
         lty = lty, main = title, xlab = "x", ylab = ylab)

  } else {
    stop("Invalid data type. Use 'expression' or 'data'.")
  }
}
