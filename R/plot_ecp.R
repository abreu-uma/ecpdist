#### Plot function ####

#' Plot function
#'
#' @description
#' Plots the density, cumulative distribution, hazard, survival and quantile
#' functions of the extended Chen-Poisson (ecp) distribution.
#'
#' @param data_type specifies whether the input is a x vector of data values or
#' an expression.
#'
#' @param from lower x axis limit.
#'
#' @param to upper x axis limit.
#'
#' @param xlim x axis limits.
#'
#' @param ylim y axis limits.
#'
#' @param x vector of data values.
#'
#' @param lambda,gamma  parameter values > 0.
#'
#' @param phi parameter value > 0 or < 0.
#'
#' @param log logical value.
#'
#' @param func_type specifies the type of function to be plotted.
#'
#' @param title title of the graphic.
#'
#' @param col to set the color of the graphic.
#'
#' @param lty to set the line type.
#'
#' @return If log = TRUE, numeric value of the logarithm of the function.
#'
#' @return graphic of the chosen ecp function.
#'
#' @examples
#'
#' ecp_plot(data_type = "expression", from = 0, to = 1,
#' func_type = "cumulative distribution",
#' title = "Cumulative Distribution Function (Expression)")
#' #  Example of plotting cumulative distribution using an expression
#' @export
#'

# Define the function for plotting

ecp_plot <- function(data_type, from = NULL, to = NULL, xlim = NULL,
                     ylim = NULL, x = NULL, lambda = 6, gamma = 0.3, phi = 30,
                     log = FALSE, func_type, title, col = "black", lty = 1) {

  # Set default plotting range if not provided
  if (is.null(from)) from <- 0
  if (is.null(to)) to <- 1
  if (is.null(xlim)) xlim <- c(from, to)

  # Define the expressions based on func_type
  func <- switch(func_type,
                 "density" = function(x) decp(x, lambda, gamma, phi, log),
                 "hazard" = function(x) hecp(x, lambda, gamma, phi, log),
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
                      'survival', 'cumulative distribution' or 'quantile'."))

  if (data_type == "expression") {
    # Plot based on expression
    ylab <- switch(func_type,
                   "density" = "Density",
                   "hazard" = "Hazard",
                   "survival" = "Survival",
                   "cumulative distribution" = "Cumulative Distribution",
                   "quantile" = "Quantile")

    curve(expr = func, from = from, to = to, xlim = xlim, ylim = ylim,
          col = col, lty = lty, main = title, xlab = "x", ylab = ylab)

  } else if (data_type == "data") {
    if (is.null(x)) {
      stop("Data points must be provided for data type 'data'.")
    }
    y_values <- switch(func_type,
                       "density" = decp(x, lambda, gamma, phi, log),
                       "hazard" = hecp(x, lambda, gamma, phi, log),
                       "survival" = secp(x, lambda, gamma, phi,
                                         lower_tail = FALSE, cum_haz = FALSE),
                       "cumulative distribution" = pecp(x, lambda, gamma, phi,
                                                        lower_tail = TRUE,
                                                        log_p = FALSE),
                       "quantile" = qecp(x, lambda, gamma, phi,
                                         lower_tail = TRUE, log_p = FALSE),
                       stop("Invalid function type. Use 'density', 'hazard',
                            'survival', 'cumulative distribution' or
                            'quantile'."))
    plot(x, y_values, type = "l", xlim = xlim, ylim = ylim, col = col,
         lty = lty, main = title, xlab = "x", ylab = func_type)

  } else {
    stop("Invalid data type. Use 'expression' or 'data'.")
  }
}
