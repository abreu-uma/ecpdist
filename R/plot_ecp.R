#### Plot function ####

#' Plot function
#'
#' @description
#' Plots the density, distribution, hazard and survival functions of the
#' extended Chen-Poisson (ecp) distribution.
#'
#' @param data_type either a x vector of data values or an expression.
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
#' @param func_type type of function to be plotted.
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
#' ecp_plot(data_type = "expression", func_type = "hazard",
#' title = "Hazard Function (Expression)") # hazard function graphic
#'
#' @export
#'
ecp_plot <- function(data_type, from = NULL, to = NULL, xlim = NULL,
                     ylim = NULL, x = NULL, lambda = 6, gamma = 0.3, phi = 30,
                     log = FALSE, func_type, title, col = "black", lty = 1) {

  # Set default plotting range if not provided

  if (is.null(from)) from <- 0
  if (is.null(to)) to <- 3
  if (is.null(xlim)) xlim <- c(from, to)

  # Determine the expression based on func_type

  expr <- switch(func_type,
                 "density" = "decp(x, lambda, gamma, phi, log)",
                 "hazard" = "hecp(x, lambda, gamma, phi, log)",
                 "survival" = "secp(x, lambda, gamma, phi, lower_tail = FALSE,
                 cum_haz = FALSE)",
                 "cumulative distribution" = "pecp(x, lambda, gamma, phi,
                 lower_tail = TRUE, log_p = FALSE)",

                 stop("Invalid function type. Use 'density' or 'hazard'."))

  # Plotting based on data type and func_type

  if (data_type == "expression") {

    # Parse and evaluate the expression

    expr_parsed <- parse(text = expr)

    if (func_type == "density") {
      curve(eval(expr_parsed, envir = list(x = x, lambda = lambda,
                                           gamma = gamma, phi = phi,
                                           log = log)),
            from = from, to = to, xlim = xlim, ylim = ylim, col = col,
            lty = lty, main = "", xlab = "x", ylab = "Density")

    } else if (func_type == "hazard") {
      curve(eval(expr_parsed, envir = list(x = x, lambda = lambda,
                                           gamma = gamma, phi = phi,
                                           log = log)),
            from = from, to = to, xlim = xlim, ylim = ylim, col = col,
            lty = lty, main = "", xlab = "x", ylab = "Hazard")
    } else if (func_type == "survival") {
      curve(eval(expr_parsed, envir = list(x = x, lambda = lambda,
                                           gamma = gamma, phi = phi,
                                           lower_tail = FALSE,
                                           cum_haz = FALSE)),
            from = from, to = to, xlim = xlim, ylim = ylim, col = col,
            lty = lty, main = "", xlab = "x", ylab = "Survival")
    } else if (func_type == "cumulative distribution") {
      curve(eval(expr_parsed, envir = list(x = x, lambda = lambda,
                                           gamma = gamma, phi = phi,
                                           lower_tail = TRUE,
                                           log_p = FALSE)),
            from = from, to = to, xlim = xlim, ylim = ylim, col = col,
            lty = lty, main = "", xlab = "x",
            ylab = "Cumulative distribution")

    }

  } else if (data_type == "data") {
    if (is.null(x)) {
      stop("Data points must be provided for data type 'data'.")
    }

    if (func_type == "density") {
      y_values <- decp(x, lambda, gamma, phi, log = log)
      plot(x, y_values, type = "l", xlim = xlim, ylim = ylim, col = col,
           lty = lty, main = "", xlab = "x", ylab = "Density")

    } else if (func_type == "hazard") {
      y_values <- hecp(x, lambda, gamma, phi, log = log)
      plot(x, y_values, type = "l", xlim = xlim, ylim = ylim, col = col,
           lty = lty, main = "", xlab = "x", ylab = "Hazard")
    } else if (func_type == "survival") {
      y_values <- secp(x, lambda, gamma, phi, lower_tail = FALSE,
                       cum_haz = FALSE)
      plot(x, y_values, type = "l", xlim = xlim, ylim = ylim, col = col,
           lty = lty, main = "", xlab = "x", ylab = "Survival")
    } else if (func_type == "cumulative distribution") {
      y_values <- pecp(x, lambda, gamma, phi, lower_tail = TRUE, log_p = FALSE)
      plot(x, y_values, type = "l", xlim = xlim, ylim = ylim, col = col,
           lty = lty, main = "", xlab = "x", ylab = "Cumulative distribution")
    }

  } else {
    stop("Invalid data type. Use 'expression' or 'data'.")
  }

  # Set the title after plotting

  title(main = title, adj = 0.5)
}

# Plotting density using an expression

ecp_plot(
  data_type = "expression",
  from = 0,
  to = 3,
  func_type = "density",
  title = "Density Function (Expression)"
)
