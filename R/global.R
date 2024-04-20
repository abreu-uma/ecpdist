
# Fazer library(devtools)

# Fazer library(roxygen2)


# Fazer devtools::load_all() para carregar o código de cada vez que se
# abre o ficheiro.

# Fazer devtools::lint() para verificar erros de sintaxe.

# Fazer devtools::check() para ver se está tudo ok.

# Fazer  document() para atualizar a documentação com o roxygen2.

# Fazer library(formatR) quando estiver a escrever código novo e depois,
# copiar o código e correr a instrução tidy_source(width.cutoff = 60).
# Isto facilita pois a sintaxe já fica quase toda bem.

#### Distribution function ####

#' Distribution function
#'
#' @description
#' Compute the extended Chen-Poisson (ecp) distribution function.
#'
#' @param q vector of quantiles.
#'
#' @param lambda,gamma  parameter values > 0.
#'
#' @param phi parameter value > 0 or < 0.
#'
#' @param lower_tail similar to lower.tail
#'
#' @param log_p logical value
#'
#' @return Numeric value of the distribution function.
#'
#' @return If lower_tail = FALSE, numeric value of the survival function.
#'
#' @return If log_p = TRUE, numeric value of the logarithm of the function.
#'
#' @examples
#' pecp(2, 1, 1, 1, lower_tail = TRUE, log_p = FALSE) # distribution function
#' pecp(2, 1, 1, 1, lower_tail = FALSE, log_p = FALSE) # survival function
#'
#' @export
pecp <- function(q, lambda, gamma, phi, lower_tail = TRUE,
                 log_p = FALSE) {
  # Check if arguments are numeric
  if (!all(sapply(list(q, lambda, gamma, phi), is.numeric))) {
    stop("non-numeric argument")
  }

  # Check for invalid arguments
  if (any(q < 0) || min(lambda <= 0) || min(gamma <=
                                              0) || min(phi == 0)) {
    stop("Invalid arguments")
  }

  # Calculate CDF #
  cdf <- 1 -  (1 - exp(-phi * exp(lambda * (1 - exp(q^gamma))))) /
    (1 - exp(-phi))

  # Adjust CDF if lower_tail is FALSE
  if (!lower_tail) {
    cdf <- 1 - cdf
  }

  # Convert CDF to log scale if log_p is TRUE
  if (log_p) {
    cdf <- log(cdf)
  }

  return(cdf)
}

pecp(2, 1, 1, 1, lower_tail = TRUE,
     log_p = FALSE)

# End


#### Survival function ####

#' Survival function
#'
#' @description
#' Compute the extended Chen-Poisson (ecp) survival function.
#'
#' @param q vector of quantiles.
#'
#' @param lambda,gamma  parameter values > 0.
#'
#' @param phi parameter value > 0 or < 0.
#'
#' @param lower_tail similar to lower.tail
#'
#' @param cum_haz logical value
#'
#' @return Numeric value of the survival function.
#'
#' @return If lower_tail = TRUE, numeric value of the distribution function.
#'
#' @return If cum_haz = TRUE, numeric value of the cumulative hazard function.
#'
#' @examples
#' secp(2, 1, 1, 1, lower_tail = FALSE, cum_haz = FALSE) # survival function
#' secp(2, 1, 1, 1, lower_tail = TRUE, cum_haz = FALSE) # distribution function
#' secp(2, 1, 1, 1, lower_tail = FALSE, cum_haz = TRUE) # cumulative
#' # hazard function
#'
#' @export
secp <- function(q, lambda, gamma, phi, lower_tail = FALSE,
                 cum_haz = FALSE) {
  # Check if arguments are numeric
  if (!all(sapply(list(q, lambda, gamma, phi), is.numeric))) {
    stop("non-numeric argument")
  }

  # Check for invalid arguments
  if (any(q < 0) || min(lambda <= 0) || min(gamma <= 0) ||
        min(phi == 0)) {
    stop("Invalid arguments")
  }

  # Calculate survival function
  sf <- (1 - exp(-phi * exp(lambda * (1 - exp(q^gamma))))) /
    (1 - exp(-phi))

  # Adjust survival function if lower_tail is TRUE
  if (lower_tail)
    sf <- 1 - sf

  # Calculate cumulative hazard function if cum_haz is TRUE

  if (cum_haz)
    sf <- -log(sf)

  return(sf)

}



#### Density function ####

#' Density function
#'
#' @description
#' Compute the extended Chen-Poisson (ecp) density function.
#'
#' @param x vector of quantiles.
#'
#' @param lambda,gamma  parameter values > 0.
#'
#' @param phi parameter value > 0 or < 0.
#'
#' @param log logical value
#'
#' @return Numeric value of the density function.
#'
#' @return If log = TRUE, numeric value of the logarithm of the function.
#'
#' @examples
#' decp(2, 1, 1, 1, log = FALSE) # density function
#'
#' @export
decp <- function(x, lambda, gamma, phi, log = FALSE) {
  # Check if arguments are numeric
  if (!all(sapply(list(x, lambda, gamma, phi), is.numeric))) {
    stop("non-numeric argument")
  }

  # Check for invalid arguments
  if (any(x < 0) || min(lambda <= 0) || min(gamma <= 0) ||
        min(phi == 0)) {
    stop("Invalid arguments")
  }

  # Calculate density function

  pdf <- (lambda * gamma * phi * x^(gamma - 1) *
            exp(-phi * exp(lambda * (1 - exp(x^gamma))) +
                  lambda * (1 - exp(x^gamma)) + x^gamma)) / (1 - exp(-phi))

  # Convert density function to log scale if log is TRUE

  if (log)
    pdf <- log(pdf)

  return(pdf)

}

#### Hazard function ####

#' Hazard function
#'
#' @description
#' Compute the extended Chen-Poisson (ecp) hazard function.
#'
#' @param x vector of quantiles.
#'
#' @param lambda,gamma  parameter values > 0.
#'
#' @param phi parameter value > 0 or < 0.
#'
#' @param log logical value
#'
#' @return Numeric value of the hazard function.
#'
#' @return If log = TRUE, numeric value of the logarithm of the function.
#'
#' @examples
#' hecp(2, 1, 1, 1, log = FALSE) # hazard function
#'
#' @export
hecp <- function(x, lambda, gamma, phi, log = FALSE) {
  # Check if arguments are numeric
  if (!all(sapply(list(x, lambda, gamma, phi), is.numeric))) {
    stop("non-numeric argument")
  }

  # Check for invalid arguments
  if (any(x < 0) || min(lambda <= 0) || min(gamma <= 0) ||
        min(phi == 0)) {
    stop("Invalid arguments")
  }

  # Calculate hazard function

  hf <- (lambda * gamma * phi * x^(gamma - 1) *
           exp(x^gamma + lambda * (1 - exp(x^gamma)))) /
    (exp(phi * exp(lambda * (1 - exp(x^gamma)))) - 1)

  # Convert hazard function to log scale if log is TRUE
  if (log)
    hf <- log(hf)

  return(hf)

}
