#' Unit Maxwell-Boltzmann distribution
#'
#' @author David Villegas Ceballos, \email{david.villegas1@udea.edu.co}
#'
#' @description
#' These functions define the density, distribution function, quantile
#' function and random generation for the Unit Maxwell-Boltzmann distribution
#' with parameter \eqn{\mu}.
#'
#' @param x,q vector of (non-negative integer) quantiles.
#' @param p vector of probabilities.
#' @param mu vector of the mu parameter.
#' @param n number of random values to return.
#' @param log,log.p logical; if TRUE, probabilities p are given as log(p).
#' @param lower.tail logical; if TRUE (default), probabilities are
#' \eqn{P[X <= x]}, otherwise, \eqn{P[X > x]}.
#'
#' @references
#' Bicher, C., Bakouch, H. S., Biçor, H. D., Alomair, G.,
#' Hussain, T., y Almohisen, A. (2024). Unit Maxwell-Boltzmann Distribution and
#' Its Application to Concentrations Pollutant Data. Axioms, 13(4), 226.
#'
#' @details
#' The Unit Maxwell-Boltzmann distribution with parameter \eqn{\mu}
#' has a support in \eqn{(0, 1)} and density given by
#'
#' \eqn{f(x| \mu) = \frac{2 \mu}{(\mu+(2-\mu)x)^2} }
#'
#' for \eqn{0 < x < 1} and \eqn{\mu > 0}.
#'
#' @example examples/examples_dUMB.R
#'
#' @export
#' @importFrom stats dbeta
dUMB <- function(x, mu = 1, log = FALSE) {
  if (any(mu <= 0)) stop("parameter mu must be positive!")

  sapply(x, function(xi) {
    if (xi <= 0 || xi >= 1) return(if (log) -Inf else 0)

    log_term <- log(1 / xi)
    log_density <- 0.5 * log(2 / pi) + 2 * log(log_term) - 3 * log(mu) - log(xi) - (log_term^2) / (2 * mu^2)

    if (log) return(log_density)
    else return(exp(log_density))
  })
}
#' @export
#' @importFrom stats pnorm
#' @rdname dUMB
pUMB <- function(q, mu = 1, lower.tail = TRUE, log.p = FALSE) {
  if (any(mu <= 0)) stop("parameter mu must be positive!")

  erf <- function(x) 2 * pnorm(x * sqrt(2)) - 1

  sapply(q, function(qi) {
    if (qi <= 0) {
      return(if (log.p) -Inf else 0)
    }
    if (qi >= 1) {
      return(if (log.p) 0 else 1)
    }

    log_term <- log(1 / qi)
    erf_arg <- log_term / (sqrt(2) * mu)
    erf_part <- erf(erf_arg)

    term1 <- (sqrt(log_term^2) * erf_part) / log_term
    term2 <- (sqrt(2 / pi) * log_term * exp(-log_term^2 / (2 * mu^2))) / mu

    cdf <- 1 - term1 + term2
    if (!lower.tail) cdf <- 1 - cdf
    if (log.p) cdf <- log(cdf)
    return(cdf)
  })
}
#' @export
#' @importFrom stats uniroot
#' @rdname dUMB
qUMB <- function(p, mu = 1) {
  if (any(mu <= 0)) stop("parameter mu must be positive!")
  if (any(p < 0 | p > 1)) stop("p must be in [0, 1]")

  quantile_fn <- function(prob) {
    sapply(prob, function(pp) {
      if (pp == 0) return(0)
      if (pp == 1) return(1)
      uniroot(function(y) pUMB(y, mu = mu) - pp,
              lower = .Machine$double.eps, upper = 1 - .Machine$double.eps,
              tol = 1e-9)$root
    })
  }
  return(quantile_fn(p))
}
#' @export
#' @importFrom stats runif
#' @rdname dUMB
rUMB <- function(n = 1, mu = 1) {
  if (mu <= 0) stop("parameter mu must be positive!")
  u <- runif(n)
  qUMB(u, mu)
}
