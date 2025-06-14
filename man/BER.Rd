% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BER.R
\name{BER}
\alias{BER}
\title{Beta Rectangular distribution}
\usage{
BER(mu.link = "logit", sigma.link = "log", nu.link = "logit")
}
\arguments{
\item{mu.link}{defines the mu.link, with "logit" link as the default for the mu parameter.}

\item{sigma.link}{defines the sigma.link, with "log" link as the default for the sigma parameter.}

\item{nu.link}{defines the nu.link, with "logit" link as the default for the nu parameter.}
}
\value{
Returns a gamlss.family object which can be used to fit a
BER distribution in the \code{gamlss()} function.
}
\description{
The Beta Rectangular family
}
\details{
The Beta Rectangular distribution with parameters \code{mu},
\code{sigma} and \code{nu} has density given by

\eqn{f(x| \mu, \sigma, \nu) = \nu + (1 - \nu) b(x| \mu, \sigma)}

for \eqn{0 < x < 1}, \eqn{0 < \mu < 1}, \eqn{\sigma > 0} and \eqn{0 < \nu < 1}.
The function \eqn{b(.)} corresponds to the traditional beta distribution
that can be computed by \code{dbeta(x, shape1=mu*sigma, shape2=(1-mu)*sigma)}.
}
\examples{
# Example 1
# Generating some random values with
# known mu and sigma
y <- rBER(n=500, mu=0.5, sigma=10, nu=0.5)

# Fitting the model
library(gamlss)
mod1 <- gamlss(y~1, family=BER)

# Extracting the fitted values for mu, sigma and nu
# using the inverse link function
inv_logit <- function(x) 1/(1 + exp(-x))

inv_logit(coef(mod1, what="mu"))
exp(coef(mod1, what="sigma"))
inv_logit(coef(mod1, what="nu"))

# Example 2
# Generating random values under some model

# A function to simulate a data set with Y ~ BER
gendat <- function(n) {
  x1 <- runif(n, min=0.4, max=0.6)
  x2 <- runif(n, min=0.4, max=0.6)
  x3 <- runif(n, min=0.4, max=0.6)
  mu    <- inv_logit(-0.5 + 1*x1)
  sigma <- exp(-1 + 4.8*x2)
  nu    <- inv_logit(-1 + 0.5*x3)
  y <- rBER(n=n, mu=mu, sigma=sigma, nu=nu)
  data.frame(y=y, x1=x1, x2=x2, x3=x3)
}

set.seed(1234)
datos <- gendat(n=500)

mod2 <- gamlss(y~x1, sigma.fo=~x2, nu.fo=~x3,
               family=BER, data=datos,
               control=gamlss.control(n.cyc=500, trace=TRUE))

summary(mod2)

}
\references{
Bayes, C. L., Bazán, J. L., & García, C. (2012). A new robust
regression model for proportions. Bayesian Analysis, 7(4), 841-866.
}
\seealso{
\link{dBER}
}
\author{
Karina Maria Garay, \email{kgarayo@unal.edu.co}
}
