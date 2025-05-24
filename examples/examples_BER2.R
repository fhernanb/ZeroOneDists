# Example 1
# Generating some random values with
# known mu and sigma
y <- rBER2(n=500, mu=0.3, sigma=7, nu=0.4)

# Fitting the model
library(gamlss)
mod1 <- gamlss(y~1, family=BER2,
               control=gamlss.control(n.cyc=500, trace=FALSE))

# Extracting the fitted values for mu, sigma and nu
# using the inverse link function
inv_logit <- function(x) 1/(1 + exp(-x))

inv_logit(coef(mod1, what="mu"))
exp(coef(mod1, what="sigma"))
inv_logit(coef(mod1, what="nu"))

# Example 2
# Generating random values under some model

# A function to simulate a data set with Y ~ BER2
gendat <- function(n) {
  x1 <- runif(n, min=0.4, max=0.6)
  x2 <- runif(n, min=0.4, max=0.6)
  x3 <- runif(n, min=0.4, max=0.6)
  mu    <- inv_logit(-0.5 + 1*x1)
  sigma <- exp(-1 + 4.8*x2)
  nu    <- inv_logit(-1 + 0.5*x3)
  y <- rBER2(n=n, mu=mu, sigma=sigma, nu=nu)
  data.frame(y=y, x1=x1, x2=x2, x3=x3)
}

set.seed(123456)
datos <- gendat(n=5000)

mod2 <- gamlss(y~x1, sigma.fo=~x2, nu.fo=~x3,
               family=BER2, data=datos,
               control=gamlss.control(n.cyc=500, trace=TRUE))

summary(mod2)

