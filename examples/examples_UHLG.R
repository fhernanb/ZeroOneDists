# Example 1
# Generating some random values with
# known mu
y <- rUHLG(n=500, mu=7)

# Fitting the model
library(gamlss)
mod1 <- gamlss(y~1, family=UHLG,
               control=gamlss.control(n.cyc=500, trace=FALSE))

# Extracting the fitted values for mu, sigma and nu
# using the inverse link function
exp(coef(mod1, what="mu"))

# Example 2
# Generating random values under some model

# A function to simulate a data set with Y ~ UHLG
gendat <- function(n) {
  x1 <- runif(n, min=0.4, max=0.6)
  x2 <- runif(n, min=0.4, max=0.6)
  mu    <- exp(-0.5 + 3*x1 - 2.5*x2)
  y <- rUHLG(n=n, mu=mu)
  data.frame(y=y, x1=x1, x2=x2)
}

datos <- gendat(n=5000)

mod2 <- gamlss(y~x1+x2,
               family=UHLG, data=datos,
               control=gamlss.control(n.cyc=500, trace=TRUE))
summary(mod2)
