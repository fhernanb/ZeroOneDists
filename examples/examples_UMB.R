# Example 1
# Generating some random values with
# known mu
y <- rUMB(n=300, mu=0.5)

# Fitting the model
library(gamlss)
mod1 <- gamlss(y~1, family=UMB,
               control=gamlss.control(n.cyc=500, trace=FALSE))

# Extracting the fitted values for mu
# using the inverse link function
exp(coef(mod1, what="mu"))

# Example 2
# Generating random values under some model

# A function to simulate a data set with Y ~ UMB
gendat <- function(n) {
  x1 <- runif(n)
  mu <- exp(-0.5 + 1 * x1)
  y <- rUMB(n=n, mu=mu)
  data.frame(y=y, x1=x1)
}

datos <- gendat(n=300)

mod2 <- gamlss(y~x1,
               family=UMB, data=datos,
               control=gamlss.control(n.cyc=500, trace=TRUE))
summary(mod2)

