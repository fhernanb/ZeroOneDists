# Example 1
# Generating random values with
# known mu and sigma
require(gamlss)
mu <- 1.5
sigma <- 4.0

y <- rUPHN(1000, mu, sigma)

mod1 <- gamlss(y~1, sigma.fo=~1,  family=UPHN,
               control=gamlss.control(n.cyc=5000, trace=TRUE))

exp(coef(mod1, what="mu"))
exp(coef(mod1, what="sigma"))

# Example 2
# Generating random values under some model

# A function to simulate a data set with Y ~ UPHN
gendat <- function(n) {
  x1 <- runif(n)
  x2 <- runif(n)
  mu <- exp(0.75 - 0.69 * x1)   # Approx 1.5
  sigma <- exp(0.5 - 0.64 * x2) # Approx 1.20
  y <- rUPHN(n, mu, sigma)
  data.frame(y=y, x1=x1, x2=x2)
}

dat <- gendat(n=2000)

mod2 <- gamlss(y~x1, sigma.fo=~x2, family=UPHN, data=dat,
               control=gamlss.control(n.cyc=5000, trace=TRUE))

summary(mod2)
