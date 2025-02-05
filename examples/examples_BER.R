# Example 1
# Generating some random values with
# known mu and sigma
y <- rBER(n=500, mu=0.5, sigma=10, nu=0.1)

# Fitting the model
library(gamlss)
mod1 <- gamlss(y~1, family=BER,
               control=gamlss.control(n.cyc=500, trace=FALSE))

# Extracting the fitted values for mu, sigma and nu
# using the inverse link function
inv_logit <- function(x) 1/(1 + exp(-x))

inv_logit(coef(mod1, what="mu"))
exp(coef(mod1, what="sigma"))
inv_logit(coef(mod1, what="nu"))

