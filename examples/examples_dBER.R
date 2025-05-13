# Example 1
# Plotting the density function for different parameter values
curve(dBER(x, mu=0.5, sigma=10, nu=0),
      from=0, to=1, col="green", las=1, ylab="f(x)")

curve(dBER(x, mu=0.5, sigma=10, nu=0.2),
      add=TRUE, col= "blue1")

curve(dBER(x, mu=0.5, sigma=10, nu=0.4),
      add=TRUE, col="yellow")

curve(dBER(x, mu=0.5, sigma=10, nu=0.6),
      add=TRUE, col="red")

legend("topleft", col=c("green", "blue1", "yellow", "red"),
       lty=1, bty="n",
       legend=c("mu=0.5, sigma=10, nu=0",
                "mu=0.5, sigma=10, nu=0.2",
                "mu=0.5, sigma=10, nu=0.4",
                "mu=0.5, sigma=10, nu=0.6"))


curve(dBER(x, mu=0.3, sigma=10, nu=0),
       from=0, to=1, col="green", las=1, ylab="f(x)")

curve(dBER(x, mu=0.3, sigma=10, nu=0.2),
       add=TRUE, col= "blue1")

curve(dBER(x, mu=0.3, sigma=10, nu=0.4),
       add=TRUE, col="yellow")

curve(dBER(x, mu=0.3, sigma=10, nu=0.6),
       add=TRUE, col="red")

legend("topright", col=c("green", "blue1", "yellow", "red"),
       lty=1, bty="n",
       legend=c("mu=0.5, sigma=10, nu=0",
                "mu=0.5, sigma=10, nu=0.2",
                "mu=0.5, sigma=10, nu=0.4",
                "mu=0.5, sigma=10, nu=0.6"))


# Example 2
# Checking if the cumulative curves converge to 1
curve(pBER(x, mu=0.5, sigma=10, nu=0),
       from=0, to=1, col="green", las=1, ylab="f(x)")

curve(pBER(x, mu=0.5, sigma=10, nu=0.2),
       add=TRUE, col= "blue1")

curve(pBER(x, mu=0.5, sigma=10, nu=0.4),
       add=TRUE, col="yellow")

curve(pBER(x, mu=0.5, sigma=10, nu=0.6),
       add=TRUE, col="red")

legend("topleft", col=c("green", "blue1", "yellow", "red"),
       lty=1, bty="n",
       legend=c("mu=0.5, sigma=10, nu=0",
                "mu=0.5, sigma=10, nu=0.2",
                "mu=0.5, sigma=10, nu=0.4",
                "mu=0.5, sigma=10, nu=0.6"))

# Example 3
# Checking the quantile function
mu <- 0.5
sigma <- 10
nu <- 0.4
p <- seq(from=0.01, to=0.99, length.out=100)
plot(x=qBER(p, mu=mu, sigma=sigma, nu=nu), y=p,
     xlab="Quantile", las=1, ylab="Probability")
curve(pBER(x, mu=mu, sigma=sigma, nu=nu), add=TRUE, col="red")

# Example 4
# Comparing the random generator output with
# the theoretical density
x <- rBER(n= 10000, mu=0.5, sigma=10, nu=0.1)
hist(x, freq=FALSE)
curve(dBER(x, mu=0.5, sigma=10, nu=0.1),
      col="tomato", add=TRUE)

