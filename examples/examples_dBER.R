# Example density function of the Beta Rectangular distribution
# Example 1
curve(dBER(x, mu=0.5, sigma=10, nu=0),
      from=0, to=1, col="green", las=1, ylab="f(x)")

curve(dBER(x, mu=0.5, sigma=10, nu=0.2),
      add=TRUE, col= "blue1")

curve(dBER(x, mu=0.5, sigma=10, nu=0.4),
      add=TRUE, col="yellow")

curve(dBER(x, mu=0.5, sigma=10, nu=0.6),
      add=TRUE, col="red")

legend("topleft", col=c("green", "blue1", "yellow", "red"), lty=1, bty="n",
       legend=c("mu=0.5, sigma=10, nu=0",
                "mu=0.5, sigma=10, nu=0.2",
                "mu=0.5, sigma=10, nu=0.4",
                "mu=0.5, sigma=10, nu=0.6"))

# Example 2
curve(dBER(x, mu=0.3, sigma=10, nu=0),
       from=0, to=1, col="green", las=1, ylab="f(x)")

curve(dBER(x, mu=0.3, sigma=10, nu=0.2),
       add=TRUE, col= "blue1")

curve(dBER(x, mu=0.3, sigma=10, nu=0.4),
       add=TRUE, col="yellow")

curve(dBER(x, mu=0.3, sigma=10, nu=0.6),
       add=TRUE, col="red")

legend("topright", col=c("green", "blue1", "yellow", "red"), lty=1, bty="n",
       legend=c("mu=0.5, sigma=10, nu=0",
                "mu=0.5, sigma=10, nu=0.2",
                "mu=0.5, sigma=10, nu=0.4",
                "mu=0.5, sigma=10, nu=0.6"))


# Cumulative Distribution Function of the Rectangular Beta Distribution
# Example 3
curve(pBER(x, mu=0.5, sigma=10, nu=0),
       from=0, to=1, col="green", las=1, ylab="f(x)")

curve(pBER(x, mu=0.5, sigma=10, nu=0.2),
       add=TRUE, col= "blue1")

curve(pBER(x, mu=0.5, sigma=10, nu=0.4),
       add=TRUE, col="yellow")

curve(pBER(x, mu=0.5, sigma=10, nu=0.6),
       add=TRUE, col="red")

legend("topleft", col=c("green", "blue1", "yellow", "red"), lty=1, bty="n",
       legend=c("mu=0.5, sigma=10, nu=0",
                "mu=0.5, sigma=10, nu=0.2",
                "mu=0.5, sigma=10, nu=0.4",
                "mu=0.5, sigma=10, nu=0.6"))

# Example 4
curve(pBER(x, mu=0.3, sigma=10, nu=0),
       from=0, to=1, col="green", las=1, ylab="f(x)")

curve(pBER(x, mu=0.3, sigma=10, nu=0.2),
       add=TRUE, col= "blue1")

curve(pBER(x, mu=0.3, sigma=10, nu=0.4),
       add=TRUE, col="yellow")

curve(pBER(x, mu=0.3, sigma=10, nu=0.6),
       add=TRUE, col="red")

legend("topleft", col=c("green", "blue1", "yellow", "red"), lty=1, bty="n",
       legend=c("mu=0.5, sigma=10, nu=0",
                "mu=0.5, sigma=10, nu=0.2",
                "mu=0.5, sigma=10, nu=0.4",
                "mu=0.5, sigma=10, nu=0.6"))


# Quantile Function of the Rectangular Beta Distribution
# Example 5
curve(qBER(x, mu=0.5, sigma=10, nu=0),
       from=0, to=1, col="green", las=1, ylab="f(x)")

curve(qBER(x, mu=0.5, sigma=10, nu=0.2),
       add=TRUE, col= "blue1")

curve(qBER(x, mu=0.5, sigma=10, nu=0.4),
       add=TRUE, col="yellow")

curve(qBER(x, mu=0.5, sigma=10, nu=0.6),
       add=TRUE, col="red")

legend("topleft", col=c("green", "blue1", "yellow", "red"), lty=1, bty="n",
       legend=c("mu=0.5, sigma=10, nu=0",
                "mu=0.5, sigma=10, nu=0.2",
                "mu=0.5, sigma=10, nu=0.4",
                "mu=0.5, sigma=10, nu=0.6"))

# Example 6
curve(qBER(x, mu=0.3, sigma=10, nu=0),
       from=0, to=1, col="green", las=1, ylab="f(x)")

curve(qBER(x, mu=0.3, sigma=10, nu=0.2),
       add=TRUE, col= "blue1")

curve(qBER(x, mu=0.3, sigma=10, nu=0.4),
       add=TRUE, col="yellow")

curve(qBER(x, mu=0.3, sigma=10, nu=0.6),
       add=TRUE, col="red")

legend("topright", col=c("green", "blue1", "yellow", "red"), lty=1, bty="n",
       legend=c("mu=0.5, sigma=10, nu=0",
                "mu=0.5, sigma=10, nu=0.2",
                "mu=0.5, sigma=10, nu=0.4",
                "mu=0.5, sigma=10, nu=0.6"))


# Random Deviations Function of the Rectangular Beta Distribution
x <- rBER(n= 10000, mu=0.5, sigma=10, nu=0.1)
hist(x, freq=FALSE)
curve(dBER(x, mu=0.5, sigma=10, nu=0.1),
      col="tomato", add=TRUE, from=0, to=1)

