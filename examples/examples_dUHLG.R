# Example 1
# Plotting the density function for different parameter values
curve(dUHLG(x, mu=0.4), from=0.01, to=0.99,
      ylim=c(0, 5),
      col="green", las=1, ylab="f(x)")

curve(dUHLG(x, mu=1),
      add=TRUE, col="blue1")

curve(dUHLG(x, mu=2),
      add=TRUE, col="black")

curve(dUHLG(x, mu=7),
      add=TRUE, col="red")

legend("topright",
       col=c("green", "blue1", "black", "red"),
       lty=1, bty="n",
       legend=c("mu=0.4",
                "mu=1",
                "mu=2",
                "mu=7"))

# Example 2
# Checking if the cumulative curves converge to 1
curve(pUHLG(x, mu=0.25),
      from=0.001, to=0.999, col="green", las=1, ylab="F(x)")

curve(pUHLG(x, mu=0.7),
      add=TRUE, col="blue1")

curve(pUHLG(x, mu=1.8),
      add=TRUE, col="black")

curve(pUHLG(x, mu=2.2),
      add=TRUE, col="red")

legend("topleft", col=c("green", "blue1", "black", "red"),
       lty=1, bty="n",
       legend=c("mu=0.25",
                "mu=0.7",
                "mu=1.8",
                "mu=2.2"))

# Example 3
# Checking the quantile function
mu <- 2
p <- seq(from=0.01, to=0.99, length.out=100)
plot(x=qUHLG(p, mu=mu), y=p,
     xlab="Quantile", las=1, ylab="Probability")
curve(pUHLG(x, mu=mu), add=TRUE, col="red")

# Example 4
# Comparing the random generator output with
# the theoretical density
x <- rUHLG(n=10000, mu=0.5)
hist(x, freq=FALSE, main="Histograma de rUHLG",
     xlab="x", ylab="Density")
curve(dUHLG(x, mu=0.5),
      col="tomato", add=TRUE, from=0.01, to=0.99)

