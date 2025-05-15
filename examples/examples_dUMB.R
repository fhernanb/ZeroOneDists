# Example 1
# Plotting the density function for different parameter values
curve(dUMB(x, mu=0.4), from=0, to=1,
      ylim=c(0, 12),
      col="green", las=1, ylab="f(x)")

curve(dUMB(x, mu=1),
      add=TRUE, col="blue1")

curve(dUMB(x, mu=2),
      add=TRUE, col="black")

curve(dUMB(x, mu=7),
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
curve(pUMB(x, mu=0.25),
      from=0, to=1, col="green", las=1, ylab="F(x)")

curve(pUMB(x, mu=0.9),
      add=TRUE, col="blue1")

curve(pUMB(x, mu=1.8),
      add=TRUE, col="black")

curve(pUMB(x, mu=2.2),
      add=TRUE, col="red")

legend("bottomright", col=c("green", "blue1", "black", "red"),
       lty=1, bty="n",
       legend=c("mu=0.25",
                "mu=0.9",
                "mu=1.8",
                "mu=2.2"))

# Example 3
# Checking the quantile function
mu <- 2
p <- seq(from=0, to=1, length.out=100)
plot(x=qUMB(p, mu=mu), y=p,
     xlab="Quantile", las=1, ylab="Probability")
curve(pUMB(x, mu=mu), add=TRUE, col="red")

# Example 4
# Comparing the random generator output with
# the theoretical density
x <- rUMB(n=1000, mu=0.5)
hist(x, freq=FALSE)
curve(dUMB(x, mu=0.5),
      col="tomato", add=TRUE, from=0, to=1)


