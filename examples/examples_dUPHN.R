# Example 1
# Plotting the density function for different parameter values
curve(dUPHN(x, mu=1, sigma=1), ylim=c(0, 5),
      from=0.01, to=0.99, col="black", las=1, ylab="f(x)")

curve(dUPHN(x, mu=2, sigma=1),
      add=TRUE, col= "red")

curve(dUPHN(x, mu=3, sigma=1),
      add=TRUE, col="seagreen")

curve(dUPHN(x, mu=4, sigma=1),
      add=TRUE, col="royalblue2")

legend("topright", col=c("black", "red", "seagreen", "royalblue2"),
       lty=1, bty="n",
       legend=c("mu=1, sigma=1",
                "mu=2, sigma=1",
                "mu=3, sigma=1",
                "mu=4, sigma=1"))

# Example 2
# Checking if the cumulative curves converge to 1
curve(pUPHN(x, mu=1, sigma=1),
       from=0.01, to=0.99, col="black", las=1, ylab="F(x)")

curve(pUPHN(x, mu=2, sigma=1),
       add=TRUE, col= "red")

curve(pUPHN(x, mu=3, sigma=1),
       add=TRUE, col="seagreen")

curve(pUPHN(x, mu=4, sigma=1),
       add=TRUE, col="royalblue2")

legend("topleft", col=c("black", "red", "seagreen", "royalblue2"),
       lty=1, bty="n",
       legend=c("mu=1, sigma=1",
                "mu=2, sigma=1",
                "mu=3, sigma=1",
                "mu=4, sigma=1"))

# Example 3
# Checking the quantile function
mu <- 2
sigma <- 3
p <- seq(from=0.01, to=0.99, length.out=100)
plot(x=qUPHN(p, mu=mu, sigma=sigma), y=p,
     xlab="Quantile", las=1, ylab="Probability")
curve(pUPHN(x, mu=mu, sigma=sigma), add=TRUE, col="red")

# Example 4
# Comparing the random generator output with
# the theoretical density
x <- rUPHN(n= 10000, mu=4, sigma=1)
hist(x, freq=FALSE)
curve(dUPHN(x, mu=4, sigma=1),
      col="tomato", add=TRUE, from=0.01, to=0.99)

