# Example 1
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
curve(qUHLG(x, mu=0.25),
      from=0.001, to=0.999, col="green", las=1, ylab="Q(p)")

curve(qUHLG(x, mu=0.7),
      add=TRUE, col="blue1")

curve(qUHLG(x, mu=1.8),
      add=TRUE, col="black")

curve(qUHLG(x, mu=2.2),
      add=TRUE, col="red")

legend("topleft", col=c("green", "blue1", "yellow", "red"),
       lty=1, bty="n",
       legend=c("mu=0.25",
                "mu=0.7",
                "mu=1.8",
                "mu=2.2"))

# Example 4
x <- rUHLG(n=10000, mu=0.5)
hist(x, freq=FALSE, main="Histograma de rUHLG",
     xlab="x", ylab="Density")

curve(dUHLG(x, mu=0.5),
      col="tomato", add=TRUE, from=min(x), to=max(x))

