n <- 1000
x1 <- rnorm(n,0,1)
x2 <- rnorm(n,3,1)
p1 <- 0.75
p2 <- 1 - p1
# use n data from uniform distribution to construct
# the proportion of each parent distribution.
u <- runif(n)
k <- as.integer(u > p2)
x <- k * x1 + (1-k) * x2
hist(x, prob = T, 
     breaks = 50, 
     xlab = "mixture x",
     ylim = c(0,1),
     main = sprintf("p1=%s, p2=%s", p1, p2))
# using weighted sum of dnorm() to construct true density function
x_true <- seq(-10,10,0.1)
y_true <- p1*dnorm(x_true,0,1) + (1-p1)*dnorm(x_true,3,1)
lines(x_true,y_true,col="red")
#lines(x_range,y_prob, col= "red")