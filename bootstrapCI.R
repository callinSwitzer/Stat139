# bootstrap confidence interval

x <- rnorm(100)
y <- rnorm(100, mean = 1)

t.test(y,x, var.equal = T)


bs <- function(o){
     ss <- sample(x, 1000, replace = T)
     tt <- sample(y, 1000, replace = T)
    abs( mean(ss) - mean(tt))
}
bob <- replicate(10000, bs())
quantile(bob, probs = c(0.025, 0.975))


