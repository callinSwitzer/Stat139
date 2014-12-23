
par(mfrow = c(1,2))
dd <- rbeta(10000, shape1 = 400, shape2 = 8)
hist(dd, breaks = 50, freq = F)
grid <- seq(min(dd), to = max(dd), length.out = 100)

# theoretical sampling distibution of the var is no longer normal
density = dnorm(grid, mean(dd),sd(dd))
lines(x = grid, density)

qqnorm(dd)
qqline(dd)


####
dev.off()
x <- 1:100 + rnorm(100)
y <- exp(.05*x + rnorm(100, sd = .1))
#y <- x^2 + rnorm(100, sd = 500)
plot(x,y)

mod1 <- lm(y~x)
abline(mod1)
mod1a <- lm(log(y) ~ x)
plot(mod1, which = 1)
plot(mod1a, which = 1)

# visualize log-transformed data
plot(x, log(y))
abline(mod1a)

# visualize line on original scale
plot(x,y, pch = 20)
curve(exp(mod1a$coefficients[1]  + mod1a$coefficients[2]*x), add = TRUE, col = "violet", lwd = 2)




x <- 1:100
x2 <- rnorm(100, mean = 50, sd = 20)
y <- rnorm(100, 10, 20) + x
foo <- data.frame(x, x2, y)
pairs(foo)

rsqT <- lm(y~x + x2, data = foo)
summary(rsqT)

1-19.42^2/var(y)

1 - (sum(rsqT$residuals^2)/(length(y) -  (2 + 1))) / (sum((y - mean(y))^2)/(length(y) - 1))


llm <- lm(y~x*x2)
summary(llm)

lmc <- lm(y~I(x-mean(x))*I(x2 - mean(x2)))
summary(lmc)



### transformations:
par(mfrow = c(1,2))
x <- 1:100
y <- log(rnorm(100, 10, 5) + x) + 20
plot(x,y, pch = 20)

expp <- lm(y~x)
abline(expp)
plot(expp, which = 1, pch = 20)

par(mfrow = c(1,2))
x <- 1:100
y <- 1/(rnorm(100, 10, 2) + .5*x)
plot(x,y, pch = 20)

expp <- lm(y~x)
abline(expp)
plot(expp, which = 1, pch = 20)

plot(x,log(y))



par(mfrow = c(1,2)) 
x <- 1:100
y <- 1/(rnorm(100, 10, 5) + .5*x)
plot(x,y, pch = 20)

expp <- lm(y~x)
abline(expp)
plot(expp, which = 1, pch = 20)

plot(x,1/y)


x <- exp(1:100*.03)
y <- x + rnorm(100)#exp(2 + rnorm(100, 0, .1) + .1*x)
plot(x,y, pch = 20)


expp <- lm(y~x)
abline(expp)
plot(expp, which = 1, pch = 20)

plot(x, log(y), pch=20)

plot(x,1/y)



dev.off()
x <- rnorm(100)
y <- x + rnorm(100, 0, .8)
cor(x,y)
plot(x,y, pch = 20)
foo <- lm(y~x)
abline(foo)
gg <- summary(foo)

gg$sigma*sqrt(1/(length(x)*var(x)))
gg$sigma*sqrt(1/(length(x)))




x <- 1:100*0.01
y <- .03/x + rnorm(100, 0, 0.3)
plot(x,y, pch = 20)
foo <- lm(y~x)
abline(foo)
plot(x, 1/(y))
foo1 <- lm(y~log(x))
plot(foo1, which = 1)

x <- c(21.5, 13.6, 13.4, 13.1, 11.4, 11, 9.4, 9, 7.4, 5.5, 3.9, -.1, -2.9, -3.8,-5.3)
mean(x)
sd(x)
