pe <- read.csv("data/florida.csv")

pe <- pe[ pe$county != "Palm Beach",]

plot(pe$buch~pe$bush)


plot(log(pe$buch)~log(pe$bush))


mod1 <- lm(log(pe$buch)~log(pe$bush))
par(mfrow = c(2,2))
plot(mod1, which = 1:4)
par(mfrow = c(1,1))

plot(mod1$residuals~ log(pe$bush))

# for simple linear regression, you can look at residuals vs fitted or residuals vs xvalues
x <- 1:100
y <- sqrt(1:100)
plot(y~x)
modx <- lm(y~x)
plot(modx$residuals~x)
plot(modx$residuals~modx$fitted.values)
hist(modx$residuals)


# sigma hat = Residual Standard error
Xbar <- mean(log(pe$bush))
ybar <- mean(log(pe$buch))
sx2 <- var(log(pe$bush))
sy2 <- var(log(pe$buch))
rxy <- cor(log(pe$buch),log(pe$bush))
summary(mod1)
rxy^2 # check R^2
rxy*sqrt(sy2/sx2) #B1 check
ybar -(rxy*sqrt(sy2/sx2))*Xbar # check B0

# #8
# how to predict
x <- log(pe$bush)
y <- log(pe$buch)
mod3 <- lm(y~x)
newx <- log(152951)#seq(from = min(x), max(x), length.out = 100)
dat <- predict(mod3, newdata=data.frame(x=newx), 
               interval = c("confidence"), type="response")
dat
# this is the same as entering the numbers
exp(-2.32 + 0.73*log(152951))

# #9
pe <- read.csv("data/florida.csv")

#pe <- pe[ pe$county != "Palm Beach",]

plot(pe$buch~pe$bush, ylim = c(0, 5000))
points(pe[ pe$county == "Palm Beach",2], pe[ pe$county == "Palm Beach",4], 
       col = "red", pch = 20)
