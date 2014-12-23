#########################
## Stat 139 Section 11 ##
## 17 Nov 2014         ##
#########################

## Problem 1
# full model: y <- 25 + 3*x1 + 4*x2 + 1.5*x1*x2
# y <- 25 + 3*x1    # when x2=0
# y <- 29 + 4.5*x1  # when x2=1
curve(25 + 3*x, from=1, to=100, xlab="Age (yrs)", ylab="Income ($10)", col="blue", main="Orange:: College     Blue:: HS")
curve(29 + 4.5*x, add=TRUE, col="orange")

## Problem 2
curve(104 + 2*x, from=0, to=300, ylim=c(-500, 1000), xlab="Weekly dark chocolate intake (grams)", ylab="Measure of blood flow", col="blue", main="Orange = BMI of 20; Blue = BMI of 30")
curve(99 - 2*x, add=TRUE, col="orange")



## Polynomial Model for Cow Data
cows <- read.table("data/cows.txt", header=TRUE)
attach(cows)

head(cows)
tail(cows)
plot(cows[ ,c(1,2,6)]) ##scatterplot matrix for Price, Age and Weight

# (b)
Wmat  <- cbind(Weight, Weight^2, Weight^3)
cor(Wmat)

# (c)
cWeight <- Weight-mean(Weight)
cWmat <- cbind(cWeight, cWeight^2, cWeight^3)
cor(cWmat)

# (d) Note that I() function changes the class of an object ‘as is’
model <- lm(Price~Age+cWeight+I(cWeight^2)+I(cWeight^3))
summary(model)

# (e)
model2 <- lm(Price~Age+cWeight+I(cWeight^2))
summary(model2)

# (f)
b0 <- model2$coef[1]
b1 <- model2$coef[2]
b2 <- model2$coef[3]
b3 <- model2$coef[4]
age <- 3
weight <- 11  # Weight is measured in 100s of lbs
avgweight <- mean(Weight)
b0 + b1*age + b2*(weight-avgweight) + b3*(weight-avgweight)^2

# (g)
newpt <- data.frame(age, weight-avgweight, (weight-avgweight)^2)
colnames(newpt) <- c("Age", "cWeight", "cWeight^2")
predict(model2, newpt, interval="confidence", level=.95)


