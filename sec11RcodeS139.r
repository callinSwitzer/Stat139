cow <- read.table("data/cows.txt", sep = "\t", header = T)

pairs(cow[c("Price", "Age", "Weight")])

cor(cbind(cow$Weight, cow$Weight^2, cow$Weight^3))

cWeight <- cow$Weight - mean(cow$Weight)

cor(cbind(cWeight, cWeight^2 , cWeight^3))

modc1 <- lm(cow$Price~cow$Age + cow$Weight + I(cow$Weight^2) + I(cow$Weight^3))
summary(modc1)
require(car)
vif(modc1)

modc <- lm(cow$Price~cow$Age + cWeight + I(cWeight^2) + I(cWeight^3))
summary(modc)
vif(modc)
range(cows$Weight)

modcr <- lm(cow$Price~cow$Age + cWeight + I(cWeight^2))
summary(modcr)
anova(modc, modcr)

