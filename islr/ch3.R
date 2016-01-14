library(MASS)
library(ISLR)

lm.fit=lm(Boston$medv ~ Boston$lstat)
summary(lm.fit)

lm.fit=lm(Boston$medv ~ Boston$lstat + Boston$rm)
summary(lm.fit)

attach(Boston)
plot(lstat, medv)
abline(lm.fit, col="red")

par(mfrow=c(2,2))
plot(lm.fit)

par(mfrow=c(1,1))
plot(1:506, hatvalues(lm.fit))
abline(2/506, 0, col="red")

plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))
which.min(hatvalues(lm.fit))

# Lab 3.6.3 Multiple Linear Regression
lm.fit = lm(medv ~ ., data=Boston)
summary(lm.fit)

# R^2
summary(lm.fit)$r.sq
# RSE
summary(lm.fit)$sigma

library(car)
vif(lm.fit)

# Seems tax and rad are collinear
lm.fit = lm(medv ~ .-tax, data=Boston)
vif(lm.fit)

# 3.6.4
lm.fit=lm(medv~lstat*rm, data=Boston)
summary(lm.fit)
par(mfrow=c(2,2))
plot(lm.fit)

# 3.6.5
lm.fit2=lm(medv~lstat+I(lstat^2))
summary(lm.fit2)

lm.fit=lm(medv~lstat)

anova(lm.fit, lm.fit2)

lm.fit7=lm(medv ~ poly(lstat, 7))
summary(lm.fit7)

lm.fitlog=lm(medv ~ log(lstat), data=Boston)
summary(lm.fitlog)

# Ex. 8
library(ISLR)
attach(Auto)
lm.fit = lm(mpg ~ horsepower)
summary(lm.fit)

predict(lm.fit, data.frame(horsepower=c(98)), interval=c("prediction"))
predict(lm.fit, data.frame(horsepower=c(98)), interval=c("confidence"))

par(mfrow=c(1,1))
plot(mpg ~ horsepower)
abline(lm.fit, col="red")

par(mfrow=c(2, 2))
plot(lm.fit)

# Ex.9
pairs(Auto)
cor.matrix = cor(Auto[,-9])
# or cor(subset(Auto, select=-name))

# for fun
#cor.matrix[abs(cor.matrix) < 0.8] = NA
#cor.matrix

lm.fit = lm(mpg~.-name, data=Auto)
summary(lm.fit)

par(mfrow=c(2, 2))
plot(lm.fit)

which.max(hatvalues(lm.fit))

lm.fit=lm(mpg~.-name+displacement:weight+displacement:cylinders, data=Auto)
summary(lm.fit)

pairs(subset(Auto, select=-name))



lm.fit1 = lm(mpg~I(acceleration^2))
lm.fit2 = lm(mpg~I(sqrt(acceleration)))
lm.fit3 = lm(mpg~acceleration)
anova(lm.fit1, lm.fit2)
# Not significant?

lm.fit4 = lm(mpg~acceleration+I(acceleration^2))
lm.fit5 = lm(mpg~acceleration+log(acceleration))
lm.fit6 = lm(mpg~log(acceleration))
summary(lm.fit6)
summary(lm.fit3)

# However, 2 problems are observed from the above plots:
# 1) the residuals vs fitted plot indicates heteroskedasticity (unconstant variance over mean) in the model.
# 2) The Q-Q plot indicates somewhat unnormality of the residuals.

# 10

attach(Carseats)
lm.fit=lm(Sales ~ Price + Urban + US)
summary(lm.fit)

lm.fit2=lm(Sales ~ Price + US)
summary(lm.fit2)

# ***
confint(lm.fit2)

par(mfrow=c(2,2))
plot(lm.fit2)
par(mfrow=c(1,1))
# Threshold: abs < 3
plot(predict(lm.fit2), rstudent(lm.fit2))

# 11
set.seed(1)
x=rnorm(100)
y=2*x + rnorm(100)
lm.fit = lm(y~x+0)
summary(lm.fit)
confint(lm.fit)

lm.fit2=lm(x~y+0)
summary(lm.fit2)


#13

x = rnorm(100)
esp = rnorm(100, 0, sqrt(0.25))
y = -1 + 0.5 * x + esp
length(y)
par(mfrow=c(1,1))
plot(x, y)
lm.fit = lm(y~x)
summary(lm.fit)
confint(lm.fit)

plot(x, y)
abline(lm.fit, lwd=3, col=2)
abline(-1, 0.5, lwd=3, col=3)
legend(-1, legend = c("model fit", "pop. regression"), col=2:3, lwd=3)


lm.fit2 = lm(y~poly(x, 2))
summary(lm.fit2)$r.sq
summary(lm.fit)$r.sq

sum(residuals(summary(lm.fit2))^2)
sum(residuals(summary(lm.fit))^2)

# 14

set.seed(1)
x1=runif(100)
x2=0.5*x1 + rnorm(100)/10
y=2+ 2*x1 + 0.3*x2 + rnorm(100)

cor(x1,x2)
plot(x1, x2)

lm.fit = lm(y~x1+x2)
summary(lm.fit)
confint(lm.fit)

lm.fit2 = lm(y~x1)
summary(lm.fit2)
confint(lm.fit2)

lm.fit3 = lm(y~x2)
summary(lm.fit3)
confint(lm.fit3)

x1=c(x1, 0.1)
x2=c(x2, 0.8)
y=c(y,6)

