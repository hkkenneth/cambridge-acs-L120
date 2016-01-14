# 5.3.1

library(ISLR)
set.seed(1)
train=sample(392, 196)
length(train)
head(train)

attach(Auto)

lm.fit=lm(mpg~horsepower, data=Auto, subset=train)
mean((mpg-predict(lm.fit, Auto))[-train]^2)

lm.fit2=lm(mpg~poly(horsepower, 2), data=Auto, subset=train)
mean((mpg-predict(lm.fit2, Auto))[-train]^2)

lm.fit3=lm(mpg~poly(horsepower, 3), data=Auto, subset=train)
mean((mpg-predict(lm.fit3, Auto))[-train]^2)

set.seed(2)
# repeat lm.fit, lm.fit2, ...

# 5.3.2
library(boot)
glm.fit=glm(mpg~horsepower, data=Auto)
cv.err=cv.glm(Auto, glm.fit)
cv.err$delta
names(cv.err)
length(cv.err$delta)

cv.error=rep(0,5)
for (i in 1:5) {
  glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
  cv.error[i]=cv.glm(Auto,glm.fit)$delta[1]
}
cv.error

# 5.3.3

set.seed(17)
cv.error.10=rep(0,10)
cv.error2.10=rep(0,10)
for (i in 1:10){
  glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
  cv.error.10[i]=cv.glm(Auto, glm.fit,K=10)$delta[1]
  cv.error2.10[i]=cv.glm(Auto, glm.fit,K=10)$delta[2]
}
cbind(cv.error.10, cv.error2.10, cv.error.10-cv.error2.10)

# 5.3.4

alpha.fn=function(data,index){
  X=data$X[index]
  Y=data$Y[index]
  return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
}

alpha.fn(Portfolio, 1:100)
set.seed(1)
alpha.fn(Portfolio, sample(100,100,replace=T))

boot(Portfolio, alpha.fn, R=1000)
# Bootstrap v.s. K-fold? How are they used differently?

boot.fn=function(data,index)
  return(coef(lm(mpg~horsepower, data=data, subset=index)))
boot.fn(Auto, 1:392)
set.seed(1)
boot.fn(Auto, sample(392, 392, replace=T))
set.seed(1)
boot.res=boot(Auto, boot.fn, 1000)
boot.coef.1=rep(0, 1000)
boot.coef.2=rep(0, 1000)
set.seed(1)
for (i in 1:1000){
  tmp = boot.fn(Auto, sample(392, 392, replace=T))
  boot.coef.1[i] = tmp[1]
  boot.coef.2[i] = tmp[2]
}
mean1=mean(boot.coef.1)
sqrt((1/999)*sum((boot.coef.1-mean1)^2))
mean1
boot.res

mean2=mean(boot.coef.2)
sqrt((1/999)*sum((boot.coef.2-mean2)^2))
# um... they are similar but not exactly the same...

summary(lm(mpg~horsepower, data=Auto))$coef

# consider non-linearity
boot.fn=function(data,index)
  coefficients(lm(mpg~horsepower+I(horsepower^2),data=data, subset=index))
set.seed(1)
boot(Auto,boot.fn,1000)

summary(lm(mpg~horsepower+I(horsepower^2), data=Auto))

# Exercise
# 2e
1 - (1-1/100)^100
# 2f
1 - (1-1/10000)^10000
# 2g
y=rep(0, 10000)
for (i in 1:10000){
  y[i] = 1 - (1-1/i)^i
}
plot(1:10000, y)

# 2g more concise soln
pr = function(n) return(1 - (1 - 1/n)^n)
x = 1:1e+05
plot(x, pr(x))

# 2h
store=rep(NA, 10000)
for(i in 1:10000){
  store[i]=sum(sample(1:100, rep=T)==4)>0
}
mean(store)



# 5

set.seed(1)
glm.fit = glm(default~income+balance, data=Default, family=binomial)
glm.fit
dim(Default)
# 5b
train = sample(10000, 5000)
glm.fit = glm(default~income+balance, data=Default, family=binomial, subset=train)
glm.probs = predict(glm.fit, newdata = Default[-train,], type="response")
glm.pred = rep("No", 5000)
glm.pred[glm.probs>0.5] = "Yes"
table(glm.pred, Default$default[-train])
mean(glm.pred != Default$default[-train])

# 5d
train = sample(10000, 5000)
glm.fit = glm(default~income+balance+student, data=Default, family=binomial, subset=train)
glm.probs = predict(glm.fit, newdata = Default[-train,], type="response")
glm.pred = rep("No", 5000)
glm.pred[glm.probs>0.5] = "Yes"
table(glm.pred, Default$default[-train])
mean(glm.pred != Default$default[-train])

# 6
# 6a
glm.fit=glm(default~income+balance, data=Default, family=binomial)
summary(glm.fit)$coef
coef(glm.fit)

# 6b
boot.fn=function(data, index)
  return(coef(glm(default~income+balance, data=data, family=binomial, subset=index)))

# 6c
boot(Default, boot.fn, 100)

# 6d
# SE given by summary() seems larger

# 7
set.seed(1)
# 7a
glm.fit = glm(Direction~Lag1+Lag2, data=Weekly, family=binomial)
# 7b
glm.fit = glm(Direction~Lag1+Lag2, data=Weekly, family=binomial, subset=2:dim(Weekly)[1])
# this is a cleaner soln...
glm.fit = glm(Direction~Lag1+Lag2, data=Weekly[-1, ], family=binomial)
# 7c
predict(glm.fit, newdata=Weekly[1,], type="response")
Weekly[1,]$Direction
# 7d
res = rep(0, dim(Weekly)[1])
for (i in 1:dim(Weekly)[1]) {
  glm.fit = glm(Direction~Lag1+Lag2, data=Weekly[-i, ], family=binomial)
  isUp = (predict(glm.fit, newdata=Weekly[i,], type="response") > 0.5)
  if (isUp && Weekly[i,]$Direction == "Down")
      res[i] = 1
  if (!isUp && Weekly[i,]$Direction == "Up")
      res[i] = 1
}
sum(res)
# 7e
sum(res)/dim(Weekly)[1]


#8
#8a
set.seed(1)
y=rnorm(100)
x=rnorm(100)
y=x-2*x^2+rnorm(100)
# n=100, p=2
#8b
plot(x, y)
#8c
set.seed(1)
ds=data.frame(x,y)
glm.fit.i = glm(y~x, data=ds)
glm.fit.ii = glm(y~poly(x, 2), data=ds)
glm.fit.iii = glm(y~poly(x, 3), data=ds)
glm.fit.iv = glm(y~poly(x, 4), data=ds)
cv.glm(ds, glm.fit.i)$delta
cv.glm(ds, glm.fit.ii)$delta
cv.glm(ds, glm.fit.iii)$delta
cv.glm(ds, glm.fit.iv)$delta
#8d
set.seed(2)
# same - LOOCV does not have randomness

#8f
summary(glm.fit.i)
summary(glm.fit.ii)
summary(glm.fit.iii)
summary(glm.fit.iv)

#9a
library(MASS)
mean(Boston$medv)
#9b
sqrt(var(Boston$medv)/dim(Boston)[1])
#9c
boot.fn = function(data, index) {
  return(mean(data$medv[index]))
}
boot.res = boot(Boston, boot.fn, 1000)
# 9d
t.test(Boston$medv)
# 9e
median(Boston$medv)
# 9f
boot.fn = function(data, index) {
  return(median(data$medv[index]))
}
boot.res = boot(Boston, boot.fn, 1000)
boot.res
