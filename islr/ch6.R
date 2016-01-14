library(ISLR)
dim(Hitters)
sum(is.na(Hitters$Salary))
Hitters=na.omit(Hitters)

library(leaps)
attach(Hitters)
regfit.full = regsubsets(Salary~., Hitters)
summary(regfit.full)

regfit.full = regsubsets(Salary~., Hitters, nvmax=19)
summary(regfit.full)

reg.summary=summary(regfit.full)
names(reg.summary)
which.max(reg.summary$rsq)
which.min(reg.summary$rss)
which.min(reg.summary$cp)
which.max(reg.summary$adjr2)
which.min(reg.summary$bic)

par(mfrow=c(2,2))
plot(reg.summary$rss, xlab="Number of Variables", ylab="RSS", type="l")
plot(reg.summary$adjr2, xlab="Number of Variables", ylab="Adjusted RSq", type="l")
points(11,reg.summary$adjr2[11], col="red", cex=2, pch=20)
plot(reg.summary$cp, xlab="Number of Variables", ylab="Cp", type="l")
points(10,reg.summary$cp[10], col="red", cex=2, pch=20)
plot(reg.summary$bic, xlab="Number of Variables", ylab="BIC", type="l")
points(6,reg.summary$bic[6], col="red", cex=2, pch=20)

par(mfrow=c(1,1))
plot(regfit.full, scale="r2")
plot(regfit.full, scale="adjr2")
plot(regfit.full, scale="Cp")
plot(regfit.full, scale="bic")

# 6.5.2

regfit.forward = regsubsets(Salary~., Hitters, nvmax=19, method="forward")
summary(regfit.forward)
plot(regfit.forward, scale="r2")

regfit.bwd = regsubsets(Salary~., Hitters, nvmax=19, method="backward")
summary(regfit.bwd)

coef(regfit.full, 7)
coef(regfit.forward, 7)
coef(regfit.bwd, 7)

set.seed(1)
train=sample(c(TRUE,FALSE), nrow(Hitters), rep=T)
test=(!train)

regfit.best=regsubsets(Salary~.,data=Hitters[train,],nvmax=19)

test.mat=model.matrix(Salary~., data=Hitters[test,])
dim(test.mat)
dim(Hitters)
colnames(test.mat)
colnames(Hitters)
head(test.mat, n=5)
head(Hitters, n=5)
summary(Hitters$Division)
# model.matrix() would change the factor column to dummy variables..

val.errors=rep(NA, 19)
for (i in 1:19){
  coefi=coef(regfit.best, id=i)
  pred=test.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((Hitters$Salary[test]-pred)^2)
}
head(val.errors)
which.min(val.errors)

coef(regfit.best,10)

predict.regsubsets=function(object, newdata, id,...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form, newdata)
  coefi=coef(object, id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}

regfit.best=regsubsets(Salary~., data=Hitters, nvmax=19)
coef(regfit.best, 10)

k=10
set.seed(1)
folds=sample(1:k, nrow(Hitters), replace=T)
cv.errors=matrix(NA, k, 19, dimnames=list(NULL, paste(1:19)))

for (j in 1:k) {
  best.fit=regsubsets(Salary~.,data=Hitters[folds!=j,], nvmax=19)
  for (i in 1:19) {
    pred=predict.regsubsets(best.fit, Hitters[folds==j,],id=i)
    cv.errors[j,i]=mean((Hitters$Salary[folds==j]-pred)^2)
  }
}
mean.cv.errors=apply(cv.errors, 2, mean)
which.min(mean.cv.errors)

par(mfrow=c(1,1))
plot(mean.cv.errors, type='b')


# 6.6
install.packages("glmnet")
library(glmnet)

x=model.matrix(Salary~., Hitters)[,-1]
y=Hitters$Salary

grid=10^seq(10,-2,length=100)
ridge.mod=glmnet(x,y,alpha=0,lambda=grid)
plot(ridge.mod)
names(ridge.mod)
#dim(ridge.mod$beta)
#length(ridge.mod$a0)
dim(coef(ridge.mod))

ridge.mod$lambda[50]
coef(ridge.mod)[,50]
sqrt(sum(coef(ridge.mod)[-1,50]^2))

ridge.mod$lambda[60]
coef(ridge.mod)[,60]
sqrt(sum(coef(ridge.mod)[-1,60]^2))

# fit for lambda=50
predict(ridge.mod, s=50, type="coefficients")[1:20,]

set.seed(1)
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
head(train)
head(test)

y.test=y[test]
ridge.mod=glmnet(x[train,], y[train], alpha=0, lambda=grid, thresh = 1e-12)
ridge.pred=predict(ridge.mod, s=4, newx=x[test,])
mean((ridge.pred-y.test)^2)

mean((mean(y[train])-y.test)^2)
ridge.max.pred=predict(ridge.mod, s=1e10, newx=x[test,])
mean((ridge.max.pred-y.test)^2)
# if lambda is so large, it is almost like just fitting with intercept

ridge.normal.pred=predict(ridge.mod, s=10, newx=x[test,])
mean((ridge.normal.pred-y.test)^2)

ridge.leastsq.pred=predict(ridge.mod, s=0, newx=x[test,], exact=T)
mean((ridge.leastsq.pred-y.test)^2)

set.seed(1)
cv.out=cv.glmnet(x[train,], y[train], alpha=0)
plot(cv.out)

summary(cv.out)
names(cv.out)
which.min(cv.out$cvm)
# min CV err
cv.out$lambda[76]
cv.out$lambda.min
# test err
ridge.pred=predict(ridge.mod, s=cv.out$lambda.min, newx=x[test,])
mean((ridge.pred-y.test)^2)

# re-fit with full data set
out=glmnet(x,y,alpha=0)
predict(out, type="coefficients", s=cv.out$lambda.min)[1:20,]

# Lasso
lasso.mod=glmnet(x[train,], y[train], alpha=1, lambda=grid)
plot(lasso.mod)

set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)

bestlam=cv.out$lambda.min
bestlam
lasso.pred=predict(lasso.mod, s=bestlam, newx = x[test,])
mean((lasso.pred-y.test)^2)

out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(out, type="coefficients", s=bestlam)[1:20,]
lasso.coef

# 6.7
install.packages("pls")
library(pls)
set.seed(2)
pcr.fit=pcr(Salary~., data=Hitters, scale=T, validation="CV")

summary(pcr.fit)
names(pcr.fit)
#pcr.fit$residuals

validationplot(pcr.fit, val.type="MSEP")

set.seed(1)
pcr.fit=pcr(Salary~., data=Hitters, subset=train, scale=T, validation="CV")
validationplot(pcr.fit, val.type="MSEP")

pcr.pred=predict(pcr.fit, x[test,], ncomp=7)
mean((pcr.pred-y.test)^2)

pcr.fit=pcr(y~x, scale=T, ncomp=7)
summary(pcr.fit)

# 6.7.2 PLS
set.seed(1)
pls.fit=plsr(Salary~., data=Hitters, subset=train, scale=TRUE, validation="CV")
summary(pls.fit)
validationplot(pls.fit, val.type="MSEP")

pls.pred=predict(pls.fit, x[test,], ncomp=2)
mean((pls.pred-y.test)^2)

pls.fit=plsr(Salary~., data=Hitters, scale=T, ncomp=2)
summary(pls.fit)






# 6.8 Exercise

# 8a
set.seed(1)
x = rnorm(100)
esp = rnorm(100)
y = 1+ 2*x + 3*x^2 + 4*x^3 + esp
Data=data.frame(y, poly(x,10))
head(Data)
fit = regsubsets(y~., data=Data,nvmax=11)
reg.summary=summary(fit)

par(mfrow=c(2,2))
plot(reg.summary$rss, xlab="Number of Variables", ylab="RSS", type="l")
plot(reg.summary$adjr2, xlab="Number of Variables", ylab="Adjusted RSq", type="l")
plot(reg.summary$cp, xlab="Number of Variables", ylab="Cp", type="l")
plot(reg.summary$bic, xlab="Number of Variables", ylab="BIC", type="l")
which.min(reg.summary$rss)
which.max(reg.summary$adjr2)
which.min(reg.summary$cp)
which.min(reg.summary$bic)

# soln
data.full = data.frame(y = y, x = x)
mod.full = regsubsets(y ~ poly(x, 10, raw = T), data = data.full, nvmax = 10)
mod.summary = summary(mod.full)
which.min(mod.summary$cp)
which.min(mod.summary$bic)
which.max(mod.summary$adjr2)

coefficients(mod.full, id = 4)

# 8d
mod.full = regsubsets(y ~ poly(x, 10, raw = T), data = data.full, nvmax = 10, method="forward")
mod.summary = summary(mod.full)
which.min(mod.summary$cp)
which.min(mod.summary$bic)
which.max(mod.summary$adjr2)
coefficients(mod.full, id = 4)

mod.full = regsubsets(y ~ poly(x, 10, raw = T), data = data.full, nvmax = 10, method="backward")
mod.summary = summary(mod.full)
which.min(mod.summary$cp)
which.min(mod.summary$bic)
which.max(mod.summary$adjr2)

# Wrong - this has no CV
lasso.mod=glmnet(poly(x, 10, raw=T), y, alpha=1, lambda=grid)
plot(lasso.mod)

# soln:
xmat = model.matrix(y ~ poly(x, 10, raw = T), data = data.full)[, -1]
mod.lasso = cv.glmnet(xmat, y, alpha = 1)
best.lambda = mod.lasso$lambda.min
best.lambda
plot(mod.lasso)
predict(mod.lasso, type="coefficients", s=best.lambda)


# 9
# 9a
set.seed(11)
dim(College)
train=sample(1:777, 776/2, replace = FALSE)
test=(-train)
head(test)

# 9b
lm.fit=lm(Apps~., data=College, subset=train)
plot(lm.fit)
lm.pred=predict(lm.fit, newdata=College[test,], type="response")
head(lm.pred, 3)
mean((lm.pred-College$Apps[test])^2)

# 9c
xmat = model.matrix(Apps~., data = College)[,]
xmat.train = xmat[train,]
xmat.test = xmat[test,]
dim(xmat)
cv.ridge.fit = cv.glmnet(xmat.train, College$Apps[train], alpha=0, lambda=grid, thresh=1e-12)
plot(cv.ridge.fit)
cv.ridge.fit$lambda.min
cv.ridge.pred = predict(cv.ridge.fit, newx = xmat.test, s=cv.ridge.fit$lambda.min)
mean((cv.ridge.pred - College$Apps[test])^2)

# 9c soln
train.mat = model.matrix(Apps~., data=College[train,])
test.mat = model.matrix(Apps~., data=College[test,])
grid = 10 ^ seq(4, -2, length=100)
mod.ridge = cv.glmnet(train.mat, College[train, "Apps"], alpha=0, lambda=grid, thresh=1e-12)
lambda.best = mod.ridge$lambda.min
lambda.best
ridge.pred = predict(mod.ridge, newx=test.mat, s=lambda.best)
mean((College[test, "Apps"] - ridge.pred)^2)


#9d
cv.lasso.fit = cv.glmnet(xmat.train, College$Apps[train], alpha=1, lambda=grid, thresh=1e-12)
cv.lasso.fit$lambda.min
cv.lasso.pred = predict(cv.lasso.fit, newx = xmat.test, s=cv.lasso.fit$lambda.min)
mean((cv.lasso.pred - College$Apps[test])^2)
predict(cv.lasso.fit, s=cv.lasso.fit$lambda.min, type="coefficients")

#10
set.seed(1)
