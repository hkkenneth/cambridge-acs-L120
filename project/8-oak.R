# ====== oak ======

dim(Data.oak)
summary(Data.oak)
Data.oak=Data.oak[,c("Y", "X3")]
plot(Data.oak$X3, Data.oak$Y)
# ...
plot((Data.oak$X3)^3, Data.oak$Y) # this still shows some non-linearity
plot((Data.oak$X3)^4, Data.oak$Y) # these are hard to distinguish
plot((Data.oak$X3)^5, Data.oak$Y)
lm.fit.oak=lm(Y~poly(X3, 8, raw=T), data=Data.oak)
# no one is significant...
summary(lm.fit.oak)

library(glmnet)
set.seed(1)
Data.oak.xmat=model.matrix(Y~poly(X3, 8, raw=T)+0, Data.oak)
lasso.fit.oak=cv.glmnet(Data.oak.xmat,Data.oak$Y,alpha=1)
plot(lasso.fit.oak)
predict(lasso.fit.oak, s=lasso.fit.oak$lambda.min, type="coefficients")

# This gives very different answer
Data.oak.xmat.2=model.matrix(Y~poly(X3, 8)+0, Data.oak)
lasso.fit.oak.2=cv.glmnet(Data.oak.xmat.2,Data.oak$Y,alpha=1)
predict(lasso.fit.oak.2, s=lasso.fit.oak.2$lambda.min, type="coefficients")

library(leaps)
best.fit=regsubsets(Y~poly(X3,8,raw=T),data=Data.oak)
summary(best.fit)

# The following makes no sense...
k=5
#set.seed(1)
#folds=sample(1:k, nrow(Data.oak), replace=T)
folds=rep(c(1:k), nrow(Data.oak)/k)
cv.errors=matrix(NA, k, 8, dimnames=list(NULL, paste(1:8)))

predict.regsubsets=function(object, newdata, id,...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form, newdata)
  coefi=coef(object, id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}

for (j in 1:k) {
  best.fit=regsubsets(Y~poly(X3, 8, raw=T),data=Data.oak[folds!=j,])
  for (i in 1:8) {
    pred=predict.regsubsets(best.fit,Data.oak[folds==j,],id=i)
    cv.errors[j,i]=mean((Data.oak$Y[folds==j]-pred)^2)
  }
}
mean.cv.errors=apply(cv.errors, 2, mean)
which.min(mean.cv.errors)
# this gives 3
mean.cv.errors

best.subset.oak=regsubsets(Y~poly(X3, 8, raw=T),data=Data.oak)
summary(best.subset.oak)
coefficients(best.subset.oak, id = 3)

reg.summary=summary(best.subset.oak)

par(mfrow=c(2,2))
plot(reg.summary$rss, xlab="Number of Variables", ylab="RSS", type="l")
plot(reg.summary$adjr2, xlab="Number of Variables", ylab="Adjusted RSq", type="l")
points(2,reg.summary$adjr2[2], col="red", cex=2, pch=20)
plot(reg.summary$cp, xlab="Number of Variables", ylab="Cp", type="l")
points(2,reg.summary$cp[2], col="red", cex=2, pch=20)
plot(reg.summary$bic, xlab="Number of Variables", ylab="BIC", type="l")
points(2,reg.summary$bic[2], col="red", cex=2, pch=20)

reg.summary=summary(best.fit)
which.max(reg.summary$adjr2)
which.min(reg.summary$cp)
which.min(reg.summary$bic)

# raw=T is very important

# TODO what can we say about this?

lm.fit.oak=lm(Y~X3+I(X3^2)+I(X3^4), data=Data.oak)
summary(lm.fit.oak)

