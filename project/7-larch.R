dim(Data.larch)
summary(Data.larch)
Data.larch=Data.larch[,c("Y", "X3", "X4", "X5", "X9")]

# No obvious pattern except Y~X5
pairs(Data.larch)

regfit.best.larch = regsubsets(Y~., Data.larch, nvmax=4)
summary(regfit.best.larch)

regfit.forward.larch = regsubsets(Y~., Data.larch, nvmax=4, method="forward")
summary(regfit.forward.larch)
plot(regfit.forward.larch, scale="r2")

regfit.backward.larch = regsubsets(Y~., Data.larch, nvmax=4, method="backward")
summary(regfit.backward.larch)
plot(regfit.backward.larch, scale="r2")

predict.regsubsets=function(object, newdata, id,...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form, newdata)
  coefi=coef(object, id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}

k=5
set.seed(1)
folds=sample(1:k, nrow(Data.larch), replace=T)
cv.errors=matrix(NA, k, 4, dimnames=list(NULL, paste(1:4)))

for (j in 1:k) {
  best.fit.larch = regsubsets(Y~., Data.larch[folds!=j,], nvmax=4)
  for (i in 1:4) {
    pred=predict.regsubsets(best.fit.larch, Data.larch[folds==j,],id=i)
    cv.errors[j,i]=mean((Data.larch$Y[folds==j]-pred)^2)
  }
}
mean.cv.errors=apply(cv.errors, 2, mean)
which.min(mean.cv.errors)

lm.fit.larch=lm(Y~.,data=Data.larch)
par(mfrow=c(2,2))
# don't seem to have outliers or interacting terms...
plot(lm.fit.larch)
summary(lm.fit.larch)
