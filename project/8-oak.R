summary(Data.oak)

Data.oak=Data.oak[,c("Y", "X3")]
dim(Data.oak)
# [1] 50  2

par(mfrow=c(2,2))
plot(Data.oak$X3, Data.oak$Y)
# Non linearity observed, so try different powers
plot((Data.oak$X3)^3, Data.oak$Y)
plot((Data.oak$X3)^4, Data.oak$Y)
plot((Data.oak$X3)^5, Data.oak$Y)

library(glmnet)
set.seed(1)
Data.oak.xmat=model.matrix(Y~poly(X3, 8, raw=T)+0, Data.oak)
lasso.fit.oak=cv.glmnet(Data.oak.xmat,Data.oak$Y,alpha=1)
plot(lasso.fit.oak)
predict(lasso.fit.oak, s=lasso.fit.oak$lambda.min, type="coefficients")
# Lasso chooses X3^4 and X3^5

k=5
set.seed(1)
folds=sample(1:k, nrow(Data.oak), replace=T)
folds=rep(c(1:k), nrow(Data.oak)/k)
cv.errors=matrix(NA, k, 8, dimnames=list(NULL, paste(1:8)))
for (j in 1:k) {
  best.fit=regsubsets(Y~poly(X3, 8, raw=T),data=Data.oak[folds!=j,])
  for (i in 1:8) {
    pred=predict.regsubsets(best.fit,Data.oak[folds==j,],id=i)
    cv.errors[j,i]=mean((Data.oak$Y[folds==j]-pred)^2)
  }
}
mean.cv.errors=apply(cv.errors, 2, mean)
which.min(mean.cv.errors)
# 3
mean.cv.errors
#         1         2         3         4         5         6         7         8 
# 21.006542  6.915234  6.328698  6.739557  6.929283  7.092939  7.848067  8.051623

# Using all data to fit
best.subset.oak=regsubsets(Y~poly(X3, 8, raw=T),data=Data.oak)
summary(best.subset.oak)
coefficients(best.subset.oak, id = 3)
# (Intercept) poly(X3, 8, raw = T)1 poly(X3, 8, raw = T)2 poly(X3, 8, raw = T)4 
#   -1.415763            -13.167499              7.161567             55.573382

reg.summary=summary(best.subset.oak)

par(mfrow=c(2,2))
plot(reg.summary$rss, xlab="Number of Variables", ylab="RSS", type="l")
plot(reg.summary$adjr2, xlab="Number of Variables", ylab="Adjusted RSq", type="l")
points(2,reg.summary$adjr2[2], col="red", cex=2, pch=20)
plot(reg.summary$cp, xlab="Number of Variables", ylab="Cp", type="l")
points(2,reg.summary$cp[2], col="red", cex=2, pch=20)
plot(reg.summary$bic, xlab="Number of Variables", ylab="BIC", type="l")
points(2,reg.summary$bic[2], col="red", cex=2, pch=20)

lm.fit.oak=lm(Y~X3+I(X3^2)+I(X3^4), data=Data.oak)
summary(lm.fit.oak)
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   -1.416      1.908  -0.742    0.462    
# X3           -13.167     12.643  -1.042    0.303    
# I(X3^2)        7.162     19.538   0.367    0.716    
# I(X3^4)       55.573     10.190   5.454 1.89e-06 ***
