summary(Data.larch)

Data.larch=Data.larch[,c("Y", "X3", "X4", "X5", "X9")]
dim(Data.larch)
# [1] 50  5

pairs(Data.larch)
# No obvious pattern except Y~X5

# Best subset variable selection
regfit.best.larch = regsubsets(Y~., Data.larch, nvmax=4)
summary(regfit.best.larch)
# Selection Algorithm: exhaustive
#          X3  X4  X5  X9 
# 1  ( 1 ) " " " " "*" " "
# 2  ( 1 ) "*" " " "*" " "
# 3  ( 1 ) "*" "*" "*" " "
# 4  ( 1 ) "*" "*" "*" "*"

# Forward stepwise variable selection
regfit.forward.larch = regsubsets(Y~., Data.larch, nvmax=4, method="forward")
summary(regfit.forward.larch)
plot(regfit.forward.larch, scale="r2")

# Backward stepwise variable selection
regfit.backward.larch = regsubsets(Y~., Data.larch, nvmax=4, method="backward")
summary(regfit.backward.larch)
plot(regfit.backward.larch, scale="r2")

# 5-fold cross-validation
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
# 4

lm.fit.larch=lm(Y~.,data=Data.larch)
par(mfrow=c(2,2))
plot(lm.fit.larch)
# Don't seem to have outliers or interacting terms

summary(lm.fit.larch)
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  5.22693    1.60806   3.250  0.00218 ** 
# X3           1.08888    0.09683  11.245 1.16e-14 ***
# X4          -0.94163    0.10946  -8.603 4.65e-11 ***
# X5           6.42197    0.23159  27.730  < 2e-16 ***
# X9           0.90268    0.14095   6.404 7.82e-08 ***
# ...
# Multiple R-squared:  0.9593,	Adjusted R-squared:  0.9557  