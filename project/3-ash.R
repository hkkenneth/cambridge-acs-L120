summary(Data.ash)

Data.ash=Data.ash[,c(-1,-12)]  # Remove id and label
dim(Data.ash)
# [1] 500  10

pairs(Data.ash)  # Output omitted as data is too dense to be display on paper
plot(Y~X7, data=Data.ash)  # We observe a clear linear relationship

lm.fit.ash=lm(Y~X7, data=Data.ash)  # Apply simple linear regression onto 1 predictor
summary(lm.fit.ash)  # X7 is significant, R^2 is 0.9966

par(mfrow=c(2,2))
plot(lm.fit.ash)  # Plots for inspection

lm.fit.2.ash=lm(Y~., data=Data.ash)  # Apply linear regression onto all predictors
summary(lm.fit.2.ash)   # X3 and X7 are both significant, R^2 is 0.9988

library(leaps)  # for regsubsets()

k=5  # 5-fold cross-validation
set.seed(1)  # make random reproducible
folds=sample(1:k, nrow(Data.ash), replace=T)  # Train/Test set splitting
cv.errors=matrix(NA, k, 9, dimnames=list(NULL, paste(1:9)))
for (j in 1:k) {
  fold.fit.ash = regsubsets(Y~., Data.ash[folds!=j,], nvmax=9)
  for (i in 1:9) {
    pred=predict.regsubsets(fold.fit.ash, Data.ash[folds==j,], id=i)
    cv.errors[j,i]=mean((Data.ash$Y[folds==j]-pred)^2)
  }
}
mean.cv.errors=apply(cv.errors, 2, mean)
which.min(mean.cv.errors)
# 4

mean.cv.errors
# 1        2        3        4        5        6        7        8        9 
# 50.92948 17.93046 17.94481 17.91992 18.01526 18.01093 18.00138 18.01917 18.02701

best.fit.ash = regsubsets(Y~., Data.ash, nvmax=9)
coef(best.fit.ash, id=4)
# (Intercept)          X3          X4          X7          X8 
# 3.58870931  2.03535597  0.02220308 42.08023730  0.01376026

coef(best.fit.ash, id=2)
# (Intercept)          X3          X7 
# 4.542427    2.030122   42.077758