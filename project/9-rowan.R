# ====== rowan ======

dim(Data.rowan)
summary(Data.rowan)
Data.rowan=Data.rowan[,c("Y", "X4")]
# these observations do not contain any outliers or high leverage points
plot(Data.rowan$X4, Data.rowan$Y)
# close-to-one Pearson correlation coefficient: high correlation between X4 and Y
cor(Data.rowan$X4, Data.rowan$Y)
# simple linear regression
lm.fit.rowan=lm(Y~X4, data=Data.rowan)
summary(lm.fit.rowan)
abline(lm.fit.rowan)
# various plots for verification, including residuals vs fitted values
par(mfrow=c(2,2))
plot(lm.fit.rowan)
# curiosity check
lm.fit.rowan.5=lm(Y~poly(X4,5), data=Data.rowan)
summary(lm.fit.rowan.5)

# DONE?