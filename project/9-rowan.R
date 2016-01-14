summary(Data.rowan)

Data.rowan=Data.rowan[,c("Y", "X4")]
dim(Data.rowan)
# [1] 50  2

plot(Data.rowan$X4, Data.rowan$Y)
# These observations do not contain any outliers or high leverage points

cor(Data.rowan$X4, Data.rowan$Y)
# [1] 0.9742865
# close-to-one Pearson correlation coefficient: high correlation between X4 and Y

# Simple linear regression
lm.fit.rowan=lm(Y~X4, data=Data.rowan)
summary(lm.fit.rowan)
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   2.1534     0.5917    3.64 0.000667 ***
# X4            3.0237     0.1009   29.96  < 2e-16 ***
# ...
# Multiple R-squared:  0.9492

abline(lm.fit.rowan, col="red")

# Various plots for verification, including residuals vs fitted values
par(mfrow=c(2,2))
plot(lm.fit.rowan)

# Curiosity check
lm.fit.rowan.5=lm(Y~poly(X4,5), data=Data.rowan)
summary(lm.fit.rowan.5)
# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  17.24188    0.31127  55.391   <2e-16 ***
# poly(X4, 5)1 65.77109    2.20104  29.882   <2e-16 ***
# poly(X4, 5)2  0.49983    2.20104   0.227   0.8214    
# poly(X4, 5)3 -0.57249    2.20104  -0.260   0.7960    
# poly(X4, 5)4 -0.09382    2.20104  -0.043   0.9662    
# poly(X4, 5)5  4.19547    2.20104   1.906   0.0632 .