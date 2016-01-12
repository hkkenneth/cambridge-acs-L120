dim(Data.elder)
# [1] 500  12

summary(Data.elder)

Data.elder=Data.elder[,c("Y", "X7")]
plot(Data.elder$X7, Data.elder$Y)  # funnel shape

lm.fit.elder=lm(Y~X7,data=Data.elder)
par(mfrow=c(2,2))
plot(lm.fit.elder)
summary(lm.fit.elder)
# Multiple R-squared:  0.3359

lm.fit.log.elder=lm(log(Y)~X7,data=Data.elder)
par(mfrow=c(2,2))
plot(lm.fit.log.elder)
summary(lm.fit.log.elder)
# Multiple R-squared:  0.3305

lm.fit.sqrt.elder=lm(sqrt(Y)~X7,data=Data.elder)
par(mfrow=c(2,2))
plot(lm.fit.sqrt.elder)
summary(lm.fit.sqrt.elder)
# Multiple R-squared:  0.3373
# Coefficients:
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  7.11973    0.06499  109.55   <2e-16 ***
# X7           0.17570    0.01104   15.92   <2e-16 ***