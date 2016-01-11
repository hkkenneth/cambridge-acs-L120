dim(Data.elder)
summary(Data.elder)

Data.elder=Data.elder[,c("Y", "X7")]
plot(Data.elder$X7, Data.elder$Y)
# funnel shape

lm.fit.elder=lm(Y~X7,data=Data.elder)
par(mfrow=c(2,2))
plot(lm.fit.elder)
summary(lm.fit.elder)
# 0.3359

#lm.fit.elder.2=lm(Y~log(X7),data=Data.elder)
#par(mfrow=c(2,2))
#plot(lm.fit.elder.2)
# It gives a funny shape for Scale-Location

lm.fit.log.elder=lm(log(Y)~X7,data=Data.elder)
par(mfrow=c(2,2))
plot(lm.fit.log.elder)
summary(lm.fit.log.elder)
# 0.3305

lm.fit.sqrt.elder=lm(sqrt(Y)~X7,data=Data.elder)
par(mfrow=c(2,2))
plot(lm.fit.sqrt.elder)
summary(lm.fit.sqrt.elder)
# 0.3373

lm.fit.elder.5=lm(Y~I(X7^2),data=Data.elder)
par(mfrow=c(2,2))
plot(lm.fit.elder.5)
# This seems to get a straight line for residuals vs fitted and scale-location(upward slope)
summary(lm.fit.elder.5)

# Still only X7^1 is significant...
lm.fit.elder.6=lm(Y~poly(X7,3),data=Data.elder)
par(mfrow=c(2,2))
plot(lm.fit.elder.6)
summary(lm.fit.elder.6)

#TODO no idea how to fit this yet