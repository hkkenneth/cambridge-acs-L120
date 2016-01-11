par(mfrow=c(1,1))
dim(Data.ash)
summary(Data.ash)

Data.ash=Data.ash[,c(-1,-12)]

pairs(Data.ash[1:50,])
summary(Data.ash$X7)
plot(Y~X7, data=Data.ash)

lm.fit.ash=lm(Y~X7, data=Data.ash)
par(mfrow=c(2,2))
plot(lm.fit.ash)
summary(lm.fit.ash)
# R^2 is 0.9966... do we still need to use other vars?

lm.fit.2.ash=lm(Y~., data=Data.ash)
summary(lm.fit.2.ash)
# X3 seems significant too

plot(1:length(Data.ash$X2), Data.ash$X2)
hist(Data.ash$X2)

#TODO best subset or lasso...