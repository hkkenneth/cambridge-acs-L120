# ====== yew ======

dim(Data.yew)
summary(Data.yew)
Data.yew=Data.yew[,c("Y", "X4")]
summary(Data.yew)
head(Data.yew)
plot(Data.yew$X4, Data.yew$Y)
Data.yew$Y[which(Data.yew$Y>500)]
outliers.yew=which(Data.yew$Y>500)
plot(Data.yew$X4[-outliers.yew], Data.yew$Y[-outliers.yew])
summary(Data.yew$Y[-outliers.yew])
lm.fit.clean.yew=lm(Y~X4, data=Data.yew[-outliers.yew,])
abline(lm.fit.clean.yew)
cor(Data.yew$X4, Data.yew$Y)
cor(Data.yew$X4[-outliers.yew], Data.yew$Y[-outliers.yew])

plot(Data.yew$X4[-outliers.yew], Data.yew$Y[-outliers.yew])
lm.fit.raw.yew=lm(Y~X4, data=Data.yew)
# the outlier with X4 ~ mean(X4)
lm.fit.clean1.yew=lm(Y~X4, data=Data.yew[-outliers.yew[1],])
# the outlier with X4 ~ 0
lm.fit.clean2.yew=lm(Y~X4, data=Data.yew[-outliers.yew[2],])


summary(lm.fit.raw.yew)
summary(lm.fit.clean1.yew)
summary(lm.fit.clean2.yew)
summary(lm.fit.clean.yew)

abline(lm.fit.yew, col="red")
abline(lm.fit.yew.2, col="blue")

plot(Data.yew$X4, Data.yew$Y)
abline(lm.fit.raw.yew, col="red")
abline(lm.fit.clean.yew, col="blue")
abline(lm.fit.clean1.yew, col="green")
abline(lm.fit.clean2.yew, col="orange")
# TODO compare the effect of outliers - quantify it with the use of leverage

# hatvalues
hatvalues(lm.fit.raw.yew)[outliers.yew]
summary(hatvalues(lm.fit.raw.yew))
