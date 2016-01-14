summary(Data.yew)
Data.yew=Data.yew[,c("Y", "X4")]

dim(Data.yew)
# [1] 50  2

plot(Data.yew$X4, Data.yew$Y)

Data.yew$Y[which(Data.yew$Y>500)]
# [1] 999 999
outliers.yew=which(Data.yew$Y>500)

plot(Data.yew$X4[-outliers.yew], Data.yew$Y[-outliers.yew])
lm.fit.clean.yew=lm(Y~X4, data=Data.yew[-outliers.yew,])
lm.fit.raw.yew=lm(Y~X4, data=Data.yew)
abline(lm.fit.clean.yew)
abline(lm.fit.raw.yew, col="red")
cor(Data.yew$X4, Data.yew$Y)
# [1] -0.1537
cor(Data.yew$X4[-outliers.yew], Data.yew$Y[-outliers.yew])
# [1] 0.9733695

plot(Data.yew$X4[-outliers.yew], Data.yew$Y[-outliers.yew])
lm.fit.raw.yew=lm(Y~X4, data=Data.yew)
# the outlier with X4 ~ mean(X4)
lm.fit.clean1.yew=lm(Y~X4, data=Data.yew[-outliers.yew[1],])
# the outlier with X4 ~ 0
lm.fit.clean2.yew=lm(Y~X4, data=Data.yew[-outliers.yew[2],])

plot(Data.yew$X4, Data.yew$Y)
abline(lm.fit.raw.yew, col="red")
abline(lm.fit.clean.yew, col="blue")
abline(lm.fit.clean1.yew, col="green")
abline(lm.fit.clean2.yew, col="orange")

# hatvalues (leverage points)
hatvalues(lm.fit.raw.yew)[outliers.yew]
#        110       1352 
# 0.02238737 0.07208742
summary(hatvalues(lm.fit.raw.yew))
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.02023 0.02311 0.03412 0.04000 0.05870 0.07567
