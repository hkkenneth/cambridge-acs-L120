dim(Data.beech)
# [1] 500  12

summary(Data.beech)
summary(as.factor(Data.beech$Y))  # This is a classification problem
# 0   1 
# 215 285

Data.beech=Data.beech[,c("Y", "X3", "X7")]
pairs(Data.beech)

plot(Data.beech$X3[Data.beech$Y==1],Data.beech$X7[Data.beech$Y==1], pch=1, xlim = c(-1,1), ylim=c(-1,1), col="red", ylab="X7", xlab="X3", main="Beech dataset: X3 versus X7 per class")
par(new=T)
plot(Data.beech$X3[Data.beech$Y==0],Data.beech$X7[Data.beech$Y==0], pch=3, xlim = c(-1,1), ylim=c(-1,1), axes = F, ylab = "", xlab="", col="blue")
legend(0.5, 1.0, c("Y==1", "Y==0"), pch=c(1,3), col=c("red","blue"))

# Training/Test set approach
set.seed(2016)
train=sample(c(TRUE,FALSE), 500, replace=T)
test=!train

# Logistic regression
glm.fit.beech = glm(Y~., Data.beech, family="binomial", subset=train)
summary(glm.fit.beech)
glm.prob.beech=predict(glm.fit.beech, newdata=Data.beech[test,], type="response")
glm.pred.beech=rep(0, sum(test))
glm.pred.beech[glm.prob.beech>0.5]=1
head(glm.pred.beech)
# [1] 1 1 0 0 1 1
mean(glm.pred.beech==Data.beech$Y[test])
# [1] 0.896

# LDA
library(MASS)
lda.fit.beech = lda(Y~., data=Data.beech, subset=train)
summary(lda.fit.beech)

lda.pred.beech=predict(lda.fit.beech, newdata=Data.beech[test,])
mean(lda.pred.beech$class==Data.beech$Y[test])
# [1] 0.908

# Explore class Y=0 vs Y=1
mean.vector.0=c(mean(Data.beech$X3[Data.beech$Y==0]), mean(Data.beech$X7[Data.beech$Y==0]))
cov.0=cov(Data.beech$X3[Data.beech$Y==0], Data.beech$X7[Data.beech$Y==0])
mean.vector.1=c(mean(Data.beech$X3[Data.beech$Y==1]), mean(Data.beech$X7[Data.beech$Y==1]))
cov.1=cov(Data.beech$X3[Data.beech$Y==1], Data.beech$X7[Data.beech$Y==1])
# cov.0  (-0.0460) and cov.1 (-0.0448) are similar in value

lda.fit.beech$means
#            X3         X7
# 0 -0.04948641 -0.5504237
# 1  0.13556175  0.3382217

# mean.vector.0 and mean.vector.1 are quite different
mean.vector.0
# [1] -0.08364792 -0.55499231
mean.vector.1
# [1] 0.08894252 0.36488935

# QDA
qda.fit.beech = qda(Y~., data=Data.beech, subset=train)
qda.pred.beech=predict(qda.fit.beech, newdata=Data.beech[test,])
mean(qda.pred.beech$class==Data.beech$Y[test])
# [1] 0.896

# standardization before KNN
standardized.train.beech=scale(Data.beech[train,c("X3","X7")])
standardized.test.beech=scale(Data.beech[test,c("X3","X7")])

# KNN
library(class)
set.seed(2015)
knn.1.pred.beech=knn(standardized.train.beech, standardized.test.beech, Data.beech[train,"Y"], k=1)
mean(knn.1.pred.beech==Data.beech$Y[test])
# [1] 0.836

set.seed(2015)
knn.3.pred.beech=knn(standardized.train.beech, standardized.test.beech, Data.beech[train,"Y"], k=3)
mean(knn.3.pred.beech==Data.beech$Y[test])
# [1] 0.876

knn.10.pred.beech=knn(standardized.train.beech, standardized.test.beech, Data.beech[train,"Y"], k=10)
mean(knn.10.pred.beech==Data.beech$Y[test])
# [1] 0.88
