dim(Data.beech)
summary(Data.beech)

Data.beech=Data.beech[,c("Y", "X3", "X7")]
pairs(Data.beech)

plot(Data.beech$X3[Data.beech$Y==1],Data.beech$X7[Data.beech$Y==1], pch=1, xlim = c(-1,1), ylim=c(-1,1), col="red")
par(new=T)
plot(Data.beech$X3[Data.beech$Y==0],Data.beech$X7[Data.beech$Y==0], pch=3, xlim = c(-1,1), ylim=c(-1,1), axes = F, ylab = "", xlab="", col="blue")

# This is classification problem
summary(as.factor(Data.beech$Y))

# training set
# make it reproducible
set.seed(2016)
train=sample(c(TRUE,FALSE), 500, replace=T)
test=!train

# logistic regression
glm.fit.beech = glm(Y~., Data.beech, family="binomial", subset=train)
summary(glm.fit.beech)
glm.prob.beech=predict(glm.fit.beech, newdata=Data.beech[test,], type="response")
glm.pred.beech=rep(0, sum(test))
glm.pred.beech[glm.prob.beech>0.5]=1
head(glm.pred.beech)
mean(glm.pred.beech==Data.beech$Y[test])
# 0.896

# LDA
library(MASS)
lda.fit.beech = lda(Y~., data=Data.beech, subset=train)
summary(lda.fit.beech)

lda.pred.beech=predict(lda.fit.beech, newdata=Data.beech[test,])
names(lda.pred.beech)
mean(lda.pred.beech$class==Data.beech$Y[test])
# 0.908
# TODO: plot the decision boundary?
# class Y=0
mean.vector.0=c(mean(Data.beech$X3[Data.beech$Y==0]), mean(Data.beech$X7[Data.beech$Y==0]))
cov.0=cov(Data.beech$X3[Data.beech$Y==0], Data.beech$X7[Data.beech$Y==0])
# class Y=1
mean.vector.1=c(mean(Data.beech$X3[Data.beech$Y==1]), mean(Data.beech$X7[Data.beech$Y==1]))
cov.1=cov(Data.beech$X3[Data.beech$Y==1], Data.beech$X7[Data.beech$Y==1])
# mean.vector.0 and mean.vector.1 are quite different
# cov.0 and cov.1 are similar in value
lda.fit.beech$means
mean.vector.0
mean.vector.1


# QDA
qda.fit.beech = qda(Y~., data=Data.beech, subset=train)

qda.pred.beech=predict(qda.fit.beech, newdata=Data.beech[test,])
mean(qda.pred.beech$class==Data.beech$Y[test])
# 0.896

# standardization before KNN
standardized.train.beech=scale(Data.beech[train,c("X3","X7")])
standardized.test.beech=scale(Data.beech[test,c("X3","X7")])

# KNN
library(class)
set.seed(2015)
knn.1.pred.beech=knn(standardized.train.beech, standardized.test.beech, Data.beech[train,"Y"], k=1)
mean(knn.1.pred.beech==Data.beech$Y[test])
#0.836

set.seed(2015)
knn.3.pred.beech=knn(standardized.train.beech, standardized.test.beech, Data.beech[train,"Y"], k=3)
mean(knn.3.pred.beech==Data.beech$Y[test])
#0.876

knn.10.pred.beech=knn(standardized.train.beech, standardized.test.beech, Data.beech[train,"Y"], k=10)
mean(knn.10.pred.beech==Data.beech$Y[test])
#0.88

knn.50.pred.beech=knn(standardized.train.beech, standardized.test.beech, Data.beech[train,"Y"], k=50)
mean(knn.50.pred.beech==Data.beech$Y[test])
#0.86

knn.100.pred.beech=knn(standardized.train.beech, standardized.test.beech, Data.beech[train,"Y"], k=100)
mean(knn.100.pred.beech==Data.beech$Y[test])
#0.852

# This CV is not the same thing as before...
knn.10.cv.pred.beech=knn.cv(scale(Data.beech[,c("X3","X7")]), Data.beech$Y, k=10)
mean(knn.10.cv.pred.beech==Data.beech$Y)

# TODO: real CV to choose K?
# TODO: make a guess about the boundary..