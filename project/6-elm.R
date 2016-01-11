dim(Data.elm)
summary(Data.elm)

Data.elm=Data.elm[,c("Y", "X3", "X8")]
pairs(Data.elm)
# This is a classification problem
summary(as.factor(Data.elm$Y))

par(mfrow=c(1,1))
plot(Data.elm$X3[Data.elm$Y==1],Data.elm$X8[Data.elm$Y==1], pch=1, xlim = c(-10,10), ylim=c(-10,10), col="red")
par(new=T)
plot(Data.elm$X3[Data.elm$Y==0],Data.elm$X8[Data.elm$Y==0], pch=3, xlim = c(-10,10), ylim=c(-10,10), axes = F, ylab = "", xlab="", col="blue")
# TODO fit the axe label

# training set
# make it reproducible
set.seed(2016)
train=sample(c(TRUE,FALSE), 100, replace=T)
test=!train

# logistic regression
glm.fit.elm = glm(Y~., Data.elm, family="binomial", subset=train)
summary(glm.fit.elm)
glm.prob.elm=predict(glm.fit.elm, newdata=Data.elm[test,], type="response")
glm.pred.elm=rep(0, sum(test))
glm.pred.elm[glm.prob.elm>0.5]=1
head(glm.pred.elm)
mean(glm.pred.elm==Data.elm$Y[test])
# 0.902439

# LDA
library(MASS)
lda.fit.elm = lda(Y~., data=Data.elm, subset=train)
summary(lda.fit.elm)

lda.pred.elm=predict(lda.fit.elm, newdata=Data.elm[test,])
names(lda.pred.elm)
mean(lda.pred.elm$class==Data.elm$Y[test])
which(glm.pred.elm!=lda.pred.elm$class)
# also 0.902439

# QDA
qda.fit.elm = qda(Y~., data=Data.elm, subset=train)

qda.pred.elm=predict(qda.fit.elm, newdata=Data.elm[test,])
mean(qda.pred.elm$class==Data.elm$Y[test])
# also 0.902439...?
which(glm.pred.elm!=qda.pred.elm$class)

standardized.train.elm=scale(Data.elm[train,c("X3","X8")])
standardized.test.elm=scale(Data.elm[test,c("X3","X8")])

mean(standardized.train.elm[,"X3"])
var(standardized.train.elm[,"X3"])
mean(standardized.train.elm[,"X8"])
var(standardized.train.elm[,"X8"])

library(class)
set.seed(2015)
knn.1.pred.elm=knn(standardized.train.elm, standardized.test.elm, Data.elm[train,"Y"], k=1)
mean(knn.1.pred.elm==Data.elm$Y[test])
#0.8780488

set.seed(2015)
knn.3.pred.elm=knn(standardized.train.elm, standardized.test.elm, Data.elm[train,"Y"], k=3)
mean(knn.3.pred.elm==Data.elm$Y[test])
#0.8780488

set.seed(2015)
knn.10.pred.elm=knn(standardized.train.elm, standardized.test.elm, Data.elm[train,"Y"], k=10)
mean(knn.10.pred.elm==Data.elm$Y[test])
#0.8780488
all(knn.1.pred.elm==knn.3.pred.elm)
all(knn.1.pred.elm==knn.10.pred.elm)
which(knn.1.pred.elm!=knn.10.pred.elm)

