summary(Data.elm)

Data.elm=Data.elm[,c("Y", "X3", "X8")]
dim(Data.elm)
# [1] 100   3
pairs(Data.elm)

summary(as.factor(Data.elm$Y))  # This is a classification problem
#  0  1 
# 75 25

plot(Data.elm$X3[Data.elm$Y==1],Data.elm$X8[Data.elm$Y==1], pch=1, xlim = c(-10,10), ylim=c(-10,10), col="red", ylab="X8", xlab="X3", main="Elm dataset: X3 versus X8 per class")
par(new=T)
plot(Data.elm$X3[Data.elm$Y==0],Data.elm$X8[Data.elm$Y==0], pch=3, xlim = c(-10,10), ylim=c(-10,10), axes = F, ylab = "", xlab="", col="blue")
legend(5.0, -5.0, c("Y==1", "Y==0"), pch=c(1,3), col=c("red","blue"))

# Training/Test set approach
set.seed(2016)
train=sample(c(TRUE,FALSE), 100, replace=T)
test=!train

# Logistic Regression
glm.fit.elm = glm(Y~., Data.elm, family="binomial", subset=train)
summary(glm.fit.elm)
glm.prob.elm=predict(glm.fit.elm, newdata=Data.elm[test,], type="response")
glm.pred.elm=rep(0, sum(test))
glm.pred.elm[glm.prob.elm>0.5]=1
head(glm.pred.elm)
# [1] 1 1 1 0 0 0
mean(glm.pred.elm==Data.elm$Y[test])
# [1] 0.902439

# LDA
library(MASS)
lda.fit.elm = lda(Y~., data=Data.elm, subset=train)

lda.pred.elm=predict(lda.fit.elm, newdata=Data.elm[test,])
mean(lda.pred.elm$class==Data.elm$Y[test])
# [1] 0.902439
which(glm.pred.elm!=lda.pred.elm$class)
# integer(0)

# QDA
qda.fit.elm = qda(Y~., data=Data.elm, subset=train)

qda.pred.elm=predict(qda.fit.elm, newdata=Data.elm[test,])
mean(qda.pred.elm$class==Data.elm$Y[test])
# [1] 0.902439
which(glm.pred.elm!=qda.pred.elm$class)
# integer(0)

# standardization before KNN
standardized.train.elm=scale(Data.elm[train,c("X3","X8")])
standardized.test.elm=scale(Data.elm[test,c("X3","X8")])
# Verify standardization
mean(standardized.train.elm[,"X3"])
# [1] 9.919531e-18
var(standardized.train.elm[,"X3"])
# [1] 1
mean(standardized.train.elm[,"X8"])
# [1] -2.003018e-18
var(standardized.train.elm[,"X8"])
# [1] 1

library(class)
set.seed(2015)
knn.1.pred.elm=knn(standardized.train.elm, standardized.test.elm, Data.elm[train,"Y"], k=1)
mean(knn.1.pred.elm==Data.elm$Y[test])
# [1] 0.8780488

set.seed(2015)
knn.3.pred.elm=knn(standardized.train.elm, standardized.test.elm, Data.elm[train,"Y"], k=3)
mean(knn.3.pred.elm==Data.elm$Y[test])
# [1] 0.8780488

set.seed(2015)
knn.10.pred.elm=knn(standardized.train.elm, standardized.test.elm, Data.elm[train,"Y"], k=10)
mean(knn.10.pred.elm==Data.elm$Y[test])
# [1] 0.8780488

all(knn.1.pred.elm==knn.3.pred.elm)
# [1] TRUE
all(knn.1.pred.elm==knn.10.pred.elm)
# [1] FALSE
which(knn.1.pred.elm!=knn.10.pred.elm)
# [1] 11 15
