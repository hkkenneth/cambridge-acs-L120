library(ISLR)
names(Smarket)

dim(Smarket)

summary(Smarket)

cor(Smarket[,-9])

pairs(Smarket)

attach(Smarket)
plot(Volume)

glm.fit=glm(Direction~.-Year-Today, data=Smarket, family=binomial())
names(Smarket)

summary(glm.fit)

coef(glm.fit)
summary(glm.fit)$coef

glm.probs=predict(glm.fit, type="response")
glm.probs[1:10]

glm.pred=rep("Down", 1250)
glm.pred[glm.probs>0.5] = "Up"

summary(as.factor(glm.pred))
summary(Smarket$Direction)

table(glm.pred, Smarket$Direction)

attach(Smarket)
train=(Year<2005)
Smarket.2005=Smarket[!train,]
dim(Smarket.2005)
Direction.2005=Direction[!train]

glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket, family=binomial, subset=train)
glm.probs=predict(glm.fit, Smarket.2005,type="response")
length(glm.probs)
glm.pred=rep("Down", 252)
glm.pred[glm.probs >.5] = "Up"
table(glm.pred, Direction.2005)
mean(glm.pred==Direction.2005)



# 4.6.3

library(MASS)
lda.fit = lda(Direction~Lag1+Lag2, data=Smarket, subset=train)
lda.fit
par(mfrow=c(1,1))
plot(lda.fit)

lda.fit$means
lda.fit$scaling
# ...?
predict(lda.fit, type="response")$posterior[1,]
max(predict(lda.fit, newdata = Smarket.2005, type="response")$posterior[,1])

#4.6.4

qda.fit = qda(Direction~Lag1+Lag2, data=Smarket, subset=train)
qda.fit

qda.pred=predict(qda.fit, Smarket.2005)
table(qda.pred$class, Direction.2005)
mean(qda.pred$class==Direction.2005)


# 4.6.5
library(class)
train.X=cbind(Lag1, Lag2)[train,]
test.X=cbind(Lag1, Lag2)[!train,]
train.Direction=Direction[train]
set.seed(1)
knn.pred=knn(train.X, test.X, train.Direction, k=1)
mean(knn.pred==Direction.2005)
knn3.pred=knn(train.X, test.X, train.Direction, k=3)
mean(knn3.pred==Direction.2005)
knn10.pred=knn(train.X, test.X, train.Direction, k=10)
mean(knn10.pred==Direction.2005)

# just for fun....
tmp=knn.cv(train.X, train.Direction, k=3)
mean(tmp == train.Direction)


# 4.6.6
dim(Caravan)
attach(Caravan)
summary(Purchase)

names(Caravan)[86]

standardized.X=scale(Caravan[,-86])
var(Caravan[,1])
mean(Caravan[,1])
var(standardized.X[,1])
mean(standardized.X[,1])

test=1:1000
train.X=standardized.X[-test,]
test.X=standardized.X[test,]
train.Y=Purchase[-test]
test.Y=Purchase[test]
set.seed(1)
knn.pred=knn(train.X,test.X,train.Y,k=1)

mean(test.Y!=knn.pred)
mean(test.Y!="No")

# just for fun
dim(standardized.X)
test2=rep(FALSE, 5822)
test2[1:1000]=TRUE
train.X.2=standardized.X[-test,]
all(train.X==train.X.2)

# 6a
tmp = exp(-6+0.05*40 + 3.5)
tmp / (1 + tmp)

# q7
top=0.8*exp(-0.5)
bottom=top+ 0.2*exp(-16/72)
top/bottom

#q9a
0.37/1.37
#q9b
0.16/0.84

# q10
summary(Weekly)
dim(Weekly)
cor(Weekly[,-c(1,9)])

pairs(Weekly)

# 10b
glm.fit=glm(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Weekly, family=binomial)
summary(glm.fit)

# 10c
glm.probs=predict(glm.fit, type="response")
length(glm.probs)
contrasts(Weekly$Direction)
glm.pred=rep("Down", 1089)
glm.pred[glm.probs>0.5]="Up"
table(glm.pred, Weekly$Direction)
mean(glm.pred == Weekly$Direction)

# 10d
test=(Weekly$Year>2008)
glm.fit=glm(Direction~Lag2, data=Weekly, subset = -test, family = binomial)
summary(glm.fit)
glm.probs=predict(glm.fit, newdata=subset(Weekly, test), type="response")
length(glm.probs)
glm.pred=rep("Down", 104)
glm.pred[glm.probs>0.5]="Up"
table(glm.pred, Direction[test])

# 10e

test=(Weekly$Year>2008)
lda.fit=lda(Direction~Lag2, data=Weekly, subset = -test)
summary(lda.fit)
lda.pred=predict(lda.fit, newdata=subset(Weekly, test), type="response")
names(lda.pred)
table(lda.pred$class, Direction[test])

# 10f
test=(Weekly$Year>2008)
qda.fit=qda(Direction~Lag2, data=Weekly, subset = -test)
summary(qda.fit)
qda.pred=predict(qda.fit, newdata=subset(Weekly, test), type="response")
table(lda.pred$class, Direction[test])

# 10g
attach(Weekly)
set.seed(1)
knn.pred=knn(as.matrix(Lag2[!test]), as.matrix(Lag2[test]), Direction[!test], k=1)
table(knn.pred, Direction[test])


#11
attach(Auto)
mpg01 = mpg
mpg01[1:length(mpg)] = 1
mpg01[mpg<median(mpg)] = 0
summary(as.factor(mpg01))
Auto.new=data.frame(mpg01, Auto)
head(Auto.new, n=3)

# 12
Power = function() {
  2^3
}
v = Power()
