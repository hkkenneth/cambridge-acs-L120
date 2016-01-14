x <- c(1, 6, 2)

ls()

length(x)

rm(x)

# All at once
rm(list=ls())

x <- matrix(data=c(1,2,3,4), nrow=2, ncol=2)

sqrt(x)

x <- rnorm(50)

y<-x+rnorm(50, mean=50, sd=.1)

cor(x,y)

# this gives much smaller correlation
y<-x+rnorm(50, mean=50, sd=10)

set.seed(1303)
rnorm(50)

set.seed(3)
y <- rnorm(100)
# mean
mean(y)
# variance
var(y)
# SD
sqrt(var(y))
sd(y)

# plotting

x <- rnorm(100)
y <- rnorm(100)

plot(x, y)
dev.off()

x <- seq(-pi, pi, length=50)
y <- x
f <- outer(x, y, function(x,y)cos(y)/(1+x^2))
contour(x, y, f)

fa <- (f-t(f))/2
contour(x, y, fa, nlevels=15)

image(x, y, fa)

persp(x, y, fa)

A=matrix(1:16,4,4)
dim(A)

# read.table()
# read.csv()

library(ISLR)
dim(Auto)

#?
Auto <- na.omit(Auto)

names(Auto)

Auto[1:4, ]

attach(Auto)
plot(cylinders, mpg)

cylinders <- as.factor(cylinders)

plot(cylinders, mpg, col="red")

hist(mpg, col=2)
hist(mpg, col=2, breaks=15)

pairs(Auto)
pairs(~ mpg + displacement + horsepower + weight + acceleration, Auto)

plot(horsepower, mpg)
identify(horsepower, mpg, name)

attach(College)

College
rownames(College)
College[,1 ]

Auto <- read.table("Auto.data")
# Error in check_for_XQuartz() :X11 library is missing: install XQuartz from xquartz.macosforge.org
fix(Auto)

head(Auto, n = 3)

Auto <- read.table("Auto.data", header=T, na.strings = "?")

Auto.naminus <- na.omit(Auto)

names(Auto)

attach(Auto)
plot(cylinders, mpg)

college <- read.csv("College.csv")
rownames(college) <- college[, 1]
college <- college[, -1]
summary(college)
pairs(college[,1:10])

attach(college)
plot(Outstate ~ Private)
plot(Private, Outstate)

Elite=rep("No", nrow(college))
Elite[Top10perc>50] = "Yes"
summary(Elite)
Elite=as.factor(Elite)
college=data.frame(college, Elite)

plot(Outstate ~ Elite)

# Fail
Band=rep("5", nrow(college))
Band[Top10perc>20] = "4"
Band[Top10perc>40] = "3"
Band[Top10perc>60] = "2"
Band[Top10perc>80] = "1"
Band=as.factor(Band)
hist(Band, Outstate)

par(mfrow=c(2,2))
hist(college$Apps)
hist(college$perc.alumni, col=2)
hist(college$S.F.Ratio, col=3, breaks=10)
hist(college$Expend, breaks=100)

par(mfrow=c(1,1))
plot(college$Outstate, college$Grad.Rate)
# High tuition correlates to high graduation rate.

plot(college$Accept / college$Apps, college$S.F.Ratio)


# Ex.9 Auto

summary(Auto.naminus)
# This is stupid
range(Auto.naminus[2])
# This is much smarter
sapply(Auto.naminus[, 1:7], range)
# v.s. the output is nicer than the default:
lapply(Auto.naminus[, 1:7], range)

sapply(Auto.naminus[, 1:7], mean)
sapply(Auto.naminus[, 1:7], sd)

Auto.subset = Auto.naminus[-c(10:85),]


pairs(Auto.naminus[,1:7])

plot(Auto$mpg, Auto$weight)
# Heavier weight correlates with lower mpg.

plot(Auto$mpg, Auto$cylinders)
# More cylinders, less mpg.

plot(Auto$mpg, Auto$year)
# Cars become more efficient over time.

library(MASS)

names(Boston)
summary(Boston)

pairs(Boston)
plot(Boston$crim, Boston$age)

hist(Boston$crim[Boston$crim>1], breaks=25)

hist(Boston$tax, breaks=25)

hist(Boston$ptratio, breaks=25)

dim(Boston[Boston$chas==1,])
# or 
dim(subset(Boston, chas == 1))

all(Boston[Boston$chas==1,] == subset(Boston, chas == 1))

t(subset(Boston, medv == min(Boston$medv)))
