df=read.csv("trainingdata.csv")

names(df)

head(df)
summary(df$X1)
summary(df$X2)
summary(df$X3)

pairs(df)

df.naminus <- na.omit(df)
dim(df.naminus)

head(df.naminus)

class(df.naminus$label)
class(df.naminus$id)
summary(df.naminus$label)
summary(df)
summary(df$label)

rownames(df.naminus)[1:10]

all(is.na(df$X1) == is.na(df$X2))
all(is.na(df$X1) == is.na(df$X6))

# No special 
boxplot(df$Y ~ df$label)
# There is an outlier
summary(df$Y)

pairs(df[,2:11])
pairs(df.naminus[,2:12])
# Special pattern between Y and X7?
plot(df$Y ~ df$X7)

