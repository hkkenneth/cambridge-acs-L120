Data=read.csv("trainingdata.csv")

dim(Data)
head(Data, n=3)
Data.ash=Data[Data$label=="ash",]
Data.beech=Data[Data$label=="beech",]
Data.elder=Data[Data$label=="elder",]
Data.elm=Data[Data$label=="elm",]
Data.larch=Data[Data$label=="larch",]
Data.oak=Data[Data$label=="oak",]
Data.rowan=Data[Data$label=="rowan",]
Data.yew=Data[Data$label=="yew",]

summary(Data$label)

dim(Data.ash)
dim(Data.beech)