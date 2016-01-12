Data=read.csv("trainingdata.csv")

dim(Data)
# [1] 1800   12

names(Data)
# [1] "id"    "Y"     "X1"    "X2"    "X3"    "X4"    "X5"    "X6"    "X7"    "X8"    "X9"    "label"

Data.ash=Data[Data$label=="ash",]
Data.beech=Data[Data$label=="beech",]
Data.elder=Data[Data$label=="elder",]
Data.elm=Data[Data$label=="elm",]
Data.larch=Data[Data$label=="larch",]
Data.oak=Data[Data$label=="oak",]
Data.rowan=Data[Data$label=="rowan",]
Data.yew=Data[Data$label=="yew",]

summary(Data$label)
# ash beech elder   elm larch   oak rowan   yew 
# 500   500   500   100    50    50    50    50
