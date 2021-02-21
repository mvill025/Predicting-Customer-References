install.packages("readr")
library(readr)
irisDS<-read.csv("data-sets/iris.csv")
attributes(irisDS)
summary(irisDS)
str(irisDS)
names(irisDS)
hist(as.numeric(factor(irisDS$Species)))
plot(irisDS$Sepal.Length)
irisDS$Species<-as.numeric(factor(irisDS$Species))
qqnorm(irisDS$)
set.seed(123)
trainSize<-round(nrow(irisDS) * 0.2)
testSize<-nrow(irisDS)-trainSize
trainSize
testSize
training_indices<-sample(seq_len(nrow(irisDS)),size=trainSize)
trainSet <- irisDS[training_indices,]
testSet <- irisDS[-training_indices,]
set.seed(405)
trainSet <- irisDS[training_indices,]
testSet <- irisDS[training_indices,]
LinearModel<-lm(trainSet$Petal.Width ~ testSet$Petal.Length)
summary(LinearModel)
predictions<-predict(LinearModel, testSet)
plot(testSet$Petal.Width, testSet$Petal.Length)
