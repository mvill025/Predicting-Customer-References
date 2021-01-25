install.packages("readr")
library(readr)
carsDS<-read.csv("data-sets/cars.csv")

attributes(carsDS)
summary(carsDS)
str(carsDS)
names(carsDS)
hist(carsDS$speed.of.car)
hist(carsDS$distance.of.car)
plot(carsDS$speed.of.car, carsDS$distance.of.car)
set.seed(123)
trainSize<-round(nrow(carsDS) * 0.2)
testSize<-nrow(carsDS)-trainSize
trainSize
testSize
training_indices<-sample(seq_len(nrow(carsDS)),size=trainSize)
trainSet <- carsDS[training_indices,]
testSet <- carsDS[-training_indices,]
set.seed(405)
trainSet <- carsDS[training_indices,]
testSet <- carsDS[training_indices,]
LinearModel<-lm(trainSet$speed.of.car ~ testSet$distance.of.car)
summary(LinearModel)
predictions<-predict(LinearModel, testSet)
predictions
