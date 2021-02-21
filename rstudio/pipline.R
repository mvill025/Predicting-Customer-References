library(readr)
library(caret)
install.packages('e1071', dependencies=TRUE)

source(file = "preprocessing/completeResponses.R")
source(file = "preprocessing/surveyIncompleteResponses.R")

inTraining <- createDataPartition(Responses$brand, p = .75, list = FALSE)
training <- Responses[inTraining,]
testing <- Responses[-inTraining,]

incompleteTest <- IncompleteResponses
incompleteTest$brand <- NULL

source(file = "train-test-modules/randomForest.R")
source(file = "train-test-modules/gbm.R")

incomplete <- incompleteTest
incomplete$brand <- gbmIncompletePredicted
