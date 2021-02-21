library(caret)
library(readr)
set.seed(998)

# import data set
WholeYear <- read.csv("data-sets/WholeYear.csv")
WholeYear["X"] <- list(NULL)

#split
inTraining <- createDataPartition(
  WholeYear$SolarRad, 
  p = .75, 
  list = FALSE
)
training <- WholeYear[inTraining,]
testing <- WholeYear[-inTraining,]

# define modeling params
fitControl <- trainControl(
  method = 'repeatedvc', 
  number = 3, 
  repeats = 1
)

# train
randomForestFits <- list()
randomForestFits[0] <- train(SolarRad~., data = training, method = "rf", trControl = fitControl)
