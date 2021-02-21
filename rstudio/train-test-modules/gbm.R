library(caret)
library(gbm)

set.seed(988)

fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)

gbmGrid <- expand.grid(
  n.trees = (0:50)*50,
  interaction.depth=c(1, 3, 5), 
  shrinkage=c(0.01, 0.001),
  n.minobsinnode=10,
  mtry=c(1,2,3,4,5,6,7,8,9,10)
)

gbmFit <- train(
  brand~., 
  data = training, 
  method = "gbm", 
  trControl = fitControl,
)

gbmFitImp <- varImp(gbmFit, scale = FALSE)
plot(gbmFitImp, top = 20 )

classes <- c("salary", "age", "elevel", "car", "zipcode", "credit", "brand")

# evaluate the random forest model
gbmPredicted <- predict(gbmFit,testing)
postResample(gbmPredicted, testing$brand)
defaultSummary(dat, lev = classes, model = rfFit2)

gbmIncompletePredicted <- predict(gbmFit,incompleteTest)
