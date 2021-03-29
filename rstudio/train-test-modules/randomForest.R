library(caret)

set.seed(988)

fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)

rfGrid <- expand.grid(mtry=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15))

rfFit2 <- train(
  brand~., 
  data = training, 
  method = "rf", 
  trControl = fitControl,
  tuneGrid = rfGrid
)

rfFit2

rfFit2Imp <- varImp(rfFit2, scale = FALSE)
plot(rfFit2Imp, top = 20 )

classes <- c(
  "salary",
  "age",
  "elevel",
  "car",
  "zipcode",
  "credit",
  "brand"
)

# evaluate the random forest model
rfPredicted <- predict(rfFit2,testing)
dat <- data.frame(
  obs = rfPredicted,
  pred = testing$brand
)

postResample(rfPredicted, testing$brand)
defaultSummary(dat, lev = classes, model = rfFit2)

rfIncompletePredicted <- predict(rfFit2,incompleteTest)

