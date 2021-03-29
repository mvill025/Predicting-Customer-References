# ============================================================================ #
# Analysis with the goals of:
#   - Predicting sales of: PC, Laptops, Netbooks and smartphones
#   - Assessing the impact of services reviews and customer reviews have on
#     sales of different product types
# ============================================================================ #

install.packages("corrplot")
library(corrplot)
library(readr)
library(caret)
set.seed(998)

historicalSales <- read.csv("productattributes/existingproductattributes2017.csv")
newSales <- read.csv("productattributes/newproductattributes2017.csv")

# create dummy variables for non-numeric values
tmp <- dummyVars("~.", data = historicalSales)
historicalSalesDF <- data.frame(predict(tmp, newdata = historicalSales))
summary(historicalSalesDF)
# create dummy variables for non-numeric values
tmp <- dummyVars("~.", data = historicalSalesDF)
df <- historicalSales
summary(df)
df$ProductType <- NULL
df$BestSellersRank <- NULL
df$ProductNum <- NULL
df$Price <- NULL
df$ProfitMargin <- NULL
# find correlation data
corrData <- cor(df)
corrplot(corrData)

# dfs separate by types
HistoricalbyType <- split(historicalSales, f = historicalSales$ProductType) 

# create df with only types client is interested in:
#   PC, Laptop, Netbook, Smartphone
Historical_PLNS_Sales <- rbind(
  HistoricalbyType$PC, # contians a null value in BestSellerRank
  HistoricalbyType$Laptop, 
  HistoricalbyType$Netbook, 
  HistoricalbyType$Smartphone
)

# create dummy variables for non-numeric values
tmp <- dummyVars("~.", data = Historical_PLNS_Sales)
df <- data.frame(predict(tmp, newdata = Historical_PLNS_Sales))
summary(df)
df$x2StarReviews <- NULL

# find correlation data
corrData <- cor(df)
corrplot(corrData)

# subset with products with a best sellers rank (BSR)

# create df with a non null best sellers rank
BSRHistoricalSales <- historicalSales[!is.na(historicalSales$BestSellersRank),]

# dfs separate by types
BSRHistoricalbyType <- split(BSRHistoricalSales, f = BSRHistoricalSales$ProductType)

# create df with only types client is interested in:
#   PC, Laptop, Netbook, Smartphone
BSR_Historical_PLNS_Sales <- rbind(
  BSRHistoricalbyType$PC,
  BSRHistoricalbyType$Laptop, 
  BSRHistoricalbyType$Netbook, 
  BSRHistoricalbyType$Smartphone
)

# create dummy variables for non-numeric values
tmp <- dummyVars("~.", data = historicalSales)
df <- data.frame(predict(tmp, newdata = historicalSales))
summary(df)
df$BestSellersRank <- NULL
#removing low correlation vars to volume

# calculate correlation
corrData <- cor(df)
corrplot(corrData)


# ============================================================================ #
# Train and Evaluate Models: - SVM, RF, GBM
# ============================================================================ #

inTraining <- createDataPartition(df$Volume, p = .75, list = FALSE)
training <- df[inTraining,]
testing <- df[-inTraining,]

# ======================== Quantile Random Forest ============================ #
qrfFitControl <- trainControl(
  method = "repeatedcv", 
  number = 10, 
  repeats = 10
)

qrfGrid <- expand.grid(
  mtry=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
)

qrfFit <- train(
  Volume~., 
  data = training, 
  method = "qrf", 
  trControl = qrfFitControl,
  tuneGrid = qrfGrid
)

# mtry = 10, Rsquared = ~0.95
qrfFit
qrfFitImp <- varImp(qrfFit, scale = FALSE)
plot(qrfFit, top = 20 )

# evaluate the random forest model
qrfPredicted <- predict(qrfFit,testing)
dat <- data.frame(
  obs = qrfPredicted,
  pred = testing$Volume
)

postResample(qrfPredicted, testing$Volume)
# RMSE          Rsquared    MAE 
# 93.7903597  0.9733696 44.2105263 

# ====== L2 Regularized Support Vector Machine (dual) with Linear Kernel ===== #
svmFitControl <- trainControl(
  method = "repeatedcv", 
  number = 10, 
  repeats = 10
)

svmFit <- train(
  Volume~., 
  data = training, 
  method = "svmLinear3", 
  trControl = svmFitControl,
)

# cost = 1, and Loss = L2
svmFit
svmFitImp <- varImp(svmFit, scale = FALSE)
plot(svmFit, top = 20 )

# evaluate the random forest model
svmPredicted <- predict(svmFit,testing)
dat <- data.frame(
  obs = svmPredicted,
  pred = testing$Volume
)

postResample(svmPredicted, testing$Volume)
# RMSE        Rsquared    MAE
# 1.5435496   0.9999923   0.6909343 

# ======================== Stochastic Gradient Boosting ====================== #
gbmFitControl <- trainControl(
  method = "repeatedcv", 
  number = 30, 
  repeats = 20
)

gbmFit <- train(
  Volume~., 
  data = training, 
  method = "gbm", 
  trControl = gbmFitControl,
)

# n.trees = 50, interaction.depth = 3, shrinkage = 0.1, n.minobsinnode = 10
gbmFit

# evaluate model
gbmPredicted <- predict(gbmFit,testing)
dat <- data.frame(
  obs = gbmPredicted,
  pred = testing$Volume
)

postResample(gbmPredicted, testing$Volume)
# RMSE          Rsquared    MAE 
# 224.2361420   0.9223778   190.3937895 

predictions <- testing
predictions$svm <- svmPredicted
predictions$gbm <- gbmPredicted
predictions$qrf <- qrfPredicted

# ======================== SVM BEST PERFORMER! =============================== #
# RMSE        Rsquared    MAE
# 1.5435496   0.9999923   0.6909343 
# ============================================================================ #

# Predicting Volume in incoming data
tmp <- dummyVars("~.", data = newSales)
newSalesDF <- data.frame(predict(tmp, newdata = newSales))

newSalesPredictions <- newSales
newSalesPredictions$Volume <- predict(svmFit, newSalesDF)
newSalesPredictions$svm <- predict(svmFit, newSalesDF)
newSalesPredictions$rrf <- predict(qrfFit, newSalesDF)

# dfs separate by types
newSalesPredictionsByType <- 
  split(
    newSalesPredictions, 
    f = newSalesPredictions$ProductType
  )

# create df with only types client is interested in:
#   PC, Laptop, Netbook, Smartphone
newSalesPredictions_PLNS <- rbind(
  newSalesPredictionsByType$PC,
  newSalesPredictionsByType$Laptop, 
  newSalesPredictionsByType$Netbook, 
  newSalesPredictionsByType$Smartphone
)

write.csv(
  newSalesPredictions, 
  file="C2.T3NewSalesPredictions.csv", 
  row.names = TRUE
)
write.csv(
  newSalesPredictions, 
  file="C2.T3NewSalesPredictions_PLNS.csv", 
  row.names = TRUE
)

# ============================================================================ #
# Evaluating the effect of customer and product reviews on sales               #
# ============================================================================ #

# do correlation analysis on all products
tmp <- dummyVars("~.", data = historicalSales)
df <- data.frame(predict(tmp, newdata = historicalSales))

# remove low correlated fields to volume from df
df$ProfitMargin <- NULL
df$BestSellersRankg <- NULL
df$ProductNum <- NULL
df$ProductDepth <- NULL
corrData <- cor(df)
corrplot(corrData)

