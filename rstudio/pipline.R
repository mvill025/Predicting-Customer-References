install.packages('e1071', dependencies=TRUE)
install.packages("writexl")
library(readr)
library(caret)
library(writexl)

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

total <- rbind(Responses, incomplete)

brands <- total$brand
plot(
  brands, 
  main = "Brand Preferance", 
  xaxt = "n", xlab = "Brands",
  ylab = "# of People", ylim = c(0,10000)
)
axis(1, at=0.75:2, labels=c("Acer", "Sony"))
axis()

write_xlsx(total, "exports/totalBrandPreferences.csv")
