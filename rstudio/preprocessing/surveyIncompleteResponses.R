library(readr)

IncompleteResponses <- read.csv("SurveyData/SurveyIncomplete.csv")

# education
IncompleteResponses$elevel <- as.factor(IncompleteResponses$elevel)
# primary car
IncompleteResponses$car <- as.factor(IncompleteResponses$car)
# zip code
IncompleteResponses$zipcode <- as.factor(IncompleteResponses$zipcode)
# computer brand
IncompleteResponses$brand <- as.factor(IncompleteResponses$brand)