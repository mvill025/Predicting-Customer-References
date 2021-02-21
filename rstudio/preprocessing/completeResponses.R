library(readr)

Responses <- read.csv("SurveyData/CompleteResponses.csv")

# education
Responses$elevel <- as.factor(Responses$elevel)
# primary car
Responses$car <- as.factor(Responses$car)
# zip code
Responses$zipcode <- as.factor(Responses$zipcode)
# computer brand
Responses$brand <- as.factor(Responses$brand)