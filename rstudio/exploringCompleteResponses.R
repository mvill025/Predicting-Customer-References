
# preprocess data
source(file = "preprocessing/completeResponses.R")

# explore
hist(Responses$salary)
hist(Responses$age)
barplot(prop.table(table(Responses$elevel)))
barplot(prop.table(table(Responses$car)))
barplot(prop.table(table(Responses$zipcode)))
hist(Responses$credit)
barplot(prop.table(table(Responses$brand)))

# feature selection