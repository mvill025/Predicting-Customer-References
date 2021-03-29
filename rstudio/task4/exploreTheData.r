install.packages("arules")
install.packages("arulesViz")
library(arules)
library(arulesViz)
set.seed(998)

transactions <- read.transactions(
  "task4/ElectronidexTransactions2017.csv",
  format = "basket",
  sep = ",",
  rm.duplicates = FALSE
)

# list all transactions
inspect(transactions)

# list all items in tranactions
itemLabels(transactions)

# summary:
#   - most frequent items
#   - itemset/transaction length distribution
#     - quarters, mean, median, min, max for a
#   - extended item information
summary(transactions)
# transactions as itemMatrix in sparse format with
# 9835 rows (elements/itemsets/transactions) and
# 125 columns (items) and a density of 0.03506172 
# 
# most frequent items:
#   iMac                HP Laptop CYBERPOWER Gamer Desktop            Apple Earpods 
# 2519                     1909                     1809                     1715 
# Apple MacBook Air                  (Other) 
# 1530                    33622 
# 
# element (itemset/transaction) length distribution:
#   sizes
# 0    1    2    3    4    5    6    7    8    9   10   11   12   13   14   15   
# 2 2163 1647 1294 1021  856  646  540  439  353  247  171  119   77   72   56
# 16   17   18   19   20   21 22   23   25   26   27   29   30 
# 41   26   20   10   10   10  5    3    1    1    3    1    1 
# 
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.000   2.000   3.000   4.383   6.000  30.000 
# 
# includes extended item information - examples:
#   labels
# 1 1TB Portable External Hard Drive
# 2 2TB Portable External Hard Drive
# 3                   3-Button Mouse

# plot individual transactions
image(sample(transactions, 500))

# number of items per transaction
size(transactions)

# list transactions by conversion
LIST(transactions)

# see which items happen the most frequently
itemFrequencyPlot(
  transactions, 
  topN = 20, 
  type = 'absolute',
)

# histogram of # of items per transaction
hist(
  size(transactions),
  main = "# of Items Histogram",
  xlab="# of items in transaction",
  border = "#0074b7",
  col = "#60a3d9",
  ylab="Frequency",
  xlim = c(1,30),
  ylim = c(0,2500),
  labels = TRUE,
  breaks = 29
)

plot(crossTable(transactions))

# ============================================================================ #
# ============================ Apriori Algorithm ============================= #
# ============================================================================ #
# itemInfo(transactions)
# summary(transactions)
# getLevels <- function(itemSet) {
#   levels <- c()
#   # Desktop level
#   print(itemSet)
#   inspect(itemSet)
#   if(itemSet %pin% "Desktop"){
#     append(levels, "Desktop")
#   } else if(length(grep("iMac", itemSet)) > 0){
#     append(levels, "Desktop")
#   }
#   return (levels)
# 
#   if(length(grep("Laptop", itemSet)) > 0){
#     append(levels, "Laptop")
#   } else if (length(grep("MacBook", itemSet)) > 0) {
#     append(levels, "Laptop")
#   }
# 
#   if(length(grep("Monitor", itemSet)) > 0){
#     append(levels, "Monitor")
#   }
# 
#   return("levels")
# }
# 
# itemLevels <- sapply(LIST(transactions), function(x) getLevels(x))
# head(LIST(transactions))
# head(itemLevels)

DesktopTransactions <- transactions[which(transactions %pin% "Desktop" | transactions %pin% "iMac"),]
iMacTransactions <- transactions[which(transactions %in% "iMac"),]
HPLaptopTransactions <- transactions[which(transactions %in% "HP Laptop"),]
monitorTransactions <- transactions[which(transactions %pin% "Monitor"),]
summary(DesktopTransactions)

runRules <- function(df, supp, conf, minlen = 1, maxlen = 30){
  return(apriori(
    df, 
    parameter = list(
      supp = supp, 
      conf = conf,
      minlen = minlen,
      maxlen = maxlen
    )
  ))
}

# general rules
general1 <- runRules(transactions, 0.01, 0.58, 1)
general2 <- runRules(transactions, 0.0045, 0.7, 1)
general3 <- runRules(transactions, 0.002, 0.90, 1)

generalRules <- union(general1, general2)
generalRules <- union(generalRules, general3)
inspect(generalRules)

# iMac rules
inspect(
  subset(runRules(iMacTransactions, 0.1, 0.1, 2), subset = lhs %in% "iMac")
)

# monitor rules
inspect(
  subset(runRules(monitorTransactions, 0.05, 0.2, 1), subset = lhs %pin% "Monitor")
)

# looking at just Desktop data
inspect(
  subset(runRules(monitorTransactions, 0.01, 0.7, 1), subset = TRUE)
)

