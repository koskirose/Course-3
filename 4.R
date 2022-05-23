library(readr)
library(arules)
library(dplyr)
library(arulesViz)
e2017 <- read.transactions("ElectronidexTransactions2017.csv",format = 'basket',header = FALSE, sep=",", 
                           rm.duplicates = TRUE )
summary(e2017)

inspect(e2017) #you can view the transactions
rows <- 9835
items <- 125
dens <- 0.03506172
num_items_bought = rows* items * dens
num_items_bought #43,104

length(e2017) #Number of transactions 9835



#item frequency plot
itemFrequencyPlot(e2017,
              topN = 10,
              main = 'Absolute Item Frequency Plot',
              type = "absolute",
              col = rainbow(5), 
              horiz = TRUE,
              xlab = "Frequency")
image(e2017)
#Item relative frequency plot
itemFrequencyPlot(e2017,
                  topN = 10,
                  main = 'Relative Item Frequency Plot',
                  type = "relative",
                  col = rainbow(5), 
                  horiz = TRUE,
                  xlab = "Frequency")

#Frequent itemsets for all items
support_all =
  apriori(e2017,
          parameter = list(target= "frequent itemsets",
                           supp = 0.01)
  )
#Inspect the 5 most frequent items
inspect(head(sort(support_all, by = "support"), 10))

inspect((sort(support_all, by= "support")))


#Min Support as 0.001, confidence as 0.8
association_rules <- apriori(e2017,
                     parameter = list(supp=0.01, conf=0.5, maxlen=10))
summary(association_rules)
inspect(association_rules)
inspect(association_rules[1:10])



subRules <- association_rules[quality(association_rules)$confidence >0.4]
plot(subRules)

top10subRules <- head(subRules, n=10, by = "confidence")
top10subRules1 <- head(subRules, n= 10, by= "count")

plot(top10subRules, method = "graph", engine = "htmlwidget")
plot(top10subRules1, method = "graph", engine = "htmlwidget")

        