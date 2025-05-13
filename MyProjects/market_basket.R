library(arules)
library(arulesViz)
#dir('E:/analytics/data')

#groceries = read.csv("E:/analytics/data/store_data.csv")
file1 = "E:/analytics/data/store_data.csv"
grocery = read.transactions(file = file1, format='basket', sep=',', skip=0, rm.duplicates = T)

summary(grocery)
inspect(grocery[1:5])

# Item Frequency Plot:______________________
itemFrequencyPlot(grocery, topN=15, xlab='item')
7500 * 0.005
# Apriori Algorithm:____________
rules = apriori(grocery, parameter = list(support = 0.005, confidence = 0.50))
options(digits=2)
summary(rules)
inspect(rules)

# Sort most likely or most frequent Rules based on Confidence:______________________
freqrules = sort(rules, by = 'confidence', decreasing = T)
inspect(freqrules[1:5])

freqrules2 = sort(rules, by='support', decreasing = T)
inspect(freqrules2[1:5])

freqrules3 = sort(rules, by = 'lift', decreasing = T)
inspect(freqrules3[1:5])

plot(freqrules)
plot(freqrules2)
plot(freqrules3)


# Product Wise Rules:______________________

milk_rule = apriori(grocery, parameter = list(support = 0.001, confidence=0.5), appearance = list(default='lhs', rhs='milk'), control = list(verbose=T))

inspect(milk_rule[1:10])
milk_rule = sort(milk_rule, by="confidence", decreasing = T)
inspect(milk_rule[1:5])
plot(milk_rule[1:10], method='graph')
