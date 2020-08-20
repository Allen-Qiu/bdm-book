rm(list=ls(all=TRUE))
lib<-"c:/rlibrary"
.libPaths(lib)
library(arules)

data('Groceries')
itemFrequencyPlot(Groceries,topN=20, type=c("absolute"))


mfis<-apriori(Groceries, parameter= list(target="frequent itemsets",confidence=0.4,supp = 0.01, minlen=2, maxlen = 2))
summary(mfis)
itemset<-head(mfis, by="support", n=5)
inspect(unique(itemset))

pis<-apriori(Groceries, parameter=list(supp=0.001, conf = 0.1, target="rules", maxlen=2))
psub<-subset(pis, subset=rhs%in%"canned beer")
inspect(head(psub, by="lift"))


rules<-apriori(data=Groceries, parameter=list(supp=0.001,conf = 0.08), 
               appearance = list(rhs="whole milk"),
               control = list(verbose=F))
rules<-sort(rules, decreasing=TRUE,by="confidence")
inspect(rules[1:5])

