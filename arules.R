# use arules package

rm(list=ls(all=TRUE))
lib<-"d:\\qjt\\R\\mylibrary"
.libPaths(lib)
library(arules)

data("Groceries")
summary(Groceries)

mfis<-apriori(Groceries, parameter= list(target="frequent itemsets", confidence=0.4, supp = 0.01, minlen=2, maxlen = 2))
summary(mfis)
itemset<-head(mfis, by="support", n=5)
inspect(unique(itemset))


pis<-apriori(Groceries, parameter=list(supp=0.001, conf = 0.1, target="rules", maxlen=2))
psub<-subset(pis, subset=rhs%in%"canned beer")
inspect(head(psub, by="confidence"))
