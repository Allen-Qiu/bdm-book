rm(list=ls(all=TRUE))
lib<-"c:/rlibrary"
.libPaths(lib)
library(clustMixType)

data.all<-read.csv('data/Wholesale_customers_data.csv')
data.all$Channel<-as.factor(data.all$Channel)
data.all$Region<-as.factor(data.all$Region)
data.all$Fresh<-data.all$Fresh/max(data.all$Fresh)
data.all$Milk <-data.all$Milk/max(data.all$Milk)
data.all$Grocery <-data.all$Grocery/max(data.all$Grocery)
data.all$Frozen <- data.all$Frozen/max(data.all$Frozen)
data.all$Detergents_Paper <-data.all$Detergents_Paper/max(data.all$Detergents_Paper)
data.all$Delicassen <-data.all$Delicassen/max(data.all$Delicassen)

# 考察k值
cost<-c()
for(k in c(1:12)){
    r<-kproto(data.all, k, lambda = 0.5, iter.max = 100, nstart = 5, verbose = T)
    cost<-c(cost,r$tot.withinss)
}
windows()
ylim <-range(cost)
xlim<-range(c(1:12))
plot(c(1:12), cost, ylim=ylim, xlim=xlim, type="l")
