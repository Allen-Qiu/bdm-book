#http://archive.ics.uci.edu/ml/datasets/Wholesale+customers#

rm(list=ls(all=TRUE))
lib<-"c:/rlibrary"
.libPaths(lib)
library(mclust)
library(dummies)
library(factoextra)

data.all<-read.csv('data/Wholesale_customers_data.csv')
data.prep<-data.all

# preporcess
data.prep$Channel<-as.factor(data.all$Channel)
data.prep$Region<-as.factor(data.all$Region)
data.prep$Fresh<-data.prep$Fresh/max(data.prep$Fresh)
data.prep$Milk <-data.prep$Milk/max(data.prep$Milk)
data.prep$Grocery <-data.prep$Grocery/max(data.prep$Grocery)
data.prep$Frozen <- data.prep$Frozen/max(data.prep$Frozen)
data.prep$Detergents_Paper <-data.prep$Detergents_Paper/max(data.prep$Detergents_Paper)
data.prep$Delicassen <-data.prep$Delicassen/max(data.prep$Delicassen)

# one-hot
df.onehot <- dummy.data.frame(data.prep,sep=".", names=c('Channel','Region'))
r<-Mclust(df.onehot, G=2:20)
print(r$classification)
res <- get_clust_tendency(df.onehot, n = nrow(df.onehot)/2, graph = FALSE,seed=123)
print(paste('Hopkins:',res$hopkins_stat))

# remove categorical attributes
colnames<-colnames(data.prep)
df<-data.prep[,!colnames%in%c('Channel','Region')]
r<-Mclust(df,G=1:12)
print(r$classification)
res <- get_clust_tendency(df, n = nrow(df)/2, graph = FALSE,seed=123)
print(paste('Hopkins:',res$hopkins_stat))

# centers
clusters<-unique(r$classification)
for(k in clusters){
    idx<-which(r$classification==k)
    center<-apply(df[idx,],MARGIN=2, FUN=sum)/length(idx)
    print(k)
    print(center)
}
