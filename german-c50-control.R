rm(list=ls(all=TRUE))
lib<-"c:\\rlibrary"
.libPaths(lib)
library(C50)

set.seed(10)
df<-read.table('data/german.data')
df$V21<- as.factor(df$V21)
size<-nrow(df)
acc<-0
idx<-sample(c(1:size),size)
k     <- 10
start <- 1
fold  <- floor(size/k)

for(i in c(1:k)){
    end <- ifelse(start+fold-1>size, size, start+fold-1) 
    idx.test<-idx[start:end]
    idx.train<-setdiff(idx,idx.test)
    test<-df[idx.test,]
    train<-df[idx.train,]
    model<-C5.0(y=train$V21,x=train[,1:20],data=train,control = C5.0Control(winnow = T,noGlobalPruning = F,fuzzyThreshold = F))
    res<-predict(model, newdata=test)
    val<-length(which(res==test$V21))/length(res)
    print(val)
    acc<-acc+val
    start <- end+1
}
print(paste('accuracy:',acc/k))
