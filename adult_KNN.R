#--------------- KNN ------------------------
rm(list=ls(all=TRUE))
.libPaths('c:/rlibrary')
library(class)
library(dummies)
source('supervised_ratio.R')

adult<-read.table("data/adult.data",sep=",",header=F,strip.white=TRUE)

#1. v2,v7,v14 有缺失值，删除缺失值
idx.missing<-unique(c(which(adult$V14=='?'),which(adult$V7=='?'),which(adult$V2=='?')))
adult.data<-adult[-idx.missing,]

#2. 对连续属性规范化
col.numeric<-c("V1","V3","V5","V11","V12","V13")
adult.matrix<-as.matrix(adult.data[,col.numeric])
adult.max<-apply(adult.matrix,MARGIN=2,FUN=max)
adult.scale<-scale(adult.matrix, center=F, scale=adult.max)
adult.data[,col.numeric]<-adult.scale

#3. V7,V14使用supervised ratio为KNN预处理数据集
adult.knn.data<-data.frame(adult.data)
adult.knn.data$V7 <-supervised_ratio(adult.data,15,7,'>50K')
adult.knn.data$V14<-supervised_ratio(adult.data,15,14,'>50K')

#4. V2,V4,V6,V8,V9,V10进行one-hot编码
adult.knn.data<-dummy.data.frame(adult.knn.data,sep=".",names=c("V2","V4","V6","V8","V9","V10"))

# 6. evaluate a KNN using 10-fold cross validation
size<-nrow(adult.knn.data)
cols<-ncol(adult.knn.data)

index<-sample(1:size,size, replace = F)
folds    <- 10
bin      <- size%/%folds
idx.start<-1
acc.knn  <-0

for (i in 0:(folds-1)){
    idx.end<-ifelse(idx.start+bin>size,size,idx.start+bin)
    idx.test <- index[idx.start:idx.end]
    test <-adult.knn.data[idx.test,1:(cols-1)]
    test.label<-adult.knn.data[idx.test,'V15']
    train<-adult.knn.data[-idx.test,]
    train.knn <- train[,1:(cols-1)]
    train.knn.label <- train[,"V15"]
    
    nn <- knn(train=train.knn, test=test, cl=train.knn.label, k=29)
    acc.knn<-length(which(nn==test.label))/length(nn) + acc.knn 
    idx.start<-idx.end+1
}
print(paste("KNN:",acc.knn/folds))
