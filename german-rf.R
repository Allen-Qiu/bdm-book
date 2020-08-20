# random forrest for gernman.data
# 2019

rm(list=ls(all=TRUE))
lib<-"d:\\qjt\\r\\mylibrary"
.libPaths(lib)
library(randomForest)
source("accuracy.R")

# hyper-parameter
# over: oversampling, re:the repeat minority, otherwise: not process imbalanced dataset
sampling<-"null"

dat<-read.table('data/german.data')
gdata<-dat
gdata$V21<-as.factor(gdata$V21)
lable<-gdata$V21

# balance dataset
if(sampling=="over"){     
    len  <-length(which(lable=="1"))-length(which(lable=="2"))
    midx <-sample(which(lable=="2"),len,replace=T)
    gdata<-rbind(gdata[which(lable=="1"),],gdata[midx,])
}else if(sampling=="re"){ 
    num<-floor(length(which(lable=="1"))/length(which(lable=="2")))
    for(i in 1:(num-1)){
        gdata<-rbind(gdata, gdata[which(lable=="2"),])
    }
}

# 10-fold cv
set.seed(10)
idx      <- sample(nrow(gdata),nrow(gdata))
acc.rf   <- c(0,0,0)
acc.rf.train   <- c(0,0,0)
start    <- 1
size     <- floor(nrow(gdata)/10)

for(k in c(1:10)){
    end    <- ifelse(start+size-1>nrow(gdata), nrow(gdata), start+size-1) 
    test.idx <- idx[start:end]
    train.idx   <- setdiff(idx,test.idx)
    test       <- gdata[test.idx,]
    test.lable <- gdata[test.idx,'V21']
    train      <- gdata[train.idx,]
    train.lable<- train[,'V21']
    
    # random forest
    rf         <- randomForest(V21~., data=train, ntree=1000)
    r          <- predict(rf,  newdata=test)
    acc.rf <- acc.rf + bacc(r,test.lable,1)
    
    r          <- predict(rf,  newdata=train[,-21])
    acc.rf.train <- acc.rf.train + bacc(r,train[,21],1)
    
    start <- end +1
}
print(paste("acc on testset positive samples:",acc.rf[1]/10))
print(paste("acc on testset negtive samples:",acc.rf[2]/10))
print(paste("acc on testset:",acc.rf[3]/10))
print(paste("acc on trainset:",acc.rf.train/10))