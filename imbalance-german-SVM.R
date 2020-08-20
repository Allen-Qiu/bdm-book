# SVM on german credit dataset

rm(list=ls(all=TRUE))
lib<-"d:\\qjt\\r\\mylibrary"
.libPaths(lib)
library(e1071)

set.seed(13)
source("supervised_ratio.R")
source("accuracy.R")

# hyper-parameter
# over: oversampling, under:undersampling; null: not process imbalanced dataset
sampling<-"under"

dat<-read.table('data/german.data')
gdata<-dat
size<-nrow(gdata)
set.seed(1)
idx<-sample(size, size)

gdata$V1<-supervised_ratio(dat, 21, 1, 1)
gdata$V3<-supervised_ratio(dat, 21, 3, 1)
gdata$V4<-supervised_ratio(dat, 21, 4, 1)
gdata$V6<-supervised_ratio(dat, 21, 6, 1)
gdata$V7<-supervised_ratio(dat, 21, 7, 1)
gdata$V8<-supervised_ratio(dat, 21, 8, 1)
gdata$V9<-supervised_ratio(dat, 21, 9, 1)
gdata$V10<-supervised_ratio(dat, 21, 10, 1)
gdata$V11<-supervised_ratio(dat, 21, 11, 1)
gdata$V12<-supervised_ratio(dat, 21, 12, 1)
gdata$V14<-supervised_ratio(dat, 21, 14, 1)
gdata$V15<-supervised_ratio(dat, 21, 15, 1)
gdata$V16<-supervised_ratio(dat, 21, 16, 1)
gdata$V17<-supervised_ratio(dat, 21, 17, 1)
gdata$V18<-supervised_ratio(dat, 21, 18, 1)
gdata$V19<-supervised_ratio(dat, 21, 19, 1)
gdata$V20<-supervised_ratio(dat, 21, 20, 1)
gdata$V21<-as.factor(gdata$V21)

acc.svm   <- c(0,0,0)
acc.svm.train<- c(0,0,0)
ind      <- sample(nrow(gdata),nrow(gdata))
start    <- 1
size     <- floor(nrow(gdata)/10)

for(k in c(1:10)){
    end    <- ifelse(start + size-1>nrow(gdata), nrow(gdata), start+size-1) 
    testind <- ind[start:end]
    test       <- gdata[testind,]
    test.lable <- gdata[testind,'V21']
    train      <- gdata[-testind,]
    train.lable<- train[,21]
    
    # oversampling
    if(sampling=="over"){
        len<-length(which(train.lable=="1"))-length(which(train.lable=="2"))
        midx<-sample(which(train.lable=="2"),len,replace=T)
        m.train<-train[midx,]
        train<-rbind(train,m.train)
    }else if(sampling=="under"){
        # undersampling
        len<-length(which(train.lable=="2"))
        midx<-sample(which(train.lable=="1"),len)
        train<-rbind(train[which(train.lable=="2"),],train[midx,])
    }
    
    train.svm <- train[,-21]
    train.svm.lable <- train[,21]
    
    model <- svm(train.svm, train.svm.lable, type="C-classification")
    
    test.svm<-test[,-21]
    pred <- predict(model, test.svm)
    acc.svm<-bacc(pred,test.lable,1) + acc.svm 
    
    pred <- predict(model, train[,-21])
    acc.svm.train<-bacc(pred,train[,21],1) + acc.svm.train
    start <- end +1
}
print(paste("acc SVM:",acc.svm/10))
print((acc.svm[1]+acc.svm[2])/20)
