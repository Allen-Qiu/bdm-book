# best RF on german dataset

rm(list=ls(all=TRUE))
lib<-"d:\\qjt\\r\\mylibrary"
.libPaths(lib)
library(rpart)
library(adabag)
library(randomForest)
source("supervised_ratio.R")
source("accuracy.R")

# hyper-parameter
# over: oversampling, under:undersampling; null: not process imbalanced dataset
sampling<-"over"

dat<-read.table('data/german.data')
gdata<-dat
size<-nrow(gdata)

set.seed(1)
idx<-sample(size, size)
gdata$V21<-as.factor(gdata$V21)

acc.rf   <- c(0,0,0)
ind      <- sample(nrow(gdata),nrow(gdata))
start    <- 1
size     <- floor(nrow(gdata)/10)
# gdata<-as.matrix(gdata)

for(k in c(1:10)){
    end    <- ifelse(start + size-1>nrow(gdata), nrow(gdata), start+size-1) 
    testind <- start:end
    trainind   <- ind[which(!ind%in%testind)]
    test       <- gdata[testind,]
    test.label <- gdata[testind,'V21']
    train      <- gdata[trainind,]
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
    
    # random forest
    rf         <- randomForest(V21~., data=train, ntree=1000)
    r          <- predict(rf,  newdata=test)
    acc.rf <- acc.rf + bacc(r,test.label,1)
    
    start <- end +1
}
print(paste("acc RF test:",acc.rf/10))
print((acc.rf[1]+acc.rf[2])/20)
