# KNN on german using smote and ADASYN to deal with imbalance issue
# if we run smote and adasyn, they will have impacts each other

rm(list=ls(all=TRUE))
lib<-"d:\\qjt\\r\\mylibrary"
.libPaths(lib)
library(class)
library(smotefamily)
source("supervised_ratio.R")
source("accuracy.R")

# hyper paramter: smote, adas or null
method<-"adas"

dat<-read.table('german.data')
gdata<-dat
size<-nrow(gdata)

gdata$V2 <- log(dat$V2)
gdata$V5 <- log(dat$V5)
gdata$V13<- log(dat$V13)

cols<-c(1,3,4,6,7,8,9,10,11,12,14,15,16,17,18,19,20)

for(i in cols){
    gdata[,i]<-supervised_ratio(dat, 21, i, 1)
}

acc <- c(0,0,0)
set.seed(1)
ind      <- sample(nrow(gdata),nrow(gdata))
start    <- 1
size     <- floor(nrow(gdata)/10)

for(k in c(1:10)){
    end    <- ifelse(start + size-1>nrow(gdata), nrow(gdata), start+size-1) 
    testind <- ind[start:end]
    test       <- gdata[testind,!names(gdata)%in%'V21']
    test.label <- gdata[testind,'V21']
    train      <- gdata[-testind,!names(gdata)%in%'V21']
    train.label<- gdata[-testind,'V21']
    
    if(method=='smote'){
        dup<-floor(length(which(train.label=="1"))/length(which(train.label=="2")))-1
        newdata<-SMOTE(train,train.label,17,dup_size=dup)
        train.new<-newdata$data[,-21]
        train.label.new<-newdata$data[,21]
        nn <- knn(train = train.new, test = test, cl = train.label.new, k=5)
        acc <- acc + bacc(nn,test.label,1)
    }else if(method=="adas"){
        newdata<-ADAS(train,train.label,17)
        train.new<-newdata$data[,-21]
        train.label.new<-newdata$data[,21]
        nn <- knn(train = train.new, test = test, cl = train.label.new, k=5)
        acc <- acc + bacc(nn,test.label,1)
    }else{
        nn <- knn(train = train, test = test, cl = train.label, k=5)
        acc <- acc + bacc(nn,test.label,1)
    }
    
    start <- end +1
}
print(paste(method,(acc[1]+acc[2])/20))