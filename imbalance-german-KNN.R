# KNN on german

rm(list=ls(all=TRUE))
lib<-"d:\\qjt\\r\\mylibrary"
.libPaths(lib)
library(class)
library(dummies)
source("supervised_ratio.R")
source("accuracy.R")

# hyper-parameter
# over: oversampling, under:undersampling; null: not process imbalanced dataset
sampling<-"over"


dat<-read.table('data/german.data')
gdata<-dat
size<-nrow(gdata)

# gdata$V2 <- log(dat$V2)
# gdata$V5 <- log(dat$V5)
# gdata$V13<- log(dat$V13)
# 
# gdata$V1<-supervised_ratio(dat, 21, 1, 1)
# gdata$V3<-supervised_ratio(dat, 21, 3, 1)
# gdata$V4<-supervised_ratio(dat, 21, 4, 1)
# gdata$V6<-supervised_ratio(dat, 21, 6, 1)
# gdata$V7<-supervised_ratio(dat, 21, 7, 1)
# gdata$V8<-supervised_ratio(dat, 21, 8, 1)
# gdata$V9<-supervised_ratio(dat, 21, 9, 1)
# gdata$V10<-supervised_ratio(dat, 21, 10, 1)
# gdata$V11<-supervised_ratio(dat, 21, 11, 1)
# gdata$V12<-supervised_ratio(dat, 21, 12, 1)
# gdata$V14<-supervised_ratio(dat, 21, 14, 1)
# gdata$V15<-supervised_ratio(dat, 21, 15, 1)
# gdata$V16<-supervised_ratio(dat, 21, 16, 1)
# gdata$V17<-supervised_ratio(dat, 21, 17, 1)
# gdata$V18<-supervised_ratio(dat, 21, 18, 1)
# gdata$V19<-supervised_ratio(dat, 21, 19, 1)
# gdata$V20<-supervised_ratio(dat, 21, 20, 1)

dummy.col<-c("V1","V3","V4","V6","V7","V8","V9","V10","V11","V12","V14","V15","V16","V17","V18","V19","V20")
df.onehot <- dummy.data.frame(dat,sep=".", names=dummy.col)
gdata<-cbind(dat$V2,dat$V5,dat$V13,df.onehot)

acc.knn   <- c(0,0,0)
set.seed(1)
ind      <- sample(nrow(gdata),nrow(gdata))
start    <- 1
size     <- floor(nrow(gdata)/10)

for(k in c(1:10)){
    end    <- ifelse(start + size-1>nrow(gdata), nrow(gdata), start+size-1) 
    testind <- ind[start:end]
    test       <- gdata[testind,!names(gdata)%in%'V21']
    test.lable <- gdata[testind,'V21']
    train      <- gdata[-testind,!names(gdata)%in%'V21']
    train.lable      <- gdata[-testind,'V21']
    
    # oversampling
    if(sampling=="over"){
        len<-length(which(train.lable=="1"))-length(which(train.lable=="2"))
        midx<-sample(which(train.lable=="2"),len,replace=T)
        m.train<-train[midx,]
        train<-rbind(train,m.train)
        m.train.lable<-train.lable[midx]
        train.lable<-c(train.lable,m.train.lable)
    }else if(sampling=="under"){
    # undersampling
        len<-length(which(train.lable=="2"))
        midx<-sample(which(train.lable=="1"),len)
        train<-rbind(train[which(train.lable=="2"),],train[midx,])
        
        m.train.lable<-train.lable[midx]
        train.lable<-c(train.lable[which(train.lable=="2")],m.train.lable)
    }
    nn <- knn(train = train, test = test, cl = train.lable, k=3)
    acc.knn <- acc.knn + bacc(nn,test.lable,1)
    start <- end +1
}
print(paste("acc KNN:",acc.knn/10))
print((acc.knn[1]+acc.knn[2])/20)

