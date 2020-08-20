# boosted tree on german credit dataset

rm(list=ls(all=TRUE))
lib<-"d:\\qjt\\R\\mylibrary"
.libPaths(lib)

library(xgboost)

dat<-read.table('data/german.data')
lable<-dat$V21
gdata<-dat[,-21]
source('supervised_ratio.R')
source("accuracy.R")

# hyper-parameter
# over: oversampling, under:undersampling; null: not process imbalanced dataset
sampling<-"over"

gdata$V2 <- log(gdata$V2)
gdata$V5 <- log(gdata$V5)
gdata$V13<- log(gdata$V13)
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
gdata<-as.matrix(gdata)

acc   <- c(0,0,0)
size  <- nrow(gdata)
step  <- size/10
start <- 1
set.seed(1)
idx<-sample(size, size)

for(k in c(1:10)){
    end       <- ifelse(start+step>size, size, start+step) 
    testind   <- idx[start:(end-1)]
    test      <- gdata[testind,]
    test.lable<- lable[testind]
    train     <- gdata[-testind,]
    train.lable <- lable[-testind]==1
    y<-train.lable
    
    # oversampling
    if(sampling=="over"){
        len<-length(which(train.lable==T))-length(which(train.lable==F))
        midx<-sample(which(train.lable==F),len,replace=T)
        m.train<-train[midx,]
        train<-rbind(train,m.train)
        y<-c(train.lable, train.lable[midx])
    }else if(sampling=="under"){
    # undersampling
        len<-length(which(train.lable==F))
        midx<-sample(which(train.lable==T),len)
        train<-rbind(train[which(train.lable==F),],train[midx,])
        y<-c(train.lable[which(train.lable==F)],train.lable[midx])
    }
    
    params    <- list(eta=0.2, gamma=10, max_depth=4, early_stopping=20)
    modle     <- xgb.train(params,data=xgb.DMatrix(train, label=y),
                           verbose = 0, nround = 10000, 
                           objective = "binary:logistic")
    r         <- predict(modle,test)
    r2        <- sapply(r, FUN=function(x){ifelse(x>=0.5,1,2)})
    acc       <- bacc(r2,test.lable,1)+acc
    start     <- end
}
print(acc/10)
print((acc[1]+acc[2])/20)