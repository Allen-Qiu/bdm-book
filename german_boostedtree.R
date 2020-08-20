# xgboosted on german credit dataset
# --2019--

rm(list=ls(all=TRUE))
lib<-"d:\\qjt\\r\\mylibrary"
.libPaths(lib)
library(xgboost)
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

# boosted tree with default parameters
mdata<-model.matrix(V21~.-1, data = gdata)
set.seed(10)
acc.bt   <- c(0,0,0)
acc.bt.train   <- c(0,0,0)
size <- nrow(mdata)
idx <- sample(size,size)
step  <- size/10
start <- 1

for(k in c(1:10)){
    end    <- ifelse(start+step-1>nrow(mdata), nrow(mdata), start+step-1)
    test.idx   <- start:end
    test       <- mdata[test.idx,]
    test.lable <- lable[test.idx]
    train      <- mdata[-test.idx,]
    y      <- lable[-test.idx]==1
    params <- list(eta=0.1,max_depth=4,lambda=1.05)
    modle  <- xgb.train(params=params,data=xgb.DMatrix(train, label=y),
                        nround = 230, 
                        objective = "binary:logistic")
    r      <- predict(modle,test)
    r2     <- sapply(r, FUN=function(x){ifelse(x>=0.5,1,2)})
    acc.bt <- acc.bt + bacc(r2,test.lable,1)
    
    r      <- predict(modle, newdata=train)
    r2     <- sapply(r, FUN=function(x){ifelse(x>=0.5,1,2)})
    acc.bt.train <- acc.bt.train + bacc(r2,lable[-test.idx],1)
    start  <- end + 1
}
print(paste("acc on testset positive samples:",acc.bt[1]/10))
print(paste("acc on testset negtive samples:",acc.bt[2]/10))
print(paste("acc on testset:",acc.bt[3]/10))
print(paste("acc on trainset:",acc.bt.train/10))
