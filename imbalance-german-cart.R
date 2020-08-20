# cart on german credit dataset

rm(list=ls(all=TRUE))
lib<-"d:\\qjt\\r\\mylibrary"
.libPaths(lib)
library(class)
library(rpart)
source("supervised_ratio.R")
source("accuracy.R")

# hyper parameter
# over: oversampling, under:undersampling; null: not process imbalanced dataset
sampling<-"under"

dat<-read.table('data/german.data')
gdata<-dat
size<-nrow(gdata)

set.seed(1)
ind<-sample(size, size)

gdata$V21<-as.factor(gdata$V21)

acc.cart  <- c(0,0,0)
start    <- 1
size     <- floor(nrow(gdata)/10)

for(k in c(1:10)){
    end    <- ifelse(start + size-1>nrow(gdata), nrow(gdata), start+size-1) 
    testind <- start:end
    trainind   <- ind[which(!ind%in%testind)]
    test       <- gdata[testind,]
    test.lable <- gdata[testind,'V21']
    train      <- gdata[trainind,]
    train.lable<-train[,21]
    
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
    
    # CART
    ct        <- rpart(V21 ~ ., data=train,
                       method = "class", cp=0.00001, minsplit=1)
    ct.pruned <- prune(ct,ct$cptable[which.min(ct$cptable[,"xerror"]),"CP"])
    r         <- predict(ct.pruned, test, type='class')
    acc.cart  <- bacc(r,test.lable,1)+acc.cart
    start <- end +1
}
print(paste( "acc cart test:",acc.cart/10))
print( (acc.cart[1]+acc.cart[2])/20)

