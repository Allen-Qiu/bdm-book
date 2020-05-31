# use adult.data build CART and C50 model

rm(list=ls(all=TRUE))
.libPaths('c:/rlibrary')
library(rpart)
library(C50)

set.seed(1)
adult<-read.table("data/adult.data",sep=",",header=F,strip.white=TRUE)
adult.data<-adult

#---------------- CART and C5.0 -------------------
folds <- 10
bin <- nrow(adult.data)%/%folds
idx.start<-1
acc.cart<-0
acc.c50 <-0

for (i in 0:(folds-1)){
    idx.end<-idx.start+bin
    idx.test <- idx.start:idx.end
    test <-adult.data[idx.test,1:14]
    test.label<-adult.data[idx.test,15]
    train<-adult.data[-idx.test,]
    
    # pruned cart
    ct <- rpart(V15 ~ ., data=train, method = "class", cp=0.00001, minsplit=1)
    ct.pruned<-prune(ct,ct$cptable[which.min(ct$cptable[,"xerror"]),"CP"])
    r<-predict(ct.pruned, test, type='class')
    acc.cart<-length(which(r==test.label))/length(r)+acc.cart
    
    # c5.0
    modle<-C5.0(V15 ~.,data=train, winnow = T, noGlobalPruning = F,fuzzyThreshold = T)
    res<-predict(modle, test)
    acc.c50<-length(which(res==test.label))/length(res)+acc.c50
    
    idx.start<-idx.end+1
}
print(paste("CART and C50:",acc.cart/10, acc.c50/10))
