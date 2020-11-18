rm(list=ls(all=TRUE))
lib<-"c:\\rlibrary"
.libPaths(lib)
library(rpart)
library(adabag)

gdata<-read.table('data/german.data', stringsAsFactors=T)
gdata$V21<- as.factor(gdata$V21)
acc.cart <- 0
acc.ada  <- 0
acc.bag  <- 0
ind      <- c(1:nrow(gdata))

for(k in c(1:10)){
    testind    <- c((k*100-100+1):(k*100))
    trainind   <- ind[which(!ind%in%testind)]
    test       <- gdata[testind,]
    test.label <- gdata[testind,'V21']
    train      <- gdata[trainind,]
    
    # adaboost
    ada        <- boosting(V21~., data=train)
    r          <- predict(ada,  newdata=test)
    acc.ada    <- length(which(r$class==test.label))/length(test.label)+acc.ada
    
    # bagging
    bag        <- bagging(V21~., data=train)
    r          <- predict(bag,  newdata=test)
    acc.bag    <- length(which(r$class==test.label))/length(test.label)+acc.bag
    
    # CART
    ct        <- rpart(V21 ~ ., data=train, 
                       method = "class", cp=0.00001, minsplit=1)
    ct.pruned <- prune(ct,ct$cptable[which.min(ct$cptable[,"xerror"]),"CP"])
    r         <- predict(ct.pruned, test, type='class')
    acc.cart  <- length(which(r==test.label))/length(r)+acc.cart
}
print(paste('acc cart:',acc.cart/10, "acc adaboost:",acc.ada/10, "acc bagging:",acc.bag/10))
