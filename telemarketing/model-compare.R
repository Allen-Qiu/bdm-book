# compare several models performance on test set
# --2020.05

rm(list=ls(all=TRUE))
.libPaths('d:/qjt/r/mylibrary')
library(e1071)
library(xgboost)
library(randomForest)
library(pROC)

bacc<-function(res, label){
    if('yes' %in% label){
        idx<-which(label=='yes')
    }else{
        idx<-which(label==1)
    }
    pacc<-length(which(res[idx]==label[idx]))*1.0/length(res[idx])
    nacc<-length(which(res[-idx]==label[-idx]))*1.0/length(res[-idx])
    return ((pacc+nacc)/2)
}

data.train <- read.table('bank-train.csv', sep=';',header=T)
data.test  <- read.table('bank-test.csv', sep=';',header=T)
data.all   <- rbind(data.train,data.test)
idx.train  <- c(1:nrow(data.train))
idx.test   <- setdiff(c(1:nrow(data.all)),idx.train)

# preprocess
# -- day --
vec<-data.all[,'day']

#-- duration --
vec<-data.all[,'duration']
data.all[,'duration']<-log(vec+1)

#-- pdays --
vec<-data.all[,'pdays']
data.all$contacted<-as.factor(sapply(vec, FUN=function(x){ifelse(x<0,'No','Yes')}))
data.all[,'pdays']<-log(vec+2)

#-- previous --
vec<-data.all[,'previous']
r<-sort(vec,decreasing=TRUE)
max.value<-r[1]
second.max.value<-r[2]
idx<-which(vec==max.value)
vec[idx]<-second.max.value
data.all[,'previous']<-vec

excluded.column <- c('balance')
data.train <- data.all[idx.train,!colnames(data.all) %in% excluded.column]
data.test  <- data.all[idx.test,!colnames(data.all)  %in% excluded.column]

# -- random forest with under-sampling
idx.train.pos <- which(data.train$y=='yes')
idx.train.neg <- which(data.train$y=='no')
idx.train.neg.sampling <- sample(idx.train.neg,length(idx.train.pos))
idx.train.sampling     <- c(idx.train.pos,idx.train.neg.sampling)

model.rf <- randomForest(y~., data=data.train[idx.train.sampling,])
r.rf     <- predict(model.rf,  newdata=data.test)
acc.rf   <- bacc(r.rf,data.test$y)
print(paste('rf',acc.rf))

#-- xgboost and svm with over-sampling
pdata<-data.all[,!colnames(data.all) %in% excluded.column]
label<-sapply(data.all$y,FUN=function(x){ifelse(x=='yes',1,0)})
data.onehot <- model.matrix(y~.-1,data=pdata)
data.train  <- data.onehot[idx.train,]
data.test   <- data.onehot[idx.test,]
label.train <- label[idx.train]
label.test  <- label[idx.test]

params<-list(eta=1.5,max_depth=5,lambda=1,alpha=0.2,gamma=70,min_child_weight=5,subsample=0.8,colsample_bytree=1)
idx.train.pos<-which(label.train==1)
idx.train.neg<-setdiff(c(1:length(label.train)), idx.train.pos)
sample.size<-length(idx.train.neg)-length(idx.train.pos)
idx.train.pos.sampling<-sample(idx.train.pos,sample.size, replace = T)
idx.train.sampling<-c(idx.train.neg,idx.train.pos,idx.train.pos.sampling)
model.xgb<-xgb.train(data=xgb.DMatrix(data.train[idx.train.sampling,],label=label.train[idx.train.sampling]), nrounds = 200, objective="binary:logistic", param=params)
r <- predict(model.xgb, newdata=data.test)
r.xgb <- sapply(r,FUN=function(x){ifelse(x>0.5,1,0)})
acc.xgb <-bacc(r.xgb,label.test)
print(paste('xgb:',acc.xgb))

model.svm <- svm(x=data.train[idx.train.sampling,], y=label[idx.train.sampling],type='nu-classification',kernel = "radial", nu=0.38,cachesize = 1000, gamma=0.00006)
r.svm <- predict(model.svm, newdata=data.test)
acc.svm <-bacc(r.svm,label.test)
print(paste('svm:',acc.svm))
