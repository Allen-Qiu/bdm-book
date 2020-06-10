# bagging with svm
# --2020.05

rm(list=ls(all=TRUE))
.libPaths('c:/rlibrary')
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

k<-20 # the number of base models
models<-vector("list",k)


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

#-- svm bagging with over-sampling
pdata<-data.all[,!colnames(data.all) %in% excluded.column]
label<-sapply(data.all$y,FUN=function(x){ifelse(x=='yes',1,0)})
data.onehot <- model.matrix(y~.-1,data=pdata)
data.train  <- data.onehot[idx.train,]
data.test   <- data.onehot[idx.test,]
label.train <- label[idx.train]
label.test  <- label[idx.test]
r<-matrix(rep(0,k*length(label.test)),ncol=k)

for(i in c(1:k)){
    print(i)
    idx.train.sampling<-sample(c(1:nrow(data.train),nrow(data.train)),replace=T)
    data.train.sampling<-data.train[idx.train.sampling,]
    label.train.sampling<-label.train[idx.train.sampling]
    
    idx.train.pos<-which(label.train.sampling==1)
    idx.train.neg<-setdiff(c(1:length(label.train.sampling)), idx.train.pos)
    sample.size<-length(idx.train.neg)-length(idx.train.pos)
    idx.train.pos.sampling<-sample(idx.train.pos,sample.size, replace = T)
    idx.train.balanced<-c(idx.train.neg,idx.train.pos,idx.train.pos.sampling)
    
    model.svm <- svm(x=data.train.sampling[idx.train.balanced,], y=label.train.sampling[idx.train.balanced],type='nu-classification',kernel = "radial", nu=0.38,cachesize = 1000, gamma=0.00006, probability=T)
    r.svm <- predict(model.svm, newdata=data.test, probability=T)
    acc.svm <-bacc(r.svm,label.test)
    print(paste('svm:',acc.svm))

    models[[i]]<-model.svm
    r[,i] <- attr(r.svm, 'probabilities')[,1]
}
# average
a<-apply(r,MARGIN=1, FUN=sum)
res<-sapply(a,FUN=function(x){ifelse(x/20>=0.47,0,1)})
bacc(res, label.test)

# voting
a<-sapply(r, FUN=function(x){ifelse(x>0.5,0,1)})
a<-apply(r, MARGIN = 1,FUN=sum)
res<-sapply(a,FUN=function(x){ifelse(x>10,0,1)})
bacc(res, label.test)


