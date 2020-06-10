# xgboost deal with imbalanced dataset and use 10-fold cv -- 2020.05

rm(list=ls(all=TRUE))
.libPaths('d:/qjt/r/mylibrary')
library(xgboost)
library(smotefamily)
library(pROC)

sampling<-'over'  # over, under, somte, adas
rdata<-read.table('bank-train.csv', sep=';',header=T)
raw.data<-rdata
row<-nrow(raw.data)
col<-ncol(raw.data)

# -- day --
vec<-raw.data[,'day']

#-- duration --
vec<-raw.data[,'duration']
raw.data[,'duration']<-log(vec+1)

#-- campaign --
vec<-raw.data[,'campaign']
# data[,'campaign']<-log(vec+1)

#-- pdays --
vec<-raw.data[,'pdays']
raw.data$contacted<-as.factor(sapply(vec, FUN=function(x){ifelse(x<0,'No','Yes')}))
raw.data[,'pdays']<-log(vec+2)

#-- previous --
vec<-raw.data[,'previous']
r<-sort(vec,decreasing=TRUE)
max.value<-r[1]
second.max.value<-r[2]
idx<-which(vec==max.value)
vec[idx]<-second.max.value
raw.data[,'previous']<-vec

# building and evaluating model
excluded.column<-c('balance')
pdata<-raw.data[,!colnames(raw.data) %in% excluded.column]
label<-sapply(pdata$y,FUN=function(x){ifelse(x=='yes',1,0)})
pdata.onehot<-model.matrix(y~.-1,data=pdata)

bacc<-function(res, label){
    # browser()
    idx<-which(label==1)
    pacc<-length(which(res[idx]==label[idx]))*1.0/length(res[idx])
    nacc<-length(which(res[-idx]==label[-idx]))*1.0/length(res[-idx])
    return ((pacc+nacc)/2)
}

set.seed(1)
m<-1
k<-10
acc<-0
auc.val<-0
num<-0

idx<-sample(c(1:row),row)
span<-ceiling(row/k)
start<-1

while(start<=row){
    if(start+span<=row){
        end   <- start+span
    }else{
        end<-row
    }
    index.test <- idx[start:end]
    index.train<- setdiff(idx,index.test)
    test.label <- label[index.test]
    
    if(sampling=='over'){
        params<-list(eta=1.5,max_depth=5,lambda=1,alpha=0.2,gamma=70,min_child_weight=5,subsample=0.8,colsample_bytree=1)
        
        print('over-sampling')
        index.train.pos<-index.train[label[index.train]==1]
        index.train.neg<-setdiff(index.train, index.train.pos)
        sample.size<-length(index.train.neg)-length(index.train.pos)
        index.train.pos.sampling<-sample(index.train.pos,sample.size, replace = T)
        index.train.sampling<-c(index.train.neg,index.train.pos,index.train.pos.sampling)
        model<-xgb.train(data=xgb.DMatrix(pdata.onehot[index.train.sampling,],label=label[index.train.sampling]), 
                         nrounds = 200,
                         objective="binary:logistic",
                         param=params)
        # browser()
    }else if(sampling=='under'){
        params<-list(eta=1.5,max_depth=5,lambda=1,alpha=0.2,gamma=70,min_child_weight=5,subsample=0.8,colsample_bytree=1)
            
        print('under-sampling')
        index.train.pos<-index.train[label[index.train]==1]
        index.train.neg<-setdiff(index.train, index.train.pos)
        index.train.neg.sampling<-sample(index.train.neg,length(index.train.pos))
        index.train.sampling<-c(index.train.pos,index.train.neg.sampling)
        model<-xgb.train(data=xgb.DMatrix(pdata.onehot[index.train.sampling,],label=label[index.train.sampling]), 
                         nrounds = 200,
                         objective="binary:logistic",
                         param=params)
        # browser()
    }else if(sampling=='smote'){
        print('smote')
        params<-list(eta=0.009,max_depth=9,lambda=1,alpha=0.3,gamma=65,min_child_weight=5,subsample=0.8,colsample_bytree=1)
            
        dup<-floor(length(which(label[index.train]=="0"))/length(which(label[index.train]=="1")))-1
        newdata<-SMOTE(X=as.data.frame(pdata.onehot[index.train,]), target=label[index.train], K = 3, dup_size = dup)
        train.smote<-newdata$data[,-ncol(newdata$data)]
        train.label<-newdata$data[,ncol(newdata$data)]
        model<-xgb.train(data=xgb.DMatrix(as.matrix(train.smote),label=as.numeric(train.label)), 
                         nrounds = 200,
                         objective="binary:logistic",
                         param=params)
        # browser()
    }else if(sampling=='adas'){
        print('adas')
        params<-list(eta=0.009,max_depth=9,lambda=1,alpha=0.3,gamma=65,min_child_weight=5,subsample=0.8,colsample_bytree=1)
        
        # browser()
        dup<-floor(length(which(label[index.train]=="0"))/length(which(label[index.train]=="1")))-1
        newdata<-ADAS(X=as.data.frame(pdata.onehot[index.train,]), target=label[index.train], K = 3)
        train.adas<-newdata$data[,-ncol(newdata$data)]
        train.label<-newdata$data[,ncol(newdata$data)]
        train.label.new<-sapply(train.label,FUN=function(x){ifelse (x=='1',1, 0)})
        model<-xgb.train(data=xgb.DMatrix(as.matrix(train.adas),label=train.label.new), 
                         nrounds = 200,
                         objective="binary:logistic",
                         param=params)
        
    }
    
    r <- predict(model, newdata=pdata.onehot[index.test,])
    r <- sapply(r,FUN=function(x){ifelse(x>0.5,1,0)})
    acc.bacc<-bacc(r,test.label)
    acc<-acc+acc.bacc
    
    roc_obj <- roc(test.label, r, quiet=T)
    val<-auc(roc_obj)
    auc.val<-auc.val+val
    
    # print(paste(acc.bacc,val))
    start <- end+1
    num<-num+1
}
print(paste('avg:',acc/num,auc.val/num))

