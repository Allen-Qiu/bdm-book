# stack using xgboost and svm and fold bagging
# 训练支持向量机的时间会比较长，耐心等待

rm(list=ls(all=TRUE))
lib<-"d:\\qjt\\R\\mylibrary"
.libPaths(lib)
library(e1071)
library(xgboost)
library(smotefamily)
source("dataprocess.R")
source('funcs.R')
set.seed(1)

stack.xgboost<-function(kdata){
    print('---building xgboost---')
    params<-list(eta=0.5,max_depth=7, gamma=75,min_child_weight=5,subsample=1,colsample_bytree=1)
    nround<-100
    K<-21
    
    sdata<-ADAS(as.data.frame(kdata$traindata),kdata$trainlabel,K)
    colnames<-names(sdata$data)
    traindata<-sdata$data[,!colnames%in%c('class')]
    trainlabel<-sapply(sdata$data[,'class'],FUN=function(x){ifelse (x=='Yes',1, 0)})
    
    idx<-c(1:nrow(traindata))
    idx<-sample(idx,length(idx))
    k<-10
    models<-vector("list",k)
    
    for(i in c(1:k)){
        idx.test<-kfold.data(idx,k,i)
        idx.train<-setdiff(idx,idx.test)
        model<- xgb.train(data=xgb.DMatrix(as.matrix(traindata[idx.train,]), label=trainlabel[idx.train]), nround = nround, objective = "binary:logistic", params=params)
        models[[i]]<-model
    }
    return(models)    
}
stack.svm<-function(kdata){
    print('---building SVM---')
    K<-21
    d<-kdata$traindata
    # sdata<-kdata$traindata
    sdata<-ADAS(as.data.frame(kdata$traindata),kdata$trainlabel,K)
    colnames<-names(sdata$data)
    traindata<-sdata$data[,!colnames%in%c('class')]
    trainlabel<-sdata$data[,'class']
    
    idx<-c(1:nrow(traindata))
    idx<-sample(idx,length(idx))
    k<-10
    models<-vector("list",k)
    
    for(i in c(1:k)){
        idx.test<-kfold.data(idx,k,i)
        idx.train<-setdiff(idx,idx.test)
        model <- svm(x=traindata[idx,], y=trainlabel[idx],type='C-classification',kernel = "radial", gamma=0.000095, cost=13950,cachesize = 400, probability=T)
        models[[i]]<-model
    }
    return(models)    
}

kdata<-dataprocess()

# predict
xgb.models<-stack.xgboost(kdata)
svm.models<-stack.svm(kdata)
r1<-rep(0,nrow(kdata$testdata))
r2<-rep(0,nrow(kdata$testdata))

for (i in 1:length(svm.models)){
    r<-predict(xgb.models[[i]],as.matrix(kdata$testdata))
    r1<-r1+r
    r<-predict(svm.models[[i]],kdata$testdata, probability=T)
    r<-attr(r, "probabilities")[,1]
    r2<-r2+r
}

r<-data.frame(a=(r1/10),b=(r2/10))

perc<-0
res<-c()
for(i in c(1:nrow(r))){
    val<-perc*r[i,1]+(1-perc)*r[i,2]
    res<-c(res,ifelse(val>0.5,'Yes','No'))
}
b<-bacc(kdata$testlabel,res)
print(paste('bacc:',b))

