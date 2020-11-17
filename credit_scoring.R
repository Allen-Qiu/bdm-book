# credit scoring on kaggle dataset hmeq
rm(list=ls(all=TRUE))
lib<-"c:\\rlibrary"
.libPaths(lib)
library(e1071)
library(pROC)

df<-read.csv('data/hmeq.csv', stringsAsFactors=T)

# exploring dataset

names<-colnames(df)
size<-nrow(df)

for(cname in names){
    # browser()
    p<-length(which(is.na(df[,cname])))/size
    print(paste(cname,":",p))
    
    if(class(df[,cname])=='factor'){
        print(cname)
        print(table(df[,cname]))
    }
}

# function for miss value
fill.numeric.value<-function(cname, df){
    vec<-df[,cname]
    idx.na<-which(is.na(vec))
    idx<-setdiff(c(1:size),idx.na)
    vec[idx.na]<-mean(vec[idx])
    df[,cname]<-vec
    return(df)
}
fill.factor.value<-function(cname, df){
    vec<-as.character(df[,cname])
    idx.na<-which(vec=='')
    idx<-setdiff(c(1:size),idx.na)
    vec[idx.na]<-'UNK'
    df[,cname]<-as.factor(vec)
    return(df)
}

for(cname in names){
    if(class(df[,cname])=='factor'){
        df<-fill.factor.value(cname,df)
    }else{
        df<-fill.numeric.value(cname,df)
    }
}

# one-hot encoding for factor
data.onehot<-model.matrix(BAD~.-1,data=df)
y<-df$BAD

bacc<-function(res, label){
    # browser()
    idx<-which(label=='1')
    pacc<-length(which(res[idx]==label[idx]))*1.0/length(res[idx])
    nacc<-length(which(res[-idx]==label[-idx]))*1.0/length(res[-idx])
    return ((pacc+nacc)/2)
}

# k fold cross validation
set.seed(1)
k     <- 10
start <- 1
fold  <- floor(size/k)
idx   <- sample(c(1:size),size)
acc<-0
auc<-0
acc.balance<-0

for(i in c(1:k)){
    end    <- ifelse(start+fold-1>size, size, start+fold-1) 
    idx.test <- idx[start:end]
    test       <- data.onehot[idx.test,]
    test.label <- y[idx.test]
    train      <- data.onehot[-idx.test,]
    train.lable<- y[-idx.test]
    
    model <- svm(x=train,y=train.lable, gamma = 1.05, type='C-classification',cost=5, probability = T)
    res <- predict(model,test,probability = T)
    prob<-attr(res,'probabilities')[,1]
    
    val1<-length(which(res==test.label))/length(test.label)
    acc<-acc+val1
    
    roc_obj <- roc(test.label, prob, quiet=T)
    val2<-auc(roc_obj)
    auc<-auc+val2
    
    val3<-bacc(res,test.label)
    print(paste('acc:',val1,'auc:',val2,'bacc',val3))
    acc.balance<-acc.balance+val3
    start <- end+1
}
print(paste('avg acc:',acc/k,'avg auc:',auc/k, 'bacc:',acc.balance/k))


