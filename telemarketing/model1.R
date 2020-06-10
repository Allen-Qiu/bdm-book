# Random forest with 10-fold cv on traning set -- 2020.05

rm(list=ls(all=TRUE))
.libPaths('c:/rlibrary')
library(e1071)
# library(smotefamily)
library(randomForest)
library(pROC)

rdata<-read.table('bank-train.csv', sep=';',header=T)
raw.data<-rdata
row<-nrow(raw.data)
col<-ncol(raw.data)

# preprocess
# -- a function for exploring variables
investigate<-function(df, cname){
    par(mfrow=(c(1,2))) 
    vec<-df[,cname]
    idx.na<-which(is.na(vec))
    barplot(table(is.na(vec))/length(vec))
    print(table(is.na(vec))/length(vec))
    idx.notna<-which(!is.na(vec))
    val<-df[idx.notna,cname]
    plot(val)
    # return(val)
}

# -- day --
vec<-raw.data[,'day']
# investigate(pdata, 'day')

#-- duration --
vec<-raw.data[,'duration']
# investigate(raw.data, 'duration')
raw.data[,'duration']<-log(vec+1)

#-- campaign --
vec<-raw.data[,'campaign']
# investigate(data, 'campaign')
# data[,'campaign']<-log(vec+1)

#-- pdays --
vec<-raw.data[,'pdays']
# investigate(data, 'pdays')
raw.data$contacted<-as.factor(sapply(vec, FUN=function(x){ifelse(x<0,'No','Yes')}))
raw.data[,'pdays']<-log(vec+2)

#-- previous --
vec<-raw.data[,'previous']
# investigate(data, 'previous')
r<-sort(vec,decreasing=TRUE)
max.value<-r[1]
second.max.value<-r[2]
idx<-which(vec==max.value)
vec[idx]<-second.max.value
raw.data[,'previous']<-vec

# building and evaluating model
excluded.column<-c('balance')
pdata<-raw.data[,!colnames(raw.data) %in% excluded.column]

bacc<-function(res, label){
    # browser()
    idx<-which(label=='yes')
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
    # browser()
    index.test <- idx[start:end]
    index.train<- setdiff(idx,index.test)
    test.label <- pdata$y[index.test]
        
    rf <- randomForest(y~., data=pdata[index.train,])
    r  <- predict(rf,  newdata=pdata[index.test,])
    acc.rf<-bacc(r,test.label)
    acc<-acc+acc.rf

    r.auc<-sapply(r, FUN = function(x){ifelse(x=='yes',1,0)})
    test.label.auc<-sapply(test.label, FUN = function(x){ifelse(x=='yes',1,0)})
        
    roc_obj <- roc(test.label.auc, r.auc, quiet=T)
    val<-auc(roc_obj)
    auc.val<-auc.val+val
    
    print(paste(acc.rf,val))
    start <- end+1
    num<-num+1
}
    
print(paste('avg:',acc/num,auc.val/num))








