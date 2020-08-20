# house-vote-84 dataset

rm(list=ls(all=TRUE))
lib<-"d:\\qjt\\R\\mylibrary"
.libPaths(lib)
library(dummies)
library(class)

df<-read.csv('data/house-votes-84.data',header=F)
dummy.col<-colnames(df)[!colnames(df)%in%c("V1")]
df.onehot <- dummy.data.frame(df,sep=".", names=dummy.col)

df.onehot.data <-df.onehot[,2:ncol(df.onehot)]
df.onehot.label<-df.onehot[,1]

idx<-sample(nrow(df),100)
test<-df.onehot.data[idx,]
test.label<-df.onehot.label[idx]

train<-df.onehot.data[-idx,]
train.label<-df.onehot.label[-idx]

#------------- KNN ---------------------
nn <- knn(train = train, test = test, cl = train.label, k=5,prob=F)
acc<-length(which(nn==test.label))/nrow(test)
print(paste("KNN:",acc))
#------------- cart --------------------
library(rpart)
df.label<-df[,1]
test<-df[idx,]
test.label<-df.label[idx]
train<-df[-idx,]

ct <- rpart(V1 ~ ., data = train, method = "class")
pred.test <- predict(ct,test,type = "class")
acc<-length(which(pred.test==test.label))/length(idx)
print(paste("CART default:",acc))

ct <- rpart(V1 ~ ., data = train, method = "class", cp = 0.00001, minsplit = 1, xval = 5)
pruned.ct <- prune(ct,cp = ct$cptable[which.min(ct$cptable[,"xerror"]),"CP"])
pred.test <- predict(pruned.ct,test,type = "class")
acc<-length(which(pred.test==test.label))/length(idx)
print(paste("CART pruned:",acc))
#----- woe ------
source("supervised_ratio.R")
col<-colnames(df)[!colnames(df)%in%c("V1")]
df.woe<-data.frame(V1=df$V1)

for(cname in col){
    df.woe[,cname]<-woe(df,'V1',cname,'republican')
}

df.woe.data<-df.woe[,-1]
df.woe.label<-df.woe[,1]
test<-df.woe.data[idx,]
test.label<-df.woe.label[idx]

train<-df.woe.data[-idx,]
train.label<-df.woe.label[-idx]

nn <- knn(train = train, test = test, cl = train.label, k=5,prob=F)
acc<-length(which(nn==test.label))/nrow(test)
print(paste("KNN woe:",acc))

#---- supervised-ratio -------
col<-colnames(df)[!colnames(df)%in%c("V1")]
df.ratio<-data.frame(V1=df$V1)

for(cname in col){
    df.ratio[,cname]<-supervised_ratio(df,'V1',cname,'republican')
}

df.ratio.data<-df.ratio[,-1]
df.ratio.label<-df.ratio[,1]
test<-df.ratio.data[idx,]
test.label<-df.ratio.label[idx]

train<-df.ratio.data[-idx,]
train.label<-df.ratio.label[-idx]

nn <- knn(train = train, test = test, cl = train.label, k=5,prob=F)
acc<-length(which(nn==test.label))/nrow(test)
print(paste("KNN supervised-ratio:",acc))


