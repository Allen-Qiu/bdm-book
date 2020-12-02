# one hot using dummies library
# using house-votes-84.data 

rm(list=ls(all=TRUE))
lib<-"c:/rlibrary"
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

nn <- knn(train = train, test = test, cl = train.label, k=5,prob=F)
acc<-length(which(nn==test.label))/nrow(test)
print(acc)
