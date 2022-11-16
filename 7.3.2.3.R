# boosted tree multiclass 

rm(list=ls(all=TRUE))
lib<-"c:/rlibrary"
.libPaths(lib)
library(xgboost)

num <- 100
x1 <- rep(0,num)+rnorm(num)
y1 <- rep(0,num)+rnorm(num)
z1 <- rep(0,num)
x2 <- rep(2,num)+rnorm(num)
y2 <- rep(2,num)+rnorm(num)
z2 <- rep(1,num)
x3 <- rep(4,num)+rnorm(num)
y3 <- rep(0,num)+rnorm(num)
z3 <- rep(2,num)

windows()
xlim<-range(c(x1,x2,x3))
ylim<-range(c(y1,y2,y3))
plot(x1,y1,col="red",xlim=xlim,ylim=ylim)
points(x2,y2)
points(x3,y3,col="blue")

x<-c(x1,x2,x3)
y<-c(y1,y2,y3)
label<-c(z1,z2,z3)
data<-as.matrix(data.frame(x=x,y=y))

idx<-unique(sample(1:length(x),length(x),replace=T))
train<-data[idx,]
train_label<-label[idx]
test<-data[-idx,]
test_label<-label[-idx]

modle <- xgb.train(data=xgb.DMatrix(train, label=train_label),
                   verbose = 0, nround = 10000, num_class=3, 
                   objective = "multi:softmax")
r <- predict(modle,test)
acc<-length(which(r==test_label))/length(r)
print(acc)
