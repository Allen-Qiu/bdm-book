# gaussian mixture modle

rm(list=ls(all=TRUE))
lib<-"d:\\qjt\\r\\mylibrary"
.libPaths(lib)
library(mclust)

set.seed(10)
num <- 100
x1  <- rep(0,num)+rnorm(num,sd=0.3)
y1  <- rep(0,num)+rnorm(num,sd=0.6)
x2  <- rep(2,num)+rnorm(num,sd=0.7)
y2  <- rep(2,num)+rnorm(num,sd=0.2)
x3  <- rep(4,num)+rnorm(num,sd=0.6)
y3  <- rep(0,num)+rnorm(num,sd=0.7)
x   <- c(x1,x2,x3)
y   <- c(y1,y2,y3)
dat <- matrix(c(x,y),nrow=length(x))

r<-Mclust(dat)

windows()
xlim <- range(x)
ylim <- range(y)
plot(0,0,xlim=xlim,ylim=ylim,pch=20)

for (i in c(1:length(x))){
    if(r$classification[i]==1){
        ptype=0
        col='yellow'
    }else if(r$classification[i]==2){
        ptype=8
        col='blue'
    }else if(r$classification[i]==3){
        ptype=21
        col='black'
    }else{
        ptype=21
        col='red'
    }
    points(x[i],y[i],pch=ptype,col=col)
}

