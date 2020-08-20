# kmeans

rm(list=ls(all=TRUE))

set.seed(10)
num <- 100
x1  <- rep(0,num)+rnorm(num,sd=0.5)
y1  <- rep(0,num)+rnorm(num,sd=0.5)
x2  <- rep(2,num)+rnorm(num,sd=0.5)
y2  <- rep(2,num)+rnorm(num,sd=0.5)
x3  <- rep(4,num)+rnorm(num,sd=0.5)
y3  <- rep(0,num)+rnorm(num,sd=0.5)
x   <- c(x1,x2,x3)
y   <- c(y1,y2,y3)
dat <- matrix(c(x,y),nrow=length(x))

clusters<-kmeans(dat, centers=3, iter.max = 10, nstart = 3, algorithm = "MacQueen")

windows()
xlim <- range(x)
ylim <- range(y)
plot(0,0,xlim=xlim,ylim=ylim)

for (i in c(1:length(x))){
    if(clusters$cluster[i]==1){
        ptype=0
        col='yellow'
    }else if(clusters$cluster[i]==2){
        ptype=8
        col='blue'
    }else if(clusters$cluster[i]==3){
        ptype=21
        col='black'
    }else{
        ptype=21
        col='red'
    }
    
    points(x[i],y[i],pch=ptype,col=col)
}
# 绘制簇的中心
for(i in 1:nrow(clusters$centers)){
    points(clusters$centers,pch=19,col="red")
}
