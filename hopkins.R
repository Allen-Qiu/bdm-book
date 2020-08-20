# 使用hopkins统计量评估聚类趋势

lib<-"c:\\rlibrary"
.libPaths(lib)
library(factoextra)

x<-runif(100, 0, 10)
y<-runif(100, 0, 10)
windows()
plot(x,y, pch=16, main='A')
windows()
x1<-rnorm(50,mean=0,sd=2)+2.5
y1<-rnorm(50,mean=0,sd=2)+2.5
x2<-rnorm(50,mean=0,sd=2)+7.5
y2<-rnorm(50,mean=0,sd=2)+7.5

xlim<-range(c(x1,x2))
ylim<-range(c(y1,y2))
plot(x1,y1,xlim=xlim,ylim=ylim,pch=16, main='B')
points(x2,y2,pch=16)

df1<-data.frame(x=x,y=y)
res <- get_clust_tendency(df1, n = nrow(df1)/2, graph = FALSE)
print(paste('Hopkins of D1:',res$hopkins_stat))

df2<-data.frame(x=c(x1,x2),y=c(y1,y2))
res <- get_clust_tendency(df2, n = nrow(df2)/2, graph = FALSE,seed=5)
print(paste('Hopkins of D2:',res$hopkins_stat))
