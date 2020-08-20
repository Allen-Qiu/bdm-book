rm(list=ls(all=TRUE))
lib<-"d:\\qjt\\R\\mylibrary"
.libPaths(lib)
library(pscl)
library(MKmisc)
library(ResourceSelection)


noise<-0.7
k<-1000
x1<-seq(1,k,1)
x2<-seq(1,k,1)

y1<-rep(1,k)+rnorm(n=k, m=1, sd=noise) 
y2<-rep(1,k)+rnorm(n=k, m=0, sd=noise) 
plot(x1,y1,xlab='x', ylab='y',type='p',col='red',main='±ê×¼²î0.7', ylim=range(c(y1,y2)))
points(x2,y2,pch=3)
label<-rep(1,length(x1))
label<-c(label,rep(0,length(x2)))

mdata<-data.frame(x=c(x1,x2),y=c(y1,y2), label=label)
model<-glm(label~x+y,family = binomial("logit"), data=mdata)

res<-predict(model, newdata=mdata, type='response')
res<-sapply(res, function(x){if(x>0.5) 1 else 0})
acc<-length(which(res==mdata$label))*1.0/length(res)
print(acc)
pR2(model)
hoslem.test(mdata$label, fitted(model), g=10)
