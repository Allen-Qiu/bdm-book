# goodness of fit of LR

rm(list=ls(all=TRUE))
lib<-"c:/rlibrary"
.libPaths(lib)
library(pscl)
library(MKmisc)
library(ResourceSelection)

k<-1000
x1<-seq(1,k,1)
x2<-seq(1,k,1)

y1<-rep(1,k)+rnorm(n=k, m=1, sd=0.5) 
y2<-rep(1,k)+rnorm(n=k, m=0, sd=0.5) 

plot(x1,y1,type='p',col='red', ylim=range(0,4))
points(x2,y2)
label<-rep(1,length(x1))
label<-c(label,rep(0,length(x2)))

mdata<-data.frame(x=c(x1,x2),y=c(y1,y2), label=label)
model<-glm(label~x+y,family = binomial("logit"), data=mdata)

res<-predict(model, newdata=mdata, type='response')
res<-sapply(res, function(x){if(x>0.5) 1 else 0})
acc<-length(which(res==mdata$label))*1.0/length(res)
print(acc)
print(pR2(model))
print(hoslem.test(mdata$label, fitted(model), g=10))
