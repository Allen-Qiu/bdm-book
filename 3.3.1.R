mydat<-data.frame(x=c(1,2,Inf,4,5,Inf),y=c('yes','no','yes','yes','yes','no'))
ind<-which(mydat$x==Inf)
calcmedian<-function(index, dat){
    d<-dat[dat$y==dat$y[index],'x']
    d<-d[d!=Inf]
    return (median(d))
}

val<-sapply(ind, FUN=calcmedian, dat=mydat)
mydat$x[ind]=val
