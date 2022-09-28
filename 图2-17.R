dat<-read.table('data/womenroles.txt', header=T)
d<-aggregate(dat$vote, by=list(dat$education,dat$gender), FUN=length)
num<-matrix(rep(0,42),21,2)
for(i in c(1:nrow(d))){
    if(d$Group.2[i]=='Female'){
        num[d$Group.1[i]+1,1]<-d$x[i]   
    }else{
        num[d$Group.1[i]+1,2]<-d$x[i]   
    }
}
windows()
barplot(t(num), beside=T, names.arg=(0:20), legend.text=c('female','male'), xlab="total")
box(bty="l")