# use adult.data build naive bayes model

rm(list=ls(all=TRUE))
.libPaths('c:/rlibrary')
library(e1071)

set.seed(1)
adult<-read.table("data/adult.data",sep=",",header=F,strip.white=TRUE)
adult.data<-adult

folds <- 10
bin <- nrow(adult.data)%/%folds
idx.start<-1
acc.nb <-0

for (i in 0:(folds-1)){
    idx.end<-idx.start+bin
    idx.test <- idx.start:idx.end
    test <-adult.data[idx.test,1:14]
    test.label<-adult.data[idx.test,15]
    train<-adult.data[-idx.test,]
    
    # naive bayes
    modle<-naiveBayes(V15 ~.,data=train)
    res<-predict(modle, test, type='class')
    acc.nb<-length(which(res==test.label))/length(res)+acc.nb
    
    idx.start<-idx.end+1
}
print(paste("NB acc:",acc.nb/folds))
