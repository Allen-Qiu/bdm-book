#--------------在测试集上测试 -----------------------
# 挑选了C50做模型

rm(list=ls(all=TRUE))
.libPaths('c:/rlibrary')
library(C50)

adult.data<-read.table("data/adult.data",sep=",",header=F,strip.white=TRUE)

adult.test<-read.table("data/adult.test",sep=",",header=F,strip.white=TRUE)
test <-adult.test[,1:14]
test.label<-adult.test[,15]
train<-adult.data

modle<-C5.0(V15 ~.,data=train, winnow = F, noGlobalPruning = F,fuzzyThreshold = F)
res<-predict(modle, test)
acc.c50<-length(which(res==test.label))/length(res)

print(paste("C50 test:", acc.c50))


