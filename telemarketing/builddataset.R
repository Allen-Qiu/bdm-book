# extract 4000 instance as test set

rm(list=ls(all=TRUE))
.libPaths('d:/qjt/r/mylibrary')

rdata<-read.table('bank-full.csv', sep=';',header=T)
size<-nrow(rdata)
idx.test<-sample(c(1:size),4000)
idx.train<-setdiff(c(1:size),idx.test)

write.table(rdata[idx.test,],'bank-test.csv',row.names=F,sep=";")
write.table(rdata[idx.train,],'bank-train.csv',row.names=F,sep=";")
