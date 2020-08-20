rm(list=ls(all=TRUE))

library(class)
mydata<-read.table("ridingmowers.txt",header=T, sep=" ")
new_data<-data.frame(Income=c(60,75), Lot_size=c(20,19))

#plot
windows()
owner_idx<-which(mydata$Ownership=='Owner')
par(mfrow=c(2,1))
plot(mydata$Income[owner_idx], mydata$Lot_Size[owner_idx],col='red', xlab='income',ylab='lot size', pch=16 )
non_idx<-which(!mydata$Ownership=='Owner')
points(mydata$Income[non_idx], mydata$Lot_Size[non_idx],col='blue')
text(new_data[,1],new_data[,2], "+")

nn <- knn(train = mydata[, 2:3], test = new_data, cl = mydata[, 4], k=5,prob=T)
print(nn)

# normalizing
mydata_m<-as.matrix(mydata[,2:3])
mmax<-apply(mydata_m,2,max)
train<-scale(mydata_m,center=F,scale=mmax)
test_m<-as.matrix(new_data)
test<-scale(test_m,center=F,scale=mmax)

plot(train[owner_idx,1], train[owner_idx,2],col='red', xlab='income',ylab='lot size', pch=16)
points(train[non_idx,1], train[non_idx,2],col='blue')
text(test[,1],test[,2], "+")

nn <- knn(train = train, test = test, cl = mydata[, 4], k=5,prob=T)
print(nn)

