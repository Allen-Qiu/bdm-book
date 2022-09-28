lsin<-read.table("data/sin.txt")
len<-length(lsin$V1)
y1<-lsin$V1
y2<-lsin$V2
plot(c(1:len), y1, type="l", xlab="x", ylab="y")
lines(c(1:len), y2, type="l", col="dark red")

