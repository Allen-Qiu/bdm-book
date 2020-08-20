model.name<-c('Neural Network','Logistic Regression', 'Linear Vector Quantization', 'Projection Pursuit Regression', 'Decision Tree')

dataset<-c('Diabetes','Gaussian','Hvpothvroid','German Credit','Waveform', 'Investment')
    
data.nn<-c(0.62,.9,.98,.18, .02, 0)
data.lr<-c(0,.13, .61, .9, .39, .92)
data.lv<-c(.21, .17, .81, .09, .33, .8)
data.pp<-c(.40, .73, .48, .85, .06, .22)
data.dt<-c(.72, .61, .13, .41, .88, .21)

xrange <- range(c(1:6))
yrange <- range(c(0:2))
plot(xrange, yrange, type="n", ylab="Error Rate" )

lines(c(1:6),data.nn,type="b",lty=1, pch=1)
lines(c(1:6),data.lr,type="b",lty=2, pch=2)
lines(c(1:6),data.lv,type="b",lty=3, pch=3)
lines(c(1:6),data.pp,type="b",lty=4, pch=4)
lines(c(1:6),data.dt,type="b",lty=5, pch=5)

legend(1.2, 1.5, model.name, cex=0.8, pch=c(1:6), lty=c(1:6),text.width = 1,text.font = )
