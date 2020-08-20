# iris数据集上的多类分类
rm(list=ls(all=TRUE))
lib<-"c:/rlibrary"
.libPaths(lib)
library(neuralnet)
data(iris)

y<-model.matrix(~Species-1, iris)
y<-data.frame(y)
trainindex<-sample(1:nrow(iris), round(0.75*nrow(iris)))
train<-cbind(iris[trainindex,1:4], y[trainindex,])
test<-iris[-trainindex,1:4]

mynet<-neuralnet(Speciessetosa + Speciesversicolor + Speciesvirginica ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, train, hidden=c(3), act.fct="tanh", linear.output=F)

res<-compute(mynet, test)
nn.pred<-max.col(res$net.result)
test.y<-max.col(y[-trainindex,])

acc<-length(which(nn.pred==test.y))/length(test.y)
print(paste("accuracy:",acc))
