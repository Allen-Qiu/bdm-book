rm(list=ls(all=TRUE))
.libPaths('c:/rlibrary')
library(e1071)

set.seed(5)
data(iris)

idx<-sample(1:nrow(iris),nrow(iris), replace=T)
idx<-unique(idx)
iris.train<-iris[idx,]
iris.test <-iris[-idx,]

# 比较调优和使用默认值的svm模型
model <- svm(Species ~ ., data=iris.train)
pred <- predict(model, iris.test)
acc<-length(which(iris.test$Species==pred))/nrow(iris.test)
print(paste('default acc:',acc))

obj <- tune.svm(Species~., data=iris.train, gamma = 2^(-10:10), cost=2^(0:10),tunecontrol = tune.control(nrepeat=10, sampling = "cross", cross=3),kernel='radial', type="C-classification")

print(paste("gamma:",obj$best.model$gamma))
print(paste("cost:" ,obj$best.model$cost))

model <- svm(Species ~ ., data=iris.train, kernel="radial", type="C-classification", gamma=obj$best.model$gamma, cost=obj$best.model$cost)
pred <- predict(model, iris.test)
acc<-length(which(iris.test$Species==pred))/nrow(iris.test)
print(paste('optimal acc:',acc))



rm(list=ls(all=TRUE))
.libPaths('c:/rlibrary')
library(e1071)

data(iris)

x <- iris[,-5]
y <- iris[,5]
obj <- tune.knn(x, y, k = 1:17, tunecontrol = tune.control(sampling = "boot"))
summary(obj)
plot(obj)

