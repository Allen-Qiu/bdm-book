rm(list=ls(all=TRUE))
lib<-"c:/rlibrary"
.libPaths(lib)
library(e1071)

data(iris)
#因为因变量是factor类型，默认建立c-classification分类模型
model <- svm(Species ~ ., data = iris)
# 也可以按照下面的方法建立模型
x <- subset(iris, select = -Species)
y <- iris$ Species
model <- svm(x, y)
# 考察模型
print(model)
summary(model)
# 在训练集上预测（仅为演示模型的使用）
pred <- predict(model, x)
table(pred, y)
# 可视化 ，用颜色区分类别，用不同符号区分支持向量
plot(cmdscale(dist(iris[,-5])),
     col = as.integer(iris[,5]),
     pch = c("o","+")[1:nrow(iris) %in% model$index + 1])
#  -----------------SVR------------------
# 产生数据
x <- seq(0.1, 5, by = 0.05)
y <- log(x) + rnorm(x, sd = 0.2)
# 估计模型，在训练集上预测
m   <- svm(x, y)
new <- predict(m, x)
# 可视化
plot(x, y)
points(x, log(x), col = 2)
points(x, new, col = 4)

#--------------一类分类------------------
# 产生一个二维的正太分布数据
X <- data.frame(a = rnorm(1000), b = rnorm(1000))
# 传统方法
m <- svm(X, gamma = 0.1)
# 或者
m <- svm(~., data = X, gamma = 0.1)
# 测试
newdata <- data.frame(a = c(0, 2), b = c(0, 2))
predict (m, newdata)
# 可视化
plot(X, col = 1:1000 %in% m$index + 1, xlim = c(-5,5), ylim=c(-5,5))
points(newdata, pch = "+", col = 2, cex = 5)
