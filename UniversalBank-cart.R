rm(list=ls(all=TRUE))
lib<-"c:\\rlibrary"
.libPaths(lib)
library(rpart)
library(rpart.plot)
library(Rcpp)
library(ggplot2)
library(caret)

bank.df <- read.csv("data/UniversalBank.csv")
bank.df <- bank.df[ , -c(1, 5)] # 排除ID和zip code.

# 划分数据集
set.seed(1)
train.index <- sample(c(1:dim(bank.df)[1]), dim(bank.df)[1]*0.6)
train.df <- bank.df[train.index, ]
test.df <- bank.df[-train.index, ]

# 建立分类模型
default.ct <- rpart(Personal.Loan ~ ., data = train.df, method = "class")

# 分析结果
print(length(default.ct$frame$var[default.ct$frame$var == "<leaf>"]))
default.ct.pred.train <- predict(default.ct,train.df,type = "class")
confusionMatrix(default.ct.pred.train, as.factor(train.df$Personal.Loan))
default.ct.pred.valid <- predict(default.ct,test.df,type = "class")
confusionMatrix(default.ct.pred.valid, as.factor(test.df$Personal.Loan))

# 加入控制参数，构建一棵更深的决策树
deeper.ct <- rpart(Personal.Loan ~ ., data = train.df, method = "class", 
                   control=rpart.control(cp = 0, minsplit = 1))
print(length(deeper.ct$frame$var[deeper.ct$frame$var == "<leaf>"]))
deeper.ct.pred.train <- predict(deeper.ct,train.df,type = "class")
confusionMatrix(deeper.ct.pred.train, as.factor(train.df$Personal.Loan))
deeper.ct.pred.test <- predict(deeper.ct,test.df,type = "class")
confusionMatrix(deeper.ct.pred.test, as.factor(test.df$Personal.Loan))

# 剪枝的决策树
cv.ct <- rpart(Personal.Loan ~ ., data = train.df, method = "class",  control=rpart.control(cp = 0, minsplit = 1, xval=5))
pruned.ct <- prune(cv.ct,cp = cv.ct$cptable[which.min(cv.ct$cptable[,"xerror"]),"CP"])
pred.train <- predict(pruned.ct,train.df,type = "class")
confusionMatrix(pred.train, as.factor(train.df$Personal.Loan))
pred.test <- predict(pruned.ct,test.df,type = "class")
confusionMatrix(pred.test, as.factor(test.df$Personal.Loan))
