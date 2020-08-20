#cart AmesHouseing
rm(list=ls(all=TRUE))
library(AmesHousing)
library(rpart)
library(rpart.plot)

ames_data<-make_ames()

set.seed(123)
size<-nrow(ames_data)
train.idx <-sample(1:size,size*0.8)
ames_train<-ames_data[train.idx,]
ames_test <-ames_data[-train.idx,]
con<-rpart.control(minsplit = 5, cp = 0.00001, maxdepth = 9)

model  <- rpart(Sale_Price ~ ., data=ames_train, method="anova",control=con)
pred<-predict(model,ames_test, type="matrix")
err<-mean(abs(pred-ames_test$Sale_Price))
print(paste('error:',err))

