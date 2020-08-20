# naive bayes

rm(list=ls(all=TRUE))
.libPaths('c:/rlibrary')
library(e1071)

bank.df <- read.csv("data/UniversalBank.csv")
bank.df <- bank.df[ , -c(1,5)] 
bank.df$Personal.Loan<-factor(bank.df$Personal.Loan)

set.seed(1)
train.index <- sample(c(1:dim(bank.df)[1]), dim(bank.df)[1]*0.6)
train.df <- bank.df[train.index, ]
test.df <- bank.df[-train.index, ]

model <- naiveBayes(Personal.Loan ~ ., data = train.df)
pred<-predict(model,test.df,type='class')
acc<-length(which(pred==test.df$Personal.Loan))/nrow(test.df)
print(acc)

