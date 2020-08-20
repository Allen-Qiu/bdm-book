# logistic regression

rm(list=ls(all=TRUE))

womenroles.data<-read.table('data/womenroles.txt',header = T)
lr<-glm(vote~education+gender,  family = binomial, data=womenroles.data)
summary(lr)

womenroles.data$y<-sapply(womenroles.data$vote, function(x){if(x=='agree') 0 else 1});
lr2<-glm(y~education+gender,  family = binomial, data=womenroles.data)

newdat<-data.frame(education=15,gender=as.factor('Male'))
predict(lr, newdata=newdat, type='response')
predict(lr2, newdata=newdat, type='response')

womenroles.new<-womenroles.data
womenroles.new$agree<-sapply(womenroles.data$vote, FUN=function(x){ifelse(x=='agree',1,0)})
womenroles.new$disagree<-sapply(womenroles.data$vote, FUN=function(x){ifelse(x=='disagree',1,0)})

womenroles.t<-aggregate(x=womenroles.new[,c('agree','disagree')],by=list(womenroles.new$education,womenroles.new$gender),FUN=sum)

colnames(womenroles.t)<-c('education','gender','agree','disagree')

fm1 <- cbind(disagree, agree) ~ gender + education
lr3 <- glm(fm1, data = womenroles.t, family = binomial())
predict(lr3, newdata=newdat, type='response')
