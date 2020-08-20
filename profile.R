# build customer profile

rm(list=ls(all=TRUE))
.libPaths('c:/rlibrary')
library(DAAG)
library(pscl)

# 1. read data file
cdata<-read.csv("data/customer_loan.csv")

table(cdata$yq)

# 2. normalize numeric attributes
cnames<-c("sqjkqx","xykpjsy6m","num_card", "zsjll","zsmll","cshk","sxze","yqsj","yjfzhke","zwze","kzpsr","yjsr","yhls","spsxed")
    
for(cname in cnames){
    maxval<-max(cdata[,cname])
    cdata[,cname]<-sapply(cdata[,cname],FUN=function(x){x/maxval})
}    
# balanced dataset
idx<-which(cdata$yq==1)
size<-length(which(cdata$yq==0))-length(idx)
idx.sample<-sample(idx,size,replace=T)
cdata.sample<-cdata[idx.sample,]
cdata.balanced<-rbind(cdata, cdata.sample)


# using VIF eliminate multicollinearity
model<-glm(yq~.-jznx-bdfc-wdhj-yjsr, family = binomial("logit"), data=cdata.balanced)
vif_val<-vif(model)
sort(vif_val,decreasing = T)[1:5]

# goodness of fit
pR2(model)
res<-predict(model,  newdata=cdata.balanced, type='response')
res<-sapply(res, function(x){if(x>0.5) 1 else 0})
acc<-length(which(res==cdata.balanced$yq))*1.0/length(res)
print(paste('ACC:',acc))

#explore coefficients
summary(model)