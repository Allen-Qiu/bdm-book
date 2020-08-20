# accuracy, balanced accuracy

bacc<-function(preds, lable, classval){
    # browser()
    idx<-which(lable==classval)
    
    len<-length(idx)
    bacc1<-length(which(preds[idx]==classval))/len
    
    len<-length(which(lable!=classval))
    bacc2<-length(which(preds[-idx]!=classval))/len
    
    acc    <- length(which(preds==lable))/length(preds)
    return(c(bacc1,bacc2,acc))
}

