# functions

#over-sampling, df: dataframe; label:column of label; minority: sampled catefory 
oversampling<-function(df,label,minority){
    idx.pos<-which(df[,label]==minority)
    idx.neg<-setdiff(c(1:nrow(df)),idx.pos)
    idx.sample<-sample(idx.pos,length(idx.neg)-length(idx.pos),replace=T)
    s<-df[idx.sample,]
    return(rbind(df,s))
}
oversampling2<-function(matrix,label.list,minority){
    # browser()
    idx.pos<-which(label.list==minority)
    idx.neg<-setdiff(c(1:nrow(matrix)),idx.pos)
    idx.sample<-sample(idx.pos,length(idx.neg)-length(idx.pos),replace=T)
    s<-matrix[idx.sample,]
    dat<-rbind(matrix,s)
    label.sample<-label.list[idx.sample]
    label<-c(as.character(label.list), as.character(label.sample))
    return(list(data=dat,label=label))
}
undersampling<-function(df,label,majority){
    idx.neg<-which(df[,label]==majority)
    idx.pos<-setdiff(c(1:nrow(df)),idx.neg)
    idx.sample<-sample(idx.neg,length(idx.pos))
    s<-df[idx.sample,]
    return(rbind(df[idx.pos,],s))
}


# k fold dataset. select no. i fold
kfold.data<-function(idx,k,i){
    len<-length(idx)
    size<-floor(len/k)
    start<-(i-1)*size+1
    if(i==k){
        end=len        
    }else{
        end<-start+size-1
    }
    return(idx[start:end])
}

# balanced acc
bacc<-function(label,pred){
    idx<-which(label=='Yes')
    pacc<-length(which(pred[idx]=='Yes'))/length(idx)
    nacc<-length(which(pred[-idx]=='No'))/length(label[-idx])
    return((pacc+nacc)/2)
}



a<-as.factor(c('A','A'))
b<-as.factor(c('B','B'))
c(a,b)
