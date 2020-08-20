# a class for converting categorical attribute to numeric
# constructor

dataprocess<-function(){
    k<-c(5,7,9,11,14)
    dat1<-read.csv("data/cc_train.csv")
    train.label<- dat1[,'Churn']
    train.idx<-1:nrow(dat1)
    dat2<-read.csv("data/cc_test.csv")
    dat<-rbind(dat1[,-c(1,k,21)],dat2[,-c(1,k)])
    # dat<-rbind(dat1[,-c(1,21)],dat2[,-c(1)])
    dat3<-read.table("data/cc_test_label.csv")
    
    colname<-"TotalCharges"
    r<-dat[, colname]
    idx<-which(is.na(r))
    m<-sum(r[-idx])/length(r[-idx])
    idx<-which(is.na(dat[,colname]))
    dat[idx,colname]<-m
    
    colname<-"MonthlyCharges"
    dat[,colname]<-log(1+dat[,colname])

    colname<-"TotalCharges"
    dat[,colname]<-log(1+dat[,colname])
    
    colname<-"tenure"
    # dat[,colname]<-log(1+dat[,colname])
    
    onehot.data<-model.matrix(~.-1,data=dat)
    
    val<-list(traindata=onehot.data[train.idx,],
              trainlabel=train.label,
              testdata=onehot.data[-train.idx,],
              testlabel=dat3$V1
    )
    attr(val, "class") <- "databuilder"
    return(val)
}
dataprocess2<-function(){
    # browser()
    k<-c(1,5,7,9,11,14)
    numeric.cols<-c(3,6,19,20)
    
    dat1<-read.csv("data/cc_train.csv")
    train.label<- dat1[,'Churn']
    train.idx<-1:nrow(dat1)
    dat2<-read.csv("data/cc_test.csv")
    
    # convert to numeric value
    idx.cols<-c(1:(ncol(dat1)-1))
    idx.cols<-setdiff(idx.cols,numeric.cols)
    idx.cols<-setdiff(idx.cols,k)
    
    for(i in idx.cols){
        col<-unique(dat1[,i])
        tidx<-sapply(col, FUN=function(x){
            which(dat1[,i]==x)[1]
        })
        dat1[,i]<-as.numeric(supervised_ratio(dat1, 21, i, 'Yes'))
        newcol<-as.character(dat2[,i])
        for(j in c(1:length(tidx))){
            idx<-which(newcol==col[j])
            newcol[idx]<-dat1[tidx[j],i]
        }
        dat2[,i]<-as.numeric(newcol)
    }
    
    dat<-rbind(dat1[,-c(k,21)],dat2[,-c(k)])
    dat3<-read.table("data/cc_test_label.csv")
    colname<-"TotalCharges"
    r<-dat[, colname]
    idx<-which(is.na(r))
    m<-sum(r[-idx])/length(r[-idx])
    idx<-which(is.na(dat[,colname]))
    dat[idx,colname]<-m

    colname<-"MonthlyCharges"
    dat[,colname]<-log(1+dat[,colname])

    colname<-"TotalCharges"
    dat[,colname]<-log(1+dat[,colname])
    # browser()
    #normalize
    # for(i in c(1:ncol(dat))){
    #     m<-max(dat[,i])
    #     dat[,i]<-sapply(dat[,i],FUN=function(x){x/m})
    # }

    val<-list(traindata=dat[train.idx,],
              trainlabel=train.label,
              testdata=dat[-train.idx,],
              testlabel=dat3$V1
    )
    attr(val, "class") <- "databuilder"
    return(val)
}
supervised_ratio <- function(dat, dependent, independent, positive){
    tab<-aggregate(x=rep(1, nrow(dat)), 
                   by=list(dat[,independent],dat[,dependent]),
                   FUN=sum)
    r<-sapply(X=dat[,independent], FUN=function(x){
        # browser()
        ci <- as.numeric(tab[tab$Group.1==x & tab$Group.2==positive,][3])
        ni <- as.numeric(tab[tab$Group.1==x & tab$Group.2!=positive,][3])
        res<-ci/(ci+ni)
        
        return (ifelse(is.na(res),0,res))
    })
    return(unlist(r))
}

woe<-function(dat, dependent, independent, positive){
    TC<-length(dat[,dependent]==positive)+1
    TN<-length(dat[,dependent]!=positive)+1
    tab<-aggregate(x=rep(1, nrow(dat)), 
                   by=list(dat[,independent],dat[,dependent]),
                   FUN=sum)
    r<-sapply(X=dat[,independent], FUN=function(x){
        # browser()
        ci <- as.numeric(tab[tab$Group.1==x & tab$Group.2==positive,][3])+1
        ni <- as.numeric(tab[tab$Group.1==x & tab$Group.2!=positive,][3])+1
        res <- log((ci*TN)/(ni*TC))
        return (ifelse(is.na(res),0,res))
    })
    return(unlist(r))
}
