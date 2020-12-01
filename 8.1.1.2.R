rm(list=ls(all=TRUE))

supervised_ratio <- function(dat, dependent, independent, positive){
    tab<-aggregate(x=rep(1, nrow(dat)), 
                   by=list(dat[,independent],dat[,dependent]),
                   FUN=sum)
    r<-sapply(X=dat[,independent], FUN=function(x){
        ci <- tab[tab$Group.1==x & tab$Group.2==positive,][3]
        ni <- tab[tab$Group.1==x & tab$Group.2!=positive,][3]
        return (ci/(ci+ni))
    })
    return(unlist(r))
}
woe<-function(dat, dependent, independent, positive){
    TC<-length(dat[,dependent]==positive)
    TN<-length(dat[,dependent]!=positive)
    tab<-aggregate(x=rep(1, nrow(dat)), 
                   by=list(dat[,independent],dat[,dependent]),
                   FUN=sum)
    r<-sapply(X=dat[,independent], FUN=function(x){
        ci <- tab[tab$Group.1==x & tab$Group.2==positive,][3]
        ni <- tab[tab$Group.1==x & tab$Group.2!=positive,][3]
        return ((ci*TN)/(ni*TC))
    })
    return(unlist(r))
}

adult<-read.table("data/adult.data",sep=",",header=F, strip.white=TRUE)
idx.missing<-unique(c(which(adult$V14=='?'),which(adult$V7=='?'),which(adult$V2=='?')))
adult.nomiss<-adult[-idx.missing,]

r1<-supervised_ratio(adult.nomiss, 15,7,'<=50K')
r2<-woe(adult.nomiss, 15,7,'<=50K')
