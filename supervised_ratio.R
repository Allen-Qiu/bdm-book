
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
