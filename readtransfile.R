rm(list=ls(all=TRUE))
lib<-"c:\\rlibrary"
.libPaths(lib)
library(arules)

readTransFile<-function(fname){
    con <- file(fname, "r")
    lines<-readLines(con)
    mylist<-sapply(lines, FUN=function(x){strsplit(x, " ")})
    close(con)
    return(mylist)
}
fname<-"data/arules_example2.txt"
mylist<-readTransFile(fname)
mylist <- as(mylist, "transactions")
rules <- apriori(mylist)
inspect(rules)
