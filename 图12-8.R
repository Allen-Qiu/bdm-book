rm(list=ls(all=TRUE))
.libPaths('c:/rlibrary')
library(igraph)

g<-read_graph("data/karate.gml", format = c("gml"))
V(g)$label<-V(g)
V(g)$label.color <- 'black'
coms<-cluster_walktrap(g, steps=4)
coms$membership<-cut_at(coms,3)
V(g)[coms$membership==1]$color<-'yellow'
V(g)[coms$membership==2]$color<-'white'
V(g)[coms$membership==3]$color<-'blue'
E(g)$width<-1
plot(g, layout=layout.kamada.kawai)
