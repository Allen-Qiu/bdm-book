rm(list=ls(all=TRUE))
lib<-"c:/rlibrary"
.libPaths(lib)
library(igraph)

g<-read_graph("data/karate.gml", format = c("gml"))
coms<-cluster_edge_betweenness(g)
print(cut_at(coms, 2))

#--- walktrap ----
coms<-cluster_walktrap(g,steps=4)
coms$membership<-cut_at(coms,3)
plot(coms, g)



