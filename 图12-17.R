rm(list=ls(all=TRUE))
.libPaths('c:/rlibrary')
library(igraph)

g<-read_graph("data/karate.gml", format = c("gml"))
V(g)$size <- 4*sqrt(strength(g))
V(g)$label<-V(g)
V(g)$label.dist <- ifelse(V(g)$size >= 10, 0, 1.0)
coms<-cluster_fast_greedy(g)
coms$membership<-cut_at(coms,2)
V(g)[coms$membership==1]$color<-'red'
V(g)[coms$membership==2]$color<-'yellow'
F1 <- V(g)[coms$membership==1]
F2 <- V(g)[coms$membership==2]
E(g)[ F1 %--% F1 ]$color <- "pink"
E(g)[ F2 %--% F2 ]$color <- "lightblue"
E(g)[ F1 %--% F2 ]$color <- "yellow"
E(g)$width<-2
plot(g, layout=layout.kamada.kawai)
