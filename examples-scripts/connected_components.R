library("muxViz")
library(igraph)
library(RColorBrewer)
library(ggraph)

set.seed(1)

#Network setup
Layers <- 3
Nodes <- 10
layerCouplingStrength <- 1
networkOfLayersType <- "categorical"

layer.colors <- brewer.pal(8, "Set2")

cat("####################################\n")
cat("# Multilayer connected components\n")
cat("####################################\n\n")

#Generate an edge-colored network
nodeTensor <- list()
g.list <- list()
for(l in 1:Layers){
    #Generate the layers
    g.list[[l]] <- igraph::erdos.renyi.game(Nodes, runif(1, 1,1.4)*log(Nodes)/Nodes, directed=F)

    #Get the list of adjacency matrices which build the multiplex
    nodeTensor[[l]] <- igraph::get.adjacency( g.list[[l]] )
}

#Define the network of layers
layerTensor <- BuildLayersTensor(Layers=Layers, OmegaParameter=layerCouplingStrength, MultisliceType=networkOfLayersType)
layerLabels <- 1:Layers

#Build the multilayer adjacency tensor
M <- BuildSupraAdjacencyMatrixFromEdgeColoredMatrices(nodeTensor, layerTensor, Layers, Nodes)

#Get the nodes in the largest connected component
lcc <- GetGiantConnectedComponent(M,Layers,Nodes)
#Get the nodes in the largest intersection component
lic <- GetGiantIntersectionComponent(M,Layers,Nodes)
#Get the nodes in the largest viable component
lvc <- GetGiantViableComponent(M,Layers,Nodes)

#Generate the coordinates for layouting our networks and other network properties
for(l in 1:Layers){
    V(g.list[[l]])$deg <- deg
    V(g.list[[l]])$color.lcc <- "0"
    V(g.list[[l]])$color.lcc[lcc] <- "1"
    V(g.list[[l]])$alpha.lcc <- 0.2
    V(g.list[[l]])$alpha.lcc[lcc] <- 1

    V(g.list[[l]])$color.lic <- "0"
    V(g.list[[l]])$color.lic[lic] <- "1"
    V(g.list[[l]])$alpha.lic <- 0.2
    V(g.list[[l]])$alpha.lic[lic] <- 1

    V(g.list[[l]])$color.lvc <- "0"
    V(g.list[[l]])$color.lvc[lvc] <- "1"
    V(g.list[[l]])$alpha.lvc <- 0.2
    V(g.list[[l]])$alpha.lvc[lvc] <- 1    
}
layout.mux <- layoutMultiplex(g.list, layout="fr", ggplot.format=T)

p <- list()
for(l in 1:Layers){
    p[[l]] <- ggraph(layout.mux[[l]]) + theme_void() +
                geom_edge_link(colour=layer.colors[l], show.legend = FALSE) + 
                geom_node_point(alpha=V(g.list[[l]])$alpha.lcc, size = V(g.list[[l]])$alpha.lcc, colour=layer.colors[l], stroke=2) +  
                theme(legend.position="none", plot.title=element_text(size=12, hjust=0.5, face="bold", colour="#888888", vjust=-1))
                #ggtitle(paste("Layer", l))
}
for(l in 1:Layers){
    p[[l+Layers]] <- ggraph(layout.mux[[l]]) + theme_void() +
                geom_edge_link(colour=layer.colors[l], show.legend = FALSE) + 
                geom_node_point(alpha=V(g.list[[l]])$alpha.lic, size = V(g.list[[l]])$alpha.lic, colour=layer.colors[l], stroke=2) + 
                theme(legend.position="none", plot.title=element_text(size=12, hjust=0.5, face="bold", colour="#888888", vjust=-1))
}
for(l in 1:Layers){
    p[[l+2*Layers]] <- ggraph(layout.mux[[l]]) + theme_void() +
                geom_edge_link(colour=layer.colors[l], show.legend = FALSE) + 
                geom_node_point(alpha=V(g.list[[l]])$alpha.lvc, size = V(g.list[[l]])$alpha.lvc, colour=layer.colors[l], stroke=2) + 
                theme(legend.position="none", plot.title=element_text(size=12, hjust=0.5, face="bold", colour="#888888", vjust=-1)) 
}

multiplot(p[[1]], p[[4]], p[[7]], p[[2]], p[[5]], p[[8]], p[[3]], p[[6]], p[[9]], cols=3)




