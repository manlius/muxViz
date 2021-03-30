source("muxLib.R")


Layers <- 4
Nodes <- 100

#Before doing this for data, I recommend to play a bit with smaller multilayers. 
#There is a function that you can call to generate simple random multiplex networks:
#
#GetSampleMultiplex <- function(Layers, Nodes, p)
#
#where p is the wiring probability of Erdos-Renyi networks. 
#If you set it such that p > (log N)/N, being N the number of nodes, 
#you will have almost surely layers with no separated components. Therefore, an example:

cat("##############\n")
cat("# Example n° 1\n")
cat("##############\n\n")

#Generate a sample multiplex with ER layers
M <- GetSampleMultiplex(Layers, Nodes, log(Nodes)/Nodes)

#Get the list of adjacency matrices which build the multiplex
nodeTensor <- SupraAdjacencyToNodesTensor(M, Layers, Nodes)
    
#Build the aggregate representation of the multiplex
A <- GetAggregateMatrix(nodeTensor, Layers, Nodes)
A <- binarizeMatrix(A)

#Get a visual taste of the flattened supra-adjacecy tensor
image(M)

res <- GetMultiplexTriads(M, Layers, Nodes)

cat("\n\n")


#Some global stats 
n.tri <- list()
clu.glob <- list()

#for the multiplex
n.tri["mux"] <- GetGlobalNumberTriangles(M, Layers,Nodes)
clu.glob["mux"] <- GetAverageGlobalClustering(M, Layers,Nodes)

#and for each layer separately
for(l in 1:Layers){
    n.tri[paste0("layer_", l)] <- GetGlobalNumberTriangles(nodeTensor[[l]], 1,Nodes)
    clu.glob[paste0("layer_", l)] <- GetAverageGlobalClustering(nodeTensor[[l]], 1,Nodes)
}

#and for the aggregate
n.tri["agg"] <- GetGlobalNumberTriangles(A, 1, Nodes)
clu.glob["agg"] <- GetAverageGlobalClustering(A, 1, Nodes)

cat("Number of triangles (including degenerate cases):\n")
print(n.tri)
cat("Global clustering:\n")
print(clu.glob)

cat("\n\n\n")

######################################################
# Alternative way, with more control on single layers
######################################################

cat("##############\n")
cat("# Example n° 2\n")
cat("##############\n\n")

layerCouplingStrength <- 1
networkOfLayersType <- "categorical"

g.list <- list()
nodeTensor <- list()

g.list[[1]] <- igraph::erdos.renyi.game(Nodes, 0.5*log(Nodes)/Nodes, directed=F)
g.list[[2]] <- igraph::erdos.renyi.game(Nodes, 1.1*log(Nodes)/Nodes, directed=F)
g.list[[3]] <- igraph::erdos.renyi.game(Nodes, 2*log(Nodes)/Nodes, directed=F)
g.list[[4]] <- igraph::erdos.renyi.game(Nodes, 1.5*log(Nodes)/Nodes, directed=F)

#Get the list of adjacency matrices which build the multiplex
for(l in 1:Layers){
    nodeTensor[[l]] <- igraph::get.adjacency( g.list[[l]] )
}
layerTensor <- BuildLayersTensor(Layers=Layers, OmegaParameter=layerCouplingStrength, MultisliceType=networkOfLayersType)
layerLabels <- 1:Layers

#Build the aggregate representation of the multiplex
g.agg <- GetAggregateNetworkFromNetworkList(g.list)

res2 <- GetMultiplexTriadsFromNetworkList(g.list, g.agg=g.agg, verbose=T)

########################################################
