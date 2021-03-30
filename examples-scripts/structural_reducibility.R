library("muxViz")
library(igraph)

#Or build by specifying layers
Layers <- 10
Nodes <- 100
NodeTensor <- list()
g.list <- list()

g.list[[1]] <- barabasi.game(Nodes, m=1, directed=F)

#generate additional layers with increasing % of overlapping edges, while preserving degree distribution
ov.fractions <- seq(0.1,0.9,0.1)
for(l in 2:10){
    g.list[[l]] <- GetRewiredLayerWithOverlappingEdges(g.list[[1]], ov.fractions[l-1], tol=1e-2)
}


#Define the layers by their adjacency matrices
for(l in 1:Layers){
    NodeTensor[[l]] <- get.adjacency(g.list[[l]])
}

#Define how layers are interconnected
LayerTensor <- Matrix(1, Layers, Layers) - speye(Layers)

#Build the multilayer adjacency tensor
M <- BuildSupraAdjacencyMatrixFromEdgeColoredMatrices(NodeTensor, LayerTensor, Layers, Nodes)

#use Ward linkage for hierarchical clustering and Categorical network of layers
Method <- "ward.D2"
Type <- "Categorical"

struct.red <- GetMultilayerReducibility(M,Layers,Nodes,Method,Type)

png("mux_structural_reducibility_hclust.png", width=1024, height=1024, res=120)
gplots::heatmap.2(as.matrix(struct.red$JSD), trace="none")
dev.off()

df.quality <- data.frame(step=0:(length(struct.red$relgQualityFunction)-1), q=struct.red$relgQualityFunction)
png("mux_structural_reducibility_quality.png", width=1024, height=1024*0.5, res=120)
p <- ggplot(df.quality, aes(step,q)) + theme_bw() +
     geom_line(color="steelblue") + geom_point(color="steelblue") +
     xlab("Merging Step, m") + ylab("Quality function, q(m)")
 print(p)
dev.off()



