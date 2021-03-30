source("muxLib.R")

#Or build by specifying layers
Layers <- 5
Nodes <- 100
NodeTensor <- list()
g.list <- list()

g.list[[1]] <- igraph::barabasi.game(Nodes, m=1, directed=F)

#generate a second layer, with 25% edge overlap, while preserving degree distribution
g.list[[2]] <- GetRewiredLayerWithOverlappingEdges(g.list[[1]], 0.25, tol=1e-2)

#generate a third layer, with 50% edge overlap, while preserving degree distribution
g.list[[3]] <- GetRewiredLayerWithOverlappingEdges(g.list[[1]], 0.50, tol=1e-2)

#generate a fourth layer, with 75% edge overlap, while preserving degree distribution
g.list[[4]] <- GetRewiredLayerWithOverlappingEdges(g.list[[1]], 0.75, tol=1e-2)

#generate a fifth layer, with 95% edge overlap, while preserving degree distribution
g.list[[5]] <- GetRewiredLayerWithOverlappingEdges(g.list[[1]], 0.95, tol=1e-2)

#Define the layers by their adjacency matrices
for(l in 1:Layers){
    NodeTensor[[l]] <- igraph::get.adjacency(g.list[[l]])
}

#Define how layers are interconnected
LayerTensor <- Matrix(1, Layers, Layers) - speye(Layers)
#Remind that the above one is a lower-level version of this one:
#LayerTensor <- BuildLayersTensor(Layers=Layers, OmegaParameter=1, MultisliceType="Categorical")

#Build the multilayer adjacency tensor
M <- BuildSupraAdjacencyMatrixFromEdgeColoredMatrices(NodeTensor, LayerTensor, Layers, Nodes)

#Calculate the global edge overlap between each pair of layers
#The output numbers are arranged into a LxL matrix, encoding pairs of layers
#and should be compatible with the values imposed by construction at the beginning
OV <- GetAverageGlobalOverlappingMatrix(M,Layers,Nodes,fastBinary=T)
print(OV)

#Calculate PageRank and Degree versatility
pr <- GetMultiPageRankCentrality(M, Layers,Nodes)
deg <- GetMultiDegree(M, Layers,Nodes, isDirected=F)

mypal <- brewer.pal(Layers, "Set1")

#Generate the coordinates for layouting our networks. Using 1st layer as template.
lay <- igraph::layout_with_fr(g.list[[1]])

p <- list()

for(l in 1:length(NodeTensor)){
    layout <- create_layout(g.list[[l]], layout = 'drl')
    layout$x <- lay[,1]
    layout$y <- lay[,2]

    V(g.list[[l]])$pr <- pr
    V(g.list[[l]])$deg <- deg

    p[[l]] <- ggraph(layout) + theme_void() +
                geom_edge_link(colour=mypal[l], show.legend = FALSE) + 
                geom_node_point(aes(size = pr, alpha=pr), color=mypal[l]) + 
                theme(legend.position="bottom", plot.title=element_text(size=16, hjust=0.5, face="bold", colour=mypal[l], vjust=-1)) + 
                ggtitle(paste("Layer", l)) +
                guides(size=guide_legend(title="MuxPR"), alpha=F)
}

png("mux_increasing_overlapping.png", width=1024*1.75, height=1024*0.5, res=120)
multiplot(p[[1]], p[[2]], p[[3]], p[[4]], p[[5]], cols=5)
dev.off()





