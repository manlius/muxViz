source("muxLib.R")


#Or build by specifying layers
Layers <- 3
Nodes <- 100
NodeTensor <- list()

#Generate an edge-colored network
g <- barabasi.game(Nodes, m=1, directed=F)
g.list <- list()
for(l in 1:Layers){
    g.list[[l]] <- delete_edges(g, sample( E(g), floor(0.2*length(E(g))) ) )
    NodeTensor[[l]] <- get.adjacency( g.list[[l]] )
}

#Let's consider a chain of layers, aka an ordinal network of layers
LayerTensor <- diagR(c(1,1), 3, 1) + diagR(c(1,1), 3, -1)

#Build the multilayer adjacency tensor
M <- BuildSupraAdjacencyMatrixFromEdgeColoredMatrices(NodeTensor, LayerTensor, Layers, Nodes)

#Calculate PageRank and Degree versatility
pr <- GetMultiPageRankCentrality(M, Layers,Nodes)
deg <- GetMultiDegree(M, Layers,Nodes, isDirected=F)


mypal <- brewer.pal(Layers, "Set1")

#Generate the coordinates for layouting our networks.
lay <- layout_with_fr(graph_from_adjacency_matrix( GetAggregateMatrix(NodeTensor, Layers, Nodes) ))

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

png("mux_BA_3layers.png", width=1024, height=1024*0.5, res=120)
multiplot(p[[1]], p[[2]], p[[3]], cols=3)
dev.off()

