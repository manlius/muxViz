source("muxLib.R")

#Or build by specifying layers
Layers <- 3
Nodes <- 90
NodeTensor <- list()

#Generate an edge-colored network with SBM structure
g1 <- erdos.renyi.game(40, log(40)/40) 
g2 <- erdos.renyi.game(30, log(30)/30) 
g3 <- erdos.renyi.game(20, log(20)/20) 

g.list <- list()

g.list[[1]] <- g1 %du% delete_edges(g2, sample( E(g2), floor(0.9*length(E(g2))) ) ) %du% delete_edges(g3, sample( E(g3), floor(0.9*length(E(g3))) ) )
g.list[[2]] <- delete_edges(g1, sample( E(g1), floor(0.9*length(E(g1))) ) ) %du% g2 %du% delete_edges(g3, sample( E(g3), floor(0.9*length(E(g3))) ) ) 
g.list[[3]] <- delete_edges(g1, sample( E(g1), floor(0.9*length(E(g1))) ) ) %du% delete_edges(g2, sample( E(g2), floor(0.9*length(E(g2))) ) ) %du% g3

    
for(l in 1:Layers){

    g.list[[l]] <- add.edges(g.list[[l]], c(40,41,70,71,39,90,69,89))

    #Make it weighted, with random weights
    E(g.list[[l]])$weight <- runif(length(E(g.list[[l]])), 0.5, 1)
    
    NodeTensor[[l]] <- get.adjacency( g.list[[l]], attr="weight" )
}



#Let's consider a chain of layers, aka an ordinal network of layers
LayerTensor <- diagR(c(1,1), 3, 1) + diagR(c(1,1), 3, -1)

#Build the multilayer adjacency tensor
M <- BuildSupraAdjacencyMatrixFromEdgeColoredMatrices(NodeTensor, LayerTensor, Layers, Nodes)

#Calculate PageRank and Degree versatility
pr <- GetMultiPageRankCentrality(M, Layers,Nodes)
deg <- GetMultiDegree(M, Layers,Nodes, isDirected=F)



mypal <- brewer.pal(Layers, "Set1")

#Generate the coordinates for layouting our networks. Using layer 1 as a template
lay <- layout_with_fr(graph_from_adjacency_matrix( GetAggregateMatrix(NodeTensor, Layers, Nodes), weighted=T, mode="undirected" ))

p <- list()

for(l in 1:length(NodeTensor)){
    layout <- create_layout(g.list[[l]], layout = 'drl')
    layout$x <- lay[,1]
    layout$y <- lay[,2]

    V(g.list[[l]])$pr <- pr
    V(g.list[[l]])$deg <- deg
    V(g.list[[l]])$pr[deg==0] <- 0

    p[[l]] <- ggraph(layout) + theme_void() +
                geom_edge_link(colour=mypal[l], show.legend = FALSE) + 
                geom_node_point(aes(size = pr, alpha=pr), color=mypal[l]) + 
                theme(legend.position="bottom", plot.title=element_text(size=16, hjust=0.5, face="bold", colour=mypal[l], vjust=-1)) + 
                ggtitle(paste("Layer", l)) +
                guides(size=guide_legend(title="MuxPR"), alpha=F)
}


#Build heatmap for the adjacency matrix of each layer
h <- list()

for(l in 1:length(NodeTensor)){
    df <- melt(as.matrix(NodeTensor[[l]]))

    h[[l]] <- ggplot(df, aes(Var1, Var2)) + theme_void() + 
                 geom_tile(aes(fill=value)) + 
                 scale_fill_gradientn(name="Weight", colors=c("white", brewer.pal(9, "YlOrRd"))) +
                 theme(legend.position="bottom")
}


png("mux_sbm_heatmaps.png", width=1024*1.25, height=1024*0.75, res=120)
multiplot(p[[1]], h[[1]], p[[2]], h[[2]], p[[3]], h[[3]], cols=3)
dev.off()


