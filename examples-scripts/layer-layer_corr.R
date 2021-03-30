library("muxViz")

#Remind to comment this line to have different results
set.seed(123456)

Layers <- 10
Nodes <- 100

cat("####################################\n")
cat("# Layer-Layer correlations\n")
cat("####################################\n\n")

layerCouplingStrength <- 1
networkOfLayersType <- "categorical"

g.list <- list()
nodeTensor <- list()


for(l in 1:Layers){
    #Generate the layers
    g.list[[l]] <- igraph::erdos.renyi.game(Nodes, runif(1, 0.1,1.5)*log(Nodes)/Nodes, directed=F)

    #Get the list of adjacency matrices which build the multiplex
    nodeTensor[[l]] <- igraph::get.adjacency( g.list[[l]] )
}
layerTensor <- BuildLayersTensor(Layers=Layers, OmegaParameter=layerCouplingStrength, MultisliceType=networkOfLayersType)
layerLabels <- 1:Layers

M <- BuildSupraAdjacencyMatrixFromEdgeColoredMatrices(nodeTensor, layerTensor, Layers, Nodes)

library(reshape2)
library(ggplot2)
library(RColorBrewer)

#Edge overlapping
LL.cor1 <- GetAverageGlobalOverlappingMatrix(M,Layers,Nodes)
LL.cor1.df <- melt(as.matrix(LL.cor1))
LL.cor1.df$type <- "Edge overlapping"

#Node overlapping
LL.cor2 <- GetAverageGlobalNodeOverlappingMatrix(M,Layers,Nodes)
LL.cor2.df <- melt(as.matrix(LL.cor2))
LL.cor2.df$type <- "Node overlapping"

#Assortativity
LL.cor3 <- GetInterAssortativityTensor(M,Layers,Nodes,F,"TT")$InterPearson
LL.cor3.df <- melt(as.matrix(LL.cor3))
LL.cor3.df$type <- "Deg-deg Pearson"

LL.cor4 <- GetInterAssortativityTensor(M,Layers,Nodes,F,"TT")$InterSpearman
LL.cor4.df <- melt(as.matrix(LL.cor4))
LL.cor4.df$type <- "Deg-deg Spearman"

#Shortest paths
LL.cor5 <- GetSPSimilarityMatrix(M,Layers,Nodes)
LL.cor5.df <- melt(as.matrix(LL.cor5))
LL.cor5.df$type <- "SP similarity"

LL.cor.df <- rbind(LL.cor1.df, LL.cor2.df, LL.cor3.df, LL.cor4.df, LL.cor5.df)

p <- ggplot(LL.cor.df, aes(Var1, Var2, fill=value, group=type)) + theme_bw() +
     theme(panel.grid=element_blank()) + xlab("") + ylab("") +
     scale_fill_gradientn(colors=brewer.pal(9, "YlOrRd")) +
     geom_tile() + 
     facet_wrap(.~type, ncol=2)

print(p)
