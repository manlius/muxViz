source("muxLib.R")
library(reshape2)
library(RColorBrewer)

#Or build by specifying layers
Layers <- 5
Nodes <- 100
NodeTensor <- list()
NodeTensor.cmI <- list()
NodeTensor.cmII <- list()
g.list <- list()

#Create 5 correlated layers
g.list[[1]] <- igraph::barabasi.game(Nodes, m=1, directed=F)

#generate a second layer, with 25% edge overlap, while preserving degree distribution
g.list[[2]] <- GetRewiredLayerWithOverlappingEdges(g.list[[1]], 0.25, tol=1e-2)

#generate a third layer, with 50% edge overlap, while preserving degree distribution
g.list[[3]] <- GetRewiredLayerWithOverlappingEdges(g.list[[1]], 0.50, tol=1e-2)

#generate a fourth layer, with 75% edge overlap, while preserving degree distribution
g.list[[4]] <- GetRewiredLayerWithOverlappingEdges(g.list[[1]], 0.75, tol=1e-2)

#generate a fifth layer, with 95% edge overlap, while preserving degree distribution
g.list[[5]] <- GetRewiredLayerWithOverlappingEdges(g.list[[1]], 0.95, tol=1e-2)


#Uncomment the loop below to try another multiplex setup and see how results change
# for(l in 1:Layers){
#     g.list[[l]] <- erdos.renyi.game(Nodes, p=1.5*log(Nodes)/Nodes, directed=F)    
# }


#Create configuration model of type I (wash out intra-layer correlations, keep inter-layer ones)
g.list.cmI <- GetConfigurationModelTypeIFromNetworkList(g.list, isDirected=NULL, method="vl", verbose=F)
   
#Create configuration model of type II (wash out both intra-/inter-layer correlations)
g.list.cmII <- GetConfigurationModelTypeIIFromNetworkList(g.list, isDirected=NULL, method="vl", verbose=F)



#Define the layers by their adjacency matrices
for(l in 1:Layers){
    NodeTensor[[l]] <- igraph::get.adjacency(g.list[[l]])
    NodeTensor.cmI[[l]] <- igraph::get.adjacency(g.list.cmI[[l]])
    NodeTensor.cmII[[l]] <- igraph::get.adjacency(g.list.cmII[[l]])
}

#Define how layers are interconnected in all networks
LayerTensor <- Matrix(1, Layers, Layers) - speye(Layers)
#Remind that the above one is a lower-level version of this one:
#LayerTensor <- BuildLayersTensor(Layers=Layers, OmegaParameter=1, MultisliceType="Categorical")

#Build the multilayer adjacency tensors
M <- BuildSupraAdjacencyMatrixFromEdgeColoredMatrices(NodeTensor, LayerTensor, Layers, Nodes)
M.cmI <- BuildSupraAdjacencyMatrixFromEdgeColoredMatrices(NodeTensor.cmI, LayerTensor, Layers, Nodes)
M.cmII <- BuildSupraAdjacencyMatrixFromEdgeColoredMatrices(NodeTensor.cmII, LayerTensor, Layers, Nodes)


#Let's verify that correlations change as expected

cat("Intra-layer assortative mixing\n")
cat("==============================\n")
cat("           Orig.   CM-I   CM-II\n")
for(l in 1:Layers){
    cat(paste("Layer",l,":",
        round(assortativity(g.list[[l]],degree(g.list[[l]]),NULL),3),
        round(assortativity(g.list.cmI[[l]],degree(g.list.cmI[[l]]),NULL),3),
        round(assortativity(g.list.cmII[[l]],degree(g.list.cmII[[l]]),NULL),3),
        "\n"
    ))
}

cat("\n")
cat("Inter-layer assortative mixing\n")
cat("==============================\n")
cat("          Orig. CM-I CM-II\n")
for(l in 1:(Layers-1)){
    for(l2 in (l+1):Layers){
        cat(paste("Layers",l,l2,":",
            round(cor(degree(g.list[[l]]),degree(g.list[[l2]]),method="pearson"),3),
            round(cor(degree(g.list.cmI[[l]]),degree(g.list.cmI[[l2]]),method="pearson"),3),
            round(cor(degree(g.list.cmII[[l]]),degree(g.list.cmII[[l2]]),method="pearson"),3),
            "\n"
        ))
    }
}
cat("Note that Orig. and CM-I should be the same (1, when using the BA example), while CM-2 can be anything in [-1,1]\n")


#Estimate correlations now, let's use Spearman rho
LL.cor <- GetInterAssortativityTensor(M,Layers,Nodes,F,"TT")$InterSpearman
LL.cor.df <- melt(as.matrix(LL.cor))
LL.cor.df$type <- "Original"

LL.cor.cmI <- GetInterAssortativityTensor(M.cmI,Layers,Nodes,F,"TT")$InterSpearman
LL.cor.cmI.df <- melt(as.matrix(LL.cor.cmI))
LL.cor.cmI.df$type <- "CM Type I"

LL.cor.cmII <- GetInterAssortativityTensor(M.cmII,Layers,Nodes,F,"TT")$InterSpearman
LL.cor.cmII.df <- melt(as.matrix(LL.cor.cmII))
LL.cor.cmII.df$type <- "CM Type II"

#Assemble the results
LL.corrs.df <- rbind(LL.cor.df, LL.cor.cmI.df, LL.cor.cmII.df)

p <- ggplot(LL.corrs.df, aes(Var1, Var2, fill=value, group=type)) + theme_bw() +
     theme(panel.grid=element_blank()) + xlab("") + ylab("") +
     scale_fill_gradientn(name="Correlation", colors=brewer.pal(9, "YlOrRd")) +
     geom_tile() + 
     facet_wrap(.~type, ncol=3)

png("mux_configmodels.png", width=1024*1.5, height=1024*0.5, res=120)
print(p)
dev.off()









