source("muxLib.R")

#Comment this line to generate different networks
set.seed(12345)

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

#Build the aggregate representation of the multiplex
A <- GetAggregateMatrix(NodeTensor, Layers, Nodes)

#Path statistics
path.stats <- list()

cat("Multilayer path statistics...\n")
path.stats.mux <- GetMultiPathStatistics(M, Layers, Nodes)

cat(paste("Layer 1 path statistics...\n"))
path.stats.layer1 <- GetMultiPathStatistics(NodeTensor[[1]], 1, Nodes)

cat(paste("Layer 2 path statistics...\n"))
path.stats.layer2 <- GetMultiPathStatistics(NodeTensor[[2]], 1, Nodes)

cat(paste("Layer 3 path statistics...\n"))
path.stats.layer3 <- GetMultiPathStatistics(NodeTensor[[3]], 1, Nodes)

cat("Aggregated network path statistics...\n")
path.stats.agg <- GetMultiPathStatistics(A, 1, Nodes)

cat("Average network path statistics...\n")
path.stats.avg <- GetMultiPathStatistics(A/3, 1, Nodes)

df.clos <- data.frame(mux=path.stats.mux$closeness,
                                  agg=path.stats.agg$closeness,
                                  avg=path.stats.avg$closeness,
                                  l1=path.stats.layer1$closeness,
                                  l2=path.stats.layer2$closeness,
                                  l3=path.stats.layer3$closeness)


#Some visualization of the results
library(GGally)
png("mux_path_stats_1.png", width=1024, height=1024, res=120)
p <- ggscatmat(df.clos, columns = 1:ncol(df.clos), alpha=0.6, corMethod="spearman") + theme_bw() + theme(panel.grid=element_blank()) + xlab("") + ylab("")
print(p)
dev.off()

#or just a correlation plot
library(corrplot)
png("mux_path_stats_2.png", width=1024, height=1024,, res=200)
corrplot(cor(df.clos))
dev.off()


