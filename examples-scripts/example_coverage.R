library(muxViz)
library(igraph)
library(RColorBrewer)
library(viridis)
library(tictoc)
library(tidyverse)

set.seed(1)

#Network setup
Layers <- 3
Nodes <- 100
layerCouplingStrength <- 0.1
networkOfLayersType <- "categorical"
isDirected <- F

colors <- brewer.pal(8, "Set2")


cat("####################################\n")
cat("# Multiplex navigability\n")
cat("####################################\n\n")

SYSTEMS <- c("BA+BA", "BA+ER", "ER+ER")
TAUS <- 10 ^ seq(-1, 3, 0.05)

cov.res <- data.frame()

for (system.type in SYSTEMS) {
    #Generate an edge-colored network
    nodeTensor <- list()
    g.list <- list()
    
    cat(paste("### System", system.type, "\n"))
    
    cat("+ Creating layers...\n")
    
    for (l in 1:Layers) {
        if (system.type == "BA+ER") {
            if (l %% 2 == 0) {
                g.list[[l]] <- igraph::barabasi.game(Nodes, m = 2, directed = F)
            } else{
                g.list[[l]] <-
                    igraph::erdos.renyi.game(Nodes, 2 * log(Nodes) / Nodes, directed = F)
            }
        }
        
        if (system.type == "BA+BA") {
            g.list[[l]] <- igraph::barabasi.game(Nodes, m = 2, directed = F)
        }
        
        if (system.type == "ER+ER") {
            g.list[[l]] <-
                igraph::erdos.renyi.game(Nodes, 2 * log(Nodes) / Nodes, directed = F)
        }
        
        igraph::E(g.list[[l]])$weight <- 1
        
        #To make it weighted, with random weights, uncomment this:
        #igraph::E(g.list[[l]])$weight <- runif(length(E(g.list[[l]])), 0.5, 1)
        
        nodeTensor[[l]] <-
            igraph::get.adjacency(g.list[[l]], attr = "weight")
    }
    
    cat("+ Building supra-matrices...\n")
    
    #Build the interconnected multiplex transition matrix
    layerTensor <-
        BuildLayersTensor(Layers,
                          OmegaParameter = layerCouplingStrength,
                          MultisliceType = networkOfLayersType)
    SupraA <-
        BuildSupraAdjacencyMatrixFromEdgeColoredMatrices(nodeTensor, layerTensor, Layers, Nodes)
    SupraT <-
        BuildSupraTransitionMatrixFromSupraAdjacencyMatrix(SupraA, Layers, Nodes, Type =
                                                               "classical")
    
    #Build the non-interconnected multiplex transition matrix
    T.mux <-
        BuildTransitionMatrixFromEdgeColoredMatrices(nodeTensor, Layers, Nodes)
    
    #Build the aggregate representation of the multiplex
    A.agg <- GetAggregateMatrix(nodeTensor, Layers, Nodes)
    
    #Build the aggregate's transition matrix (this is atrick, but it can be confusing, maybe deserves a single-layer function)
    T.agg <- BuildTransitionMatrixFromSingleLayer(A.agg, Nodes)
    #L.agg <- speye(Nodes) - T.agg
    
    cat("+ Coverage multilayer...\n")
    
    #Uncomment this if you want to exact calculation as well
    # tic()
    # cov.mul <- GetCoverageEvolutionMultilayer(SupraT, Layers, Nodes, 10^seq(-1,3,0.05))
    # cov.mul$type <- "Exact"
    # cov.mul$system <- system.type
    # cov.res <- rbind(cov.res, cov.mul)
    # toc()
    
    DisconnectedNodes <-
        igraph::clusters(igraph::graph.adjacency(SupraA))$no
    
    tic()
    cov.mul.appr <-
        GetCoverageEvolutionMultilayer(
            SupraT,
            Layers,
            Nodes,
            TAUS,
            Approximate = T,
            Approximate.disconnected = DisconnectedNodes
        )
    cov.mul.appr$type <- "Approximate"
    cov.mul.appr$system <- system.type
    cov.mul.appr$method <- "Multilayer"
    cov.res <- rbind(cov.res, cov.mul.appr)
    toc()
    
    cat("+ Coverage multiplex...\n")
    
    DisconnectedNodes <-
        igraph::clusters(igraph::graph.adjacency(SupraA))$no
    
    tic()
    cov.mul.appr <-
        GetCoverageEvolutionEdgeColored(
            T.mux,
            Layers,
            Nodes,
            TAUS,
            Approximate = T,
            Approximate.disconnected = DisconnectedNodes
        )
    cov.mul.appr$type <- "Approximate"
    cov.mul.appr$system <- system.type
    cov.mul.appr$method <- "Multiplex"
    cov.res <- rbind(cov.res, cov.mul.appr)
    toc()
    
    cat("+ Coverage of the aggregate network...\n")
    
    DisconnectedNodes <-
        igraph::clusters(igraph::graph.adjacency(A.agg))$no
    
    tic()
    cov.mul.appr <-
        GetCoverageEvolutionSingleLayer(
            T.agg,
            Nodes,
            TAUS,
            Approximate = T,
            Approximate.disconnected = DisconnectedNodes
        )
    cov.mul.appr$type <- "Approximate"
    cov.mul.appr$system <- system.type
    cov.mul.appr$method <- "Aggregate"
    cov.res <- rbind(cov.res, cov.mul.appr)
    toc()
}

#Calculations might lead to complex numbers for coverage, with zero imaginary part.
#Check, and if not the case raise an error
if (any(abs(Im(cov.res$rho)) > 1e-12)) {
    stop("ERROR! Unexpected imaginary numbers.")
} else{
    cov.res$rho <- Re(cov.res$rho)
}


p1 <- cov.res %>%
    ggplot(aes(tau, rho, group = system, color = system)) + 
    theme_minimal() + 
    theme(panel.grid = element_blank()) +
    geom_line() + 
    geom_point(size = 0.5, aes(shape = system)) +
    scale_x_log10() +
    scale_color_manual(values = colors) +
    labs(x = "Diffusion time", y = "Coverage") +
    facet_wrap(method ~ ., ncol = 1)

print(p1)
