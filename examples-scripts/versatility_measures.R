library("muxViz")

#This dataset is provided with the "standalone" library
config.file <- "data/StarWars/StarWars_config.txt"

#This flag is important to understand if input networks are directed or not
isDirected <- F
#This flag is important to understand if input networks are weighted or not
isWeighted <- T
#Network of layers (ordinal or categorical) and coupling strength
networkOfLayersType <- "categorical"
LayerCouplingStrength <- 10



##################
#Build network
##################

mux <- buildMultilayerNetworkFromMuxvizFiles(config.file=config.file,  
                                             isDirected=isDirected,
                                             isWeighted=isWeighted,
                                             MultisliceType=networkOfLayersType, 
                                             LayerCouplingStrength=LayerCouplingStrength, 
                                             format="muxviz edge-colored",
                                             verbose=T)


res <- list()

res[["degree"]] <- data.frame()
res[["strength"]] <- data.frame()
res[["pagerank"]] <- data.frame()
res[["closeness"]] <- data.frame()
res[["hub"]] <- data.frame()
res[["authority"]] <- data.frame()

##################
#Multilayer analysis
##################

#Degree versatility
deg.mux <- getVersatilityProfile(mux, type="degree")

#Strength versatility
str.mux <- getVersatilityProfile(mux, type="strength")

#PageRank versatility 
pr.mux <- getVersatilityProfile(mux, type="pagerank")

#Closeness versatility 
cl.mux <- getVersatilityProfile(mux, type="closeness")

#HITS versatilities
hub.mux <- getVersatilityProfile(mux, type="hub")
auth.mux <- getVersatilityProfile(mux, type="authority")

res[["degree"]] <- data.frame(node=mux$nodeIDs, mux=deg.mux)
res[["strength"]] <- data.frame(node=mux$nodeIDs, mux=str.mux)
res[["pagerank"]] <- data.frame(node=mux$nodeIDs, mux=pr.mux)
res[["closeness"]] <- data.frame(node=mux$nodeIDs, mux=cl.mux)
res[["hub"]] <- data.frame(node=mux$nodeIDs, mux=hub.mux)
res[["authority"]] <- data.frame(node=mux$nodeIDs, mux=auth.mux)

##################
#Aggregate analysis
##################

#Using standard igraph library for this

#Degree centrality
deg.agg <- igraph::degree(mux$g.agg)

#Strength centrality
str.agg <- igraph::strength(mux$g.agg)

#PageRank centrality 
pr.agg <- igraph::page_rank(mux$g.agg)$vector
pr.agg <- pr.agg/max(pr.agg)

#Closeness centrality 
cl.agg <- igraph::closeness(mux$g.agg, normalized=T)

#HITS centrality
hub.agg <- igraph::hub_score(mux$g.agg)$vector
auth.agg <- igraph::authority_score(mux$g.agg)$vector

res[["degree"]]$agg <- deg.agg
res[["strength"]]$agg <- str.agg
res[["pagerank"]]$agg <- pr.agg
res[["closeness"]]$agg <- cl.agg
res[["hub"]]$agg <- hub.agg
res[["authority"]]$agg <- auth.agg

##################
#Single-layer analysis
##################

#Using standard igraph library for this

for(l in 1:mux$Layers){

    #Degree centrality
    deg.l <- igraph::degree(mux$g.list[[l]])

    #Strength centrality
    str.l <- igraph::strength(mux$g.list[[l]])

    #PageRank centrality 
    pr.l <- igraph::page_rank(mux$g.list[[l]])$vector
    pr.l <- pr.l/max(pr.l)

    #Closeness centrality 
    cl.l <- igraph::closeness(mux$g.list[[l]], normalized=T)

    #HITS centrality
    hub.l <- igraph::hub_score(mux$g.list[[l]])$vector
    auth.l <- igraph::authority_score(mux$g.list[[l]])$vector

    res[["degree"]] <- cbind(res[["degree"]], deg.l)
    res[["strength"]] <- cbind(res[["strength"]], str.l)
    res[["pagerank"]] <- cbind(res[["pagerank"]], pr.l)
    res[["closeness"]] <- cbind(res[["closeness"]], cl.l)
    res[["hub"]] <- cbind(res[["hub"]], hub.l)
    res[["authority"]] <- cbind(res[["authority"]], auth.l)
}
colnames(res[["degree"]])[4:(3+mux$Layers)] <- paste0("layer",1:mux$Layers)
colnames(res[["strength"]])[4:(3+mux$Layers)] <- paste0("layer",1:mux$Layers)
colnames(res[["pagerank"]])[4:(3+mux$Layers)] <- paste0("layer",1:mux$Layers)
colnames(res[["closeness"]])[4:(3+mux$Layers)] <- paste0("layer",1:mux$Layers)
colnames(res[["hub"]])[4:(3+mux$Layers)] <- paste0("layer",1:mux$Layers)
colnames(res[["authority"]])[4:(3+mux$Layers)] <- paste0("layer",1:mux$Layers)


#Just plot correlation plots for the case of pagerank

library(GGally)
png("mux_centrality_stats_1.png", width=1024, height=1024, res=120)
p <- ggscatmat(res[["pagerank"]], columns = 2:ncol(res[["pagerank"]]), alpha=0.6, corMethod="spearman") + theme_bw() + theme(panel.grid=element_blank()) + xlab("") + ylab("")
print(p)
dev.off()

library(corrplot)
png("mux_centrality_stats_2.png", width=1024, height=1024,, res=200)
corrplot(cor(res[["pagerank"]][,2:ncol(res[["pagerank"]])]))
dev.off()
