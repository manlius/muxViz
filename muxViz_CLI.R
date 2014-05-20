#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# MuxVisual: Visualization of Multiplex Networks with R & openGL
#
# Version: 0.1
# Last update: Jan 2014
# Authors: Manlio De Domenico
#               Universitat Rovira i Virgili, Tarragona (Spain)
#
# History:
#
# More about RGL:
# http://cran.r-project.org/web/packages/rgl/rgl.pdf
# http://hosho.ees.hokudai.ac.jp/~kubo/Rdoc/library/rgl/html/00Index.html
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(igraph)
library(rgl)
library(mapproj)
library(maps)
library(OpenStreetMap)

###############
#Input Parameters
###############

#==== File list with input networks
#Format: path/edgelist_file;layerLabel;path/layout_file
inputList <- "layers_list.txt"

#==== Network type
DIRECTED <- F
WEIGHTED <- F

#==== Choose the layout
LAYOUT_FRUCHTERMAN_REINGOLD <- T    #for networks with < 1000 nodes
LAYOUT_LGL <- F                                              #for networks with > 1000 nodes
LAYOUT_DRL <- F                                              #for networks with > 1000 nodes
LAYOUT_SPRING <- F                                       #for networks with < 100 nodes
LAYOUT_KAMADA_KAWAI <- F                        #for networks with < 100 nodes
LAYOUT_REINGOLD_TILFORD <- F                #for networks with < 1000 nodes
LAYOUT_COMBINED <- F                                 #for large and complicated networks...
LAYOUT_MAXITER <- 1000
LAYOUT_BY_LAYER_ID <- 0                            #0: use the aggregated, >0 use that layer ID
LAYOUT_INDEPENDENT <- F                           #if each layer can be layouted separately
                                                                            #If external layout is provided for each layer, it will overwrite
                                                                            #the above alternatives

#==== Output/Export options
FILE_RGL_SNAPSHOT <- "muxViz.png"
FILE_RGL_MOVIE <- "muxViz_movie.png"
EXPORT_MOVIE <- F

#==== Community detection algorithm
COMMUNITY_EDGE_BETWEENNESS <- F
COMMUNITY_RANDOM_WALK_TRAP <- F
COMMUNITY_INFOMAP <- T

#==== Graphic options
LAYER_SHOW <- T
AGGREGATE_SHOW <- T
NODE_LABELS_SHOW <- F
GEOGRAPHIC_BOUNDARIES_SHOW <- T
GEOGRAPHIC_BOUNDARIES_AGGREGATE_SHOW <- T
OSMType <- "stamen-watercolor"               #this fix the type of map to be used in the background
                                                                #osm-bbike-german
                                                                #bing
INTERLINK_SHOW <- F
INTERLINK_SHOW_FRACTION <- 0.2 #this allows to show only a few (randomly chosen) interlinks
                                                                #0: no interlinks 1: show all
RESCALE_WEIGHT <- T

NODE_TRANSP <- 0.2
EDGE_TRANSP <- 0.2
INTERLINK_TRANSP <- 0.2

PLOT_TITLE <- ""
PLOT_SUBTITLE <- ""
PLOT_FOV <- 20  #deg, can be changed with mouse
LAYER_COLOR <- "gray"
LAYER_LABEL_PREFIX <- "L"
LAYER_AGGREGATE_COLOR <- "blue"
LAYER_AGGREGATE_LABEL_PREFIX <- "Aggregate"
LAYER_AGGREGATE_TRANSP <- 0.1
LAYER_TRANSP <- 0.1
LAYER_SHIFT <- 1                                #this shift the layers along x-axis to improve the perspective
LAYER_ARROW_SIZE <- 1
LAYER_ARROW_WIDTH <- 1
LAYER_ID_SHOW_TOPLEFT <- F
LAYER_ID_SHOW_BOTTOMLEFT<- T
LAYER_ID_SHOW_TOPRIGHT <- F
LAYER_ID_SHOW_BOTTOMRIGHT<- F
LAYER_ID_FONTSIZE <- 0.9
NODE_DEFAULT_SIZE <- 2
NODE_SIZE_PROPORTIONAL_TO_LOGPAGERANK <- F
NODE_SIZE_PROPORTIONAL_TO_STRENGTH <- F
NODE_SIZE_PROPORTIONAL_TO_LOGSTRENGTH <- T
NODE_SIZE_PROPORTIONAL_TO_LOGLOGSTRENGTH <- F
NODE_COLOR_BY_COMMUNITY <- T
COMMUNITY_MIN_SIZE <- 0     #will color with same RGB all nodes in communities smaller than this size
EDGE_DEFAULT_SIZE <- 1
EDGE_SIZE_PROPORTIONAL_TO_WEIGHT <- F
EDGE_SIZE_PROPORTIONAL_TO_LOGWEIGHT <- F
EDGE_SIZE_PROPORTIONAL_TO_LOGLOGWEIGHT <- T
EDGE_BENDING <- 0.2

INTERLINK_COLOR <- "black"
INTERLINK_TYPE <- "dotted"
INTERLINK_WIDTH <- 1

BACKGROUND_COLOR <- "#FFFFFF"

###############
#End Parameters
###############

LAYERS <- 1

#Read the input
fileInput <- readLines(inputList)
LAYERS <- length(fileInput)
layerEdges <- vector("list",LAYERS+1)
fileName <- vector("list",LAYERS)
layerLabel <- vector("list",LAYERS+1)
layerLayoutFile <- vector("list",LAYERS)
layerLayout <- vector("list",LAYERS+1)
nodesLabel <- vector("list",LAYERS+1)

for(l in 1:LAYERS){
    fileName[l] <- strsplit(fileInput[l],';')[[1]][1]
    layerLabel[l] <- strsplit(fileInput[l],';')[[1]][2]
    layerLayoutFile[l] <- strsplit(fileInput[l],';')[[1]][3]
    
    layerEdges[[l]] <-  read.table(fileName[[l]][1], header=F)
    
    if(layerLabel[[l]][1]=="" || is.na(layerLabel[[l]][1])){
        layerLabel[[l]][1] <- paste(LAYER_LABEL_PREFIX, l)
    }
}

layerLabel[[LAYERS+1]][1] <- LAYER_AGGREGATE_LABEL_PREFIX

#Find the minimum and maximum node ID in the multiplex
idmin <- 1e100
idmax <- 0
offset = 0;

for(l in 1:LAYERS){
    if( min(layerEdges[[l]][,1:2]) < idmin) idmin <- min(layerEdges[[l]][,1:2])
    if( max(layerEdges[[l]][,1:2]) > idmax) idmax <- max(layerEdges[[l]][,1:2])
}

if(idmin == 0) offset = 1;
Nodes = idmax + offset

print(paste("Minimum node ID: ",idmin))
print(paste("Maximum node ID: ",idmax))
print(paste("Using offset: ",offset))

#If all external layouts have been provided:
LAYOUT_EXTERNAL <- !is.na(all(layerLayoutFile != "")) 
GEOGRAPHIC_LAYOUT <- LAYOUT_EXTERNAL
XMAX <- -1e10
YMAX <- -1e10
XMIN <- 1e10
YMIN <- 1e10
LONGMAX <- -1e10
LATMAX <- -1e10
LONGMIN <- 1e10
LATMIN <- 1e10

#If each layout is specified correctly
for(l in 1:LAYERS){
    if(layerLayoutFile[[l]][1]!="" && (!is.na(layerLayoutFile[[l]][1]))){
        layerTable <- read.table(layerLayoutFile[[l]][1], header=T)
        layerLayout[[l]] <- matrix(c(1),nrow=Nodes,ncol=2)
        
        if(length(layerTable$nodeLat)==Nodes && length(layerTable$nodeLong)==Nodes){
            print(paste("Layout for layer",l,"is geographic. Converting."))
            #Get boundaries
            longBounds = c(min(layerTable$nodeLong),max(layerTable$nodeLong))
            latBounds = c(min(layerTable$nodeLat),max(layerTable$nodeLat))

            if(min(layerTable$nodeLong,na.rm=T) < LONGMIN) LONGMIN = min(layerTable$nodeLong,na.rm=T)
            if(min(layerTable$nodeLat,na.rm=T) < LATMIN) LATMIN = min(layerTable$nodeLat,na.rm=T)
            if(max(layerTable$nodeLong,na.rm=T) > LONGMAX) LONGMAX = max(layerTable$nodeLong,na.rm=T)
            if(max(layerTable$nodeLat,na.rm=T) > LATMAX) LATMAX = max(layerTable$nodeLat,na.rm=T)

            print(paste("  Latitude boundaries: ",min(layerTable$nodeLat),max(layerTable$nodeLat)))
            print(paste("  Longitude boundaries: ",min(layerTable$nodeLong),max(layerTable$nodeLong)))
            
            #The input layout is geographic, we must convert it to cartesian
            sphCoordinates <- list()
            sphCoordinates$x <- layerTable$nodeLong
            sphCoordinates$y <- layerTable$nodeLat
            cartCoordinates <- mapproject(sphCoordinates,proj="mercator")
            
            layerTable$nodeX <- cartCoordinates$x
            layerTable$nodeY <- cartCoordinates$y
        }
        
        if(length(layerTable$nodeX)==Nodes && length(layerTable$nodeY)==Nodes){
            layerLayout[[l]][layerTable$nodeID + offset,1:2] <- cbind(layerTable$nodeX,layerTable$nodeY)

            if(min(layerTable$nodeX,na.rm=T) < XMIN) XMIN = min(layerTable$nodeX,na.rm=T)
            if(min(layerTable$nodeY,na.rm=T) < YMIN) YMIN = min(layerTable$nodeY,na.rm=T)
            if(max(layerTable$nodeX,na.rm=T) > XMAX) XMAX = max(layerTable$nodeX,na.rm=T)
            if(max(layerTable$nodeY,na.rm=T) > YMAX) YMAX = max(layerTable$nodeY,na.rm=T)
            
            GEOGRAPHIC_LAYOUT <- GEOGRAPHIC_LAYOUT && T
            print(paste("Layout for layer",l,"specified correctly from external file",layerLayoutFile[[l]][1]))
        }else{
            print(paste("Layout for layer",l,"not specified correctly. Proceeding with automatic layouting."))
            LAYOUT_EXTERNAL <- F
            GEOGRAPHIC_LAYOUT <- F
        }

        if(length(layerTable$nodeLabel)==Nodes){
            print(paste("Nodes' labels for layer",l,"specified correctly from external file",layerLayoutFile[[l]][1]))            
            #Assign labels to nodes
            nodesLabel[[l]][layerTable$nodeID + offset] <- as.character(layerTable$nodeLabel)
            print("Assigned labels.")
        }else{
            print(paste("Nodes' labels for layer",l,"not specified correctly. Proceeding with automatic labeling."))
            nodesLabel[[l]] <- 1:Nodes
        }
    }else{
        print(paste("Layout for layer",l,"not specified correctly. Proceeding with automatic layouting."))
        LAYOUT_EXTERNAL <- F
        GEOGRAPHIC_LAYOUT <- F

        print(paste("Nodes' labels for layer",l,"not specified correctly. Proceeding with automatic labeling."))
        #Assign labels to nodes
        nodesLabel[[l]] <- 1:Nodes
    }
}

#giving the layout of the aggregate from external file makes no sense if it is different from the other layers
#and it is also annoying to be constrained to specify the aggregate, if one does not want to show it.
#Therefore, here I prefer to assign manually the layout of the first layer to the aggregate.
#So far, I accept this possibility just for sake of completeness, but a correct use of muxViz should avoid
#situations like this..
layerLayout[[LAYERS+1]] <- layerLayout[[1]]
nodesLabel[[LAYERS+1]] <- nodesLabel[[1]]

#Create the graph objects
g <- vector("list",LAYERS+1)

Adj_aggr <- matrix(0,ncol=Nodes,nrow=Nodes)

for(l in 1:LAYERS){    
    #create an undirected edges list (both directions specified) if requested
    if(!DIRECTED){
        if(ncol(layerEdges[[l]])==3){
            #swap columns
            tmpedges <- layerEdges[[l]][,c(2,1,3)]
            #change the name of the columns, or merge will not work
            colnames(tmpedges) <- c("V1","V2","V3")
            #merge
            layerEdges[[l]]<-merge(layerEdges[[l]],tmpedges,all=T)
        }else{
            #swap columns
            tmpedges <- layerEdges[[l]][,c(2,1)]
            #change the name of the columns, or merge will not work
            colnames(tmpedges) <- c("V1","V2")
            #merge
            layerEdges[[l]]<-merge(layerEdges[[l]],tmpedges,all=T)
        }
    }
    
    if(offset>0){
        layerEdges[[l]][,1] <- layerEdges[[l]][,1] + offset
        layerEdges[[l]][,2] <- layerEdges[[l]][,2] + offset
    }

    if(WEIGHTED){
        if(RESCALE_WEIGHT){
            if(ncol(layerEdges[[l]])==3){
                print("Rescaling weights...")
                layerEdges[[l]][,3] <- layerEdges[[l]][,3]/min(layerEdges[[l]][,3])
            }
        }
    }

    #We need the (weighted) adjacency matrix to build the aggregate network    
    A_layer <- matrix(0,ncol=Nodes,nrow=Nodes)

    #For weighted network, and for our requirements this should be done
    if(ncol(layerEdges[[l]])==3){
        for(i in 1:nrow(layerEdges[[l]])) A_layer[ layerEdges[[l]][i,1], layerEdges[[l]][i,2] ] <- layerEdges[[l]][i,3]
    }else{
        for(i in 1:nrow(layerEdges[[l]])) A_layer[ layerEdges[[l]][i,1], layerEdges[[l]][i,2] ] <- 1
    }
        
    if(WEIGHTED){
        g[[l]] <- graph.adjacency(A_layer,weighted=WEIGHTED)
    }else{
        g[[l]] <- graph.adjacency(A_layer,weighted=NULL)   
    }

    if(LAYOUT_BY_LAYER_ID>0){
        if(l==LAYOUT_BY_LAYER_ID){
            A_layer1 <- A_layer
        }
    }

    Adj_aggr <- Adj_aggr + A_layer
    
    print(paste("Layer ",l,": ",fileName[[l]][1],"   Name:",layerLabel[[l]][1]))
    print(paste("Layer ",l," Directed: ",DIRECTED))
    print(paste("Layer ",l," Weighted: ",WEIGHTED))
    print(paste(nrow(layerEdges[[l]]),"Edges in layer: ",l))
}

#the aggregate
g[[LAYERS+1]] <- graph.adjacency(Adj_aggr,weighted=T)

layouts <- vector("list",LAYERS+1)

#Check if the layouts are specified by external files, otherwise proceed with the automatic ones
if(!LAYOUT_EXTERNAL){
    if(!LAYOUT_INDEPENDENT){    
        print("Constrained layout option.")
        if(LAYOUT_BY_LAYER_ID){
            #It will use the first layer to layout the others
            if(WEIGHTED){
                gAggr <- graph.adjacency(A_layer1, weighted=T)
            }else{
                gAggr <- graph.adjacency(A_layer1, weighted=NULL)    
            }
        }else{
            #Aggregate graph from aggregate adjacency matrix
            gAggr <- graph.adjacency(Adj_aggr, weighted=T)
            print("Aggregate network created. Proceeding with layout to obtain coordinates for each layer.")
        }
        
        #Note that here, gAggr does not correspond to the aggregate when LAYOUT_BY_LAYER_ID is T 
        #But this is only confusing in this piece of code. I am too lazy now to change the name of this
        #variable, I keep this note for the future. The aggregate is in g[[LAYERS+1]]
        
        #Choose a layout and apply it to the aggregate network
        if(LAYOUT_FRUCHTERMAN_REINGOLD){
            lAggr <- layout.fruchterman.reingold.grid(gAggr,weights=E(gAggr)$weight,niter=LAYOUT_MAXITER,area=vcount(gAggr)^1.,repulserad=vcount(gAggr)^1.3,dim=2)
        }
        if(LAYOUT_LGL){
            lAggr <- layout.lgl(gAggr,maxiter=LAYOUT_MAXITER)
        }
        if(LAYOUT_DRL){
            lAggr <- layout.drl(gAggr,options=list(simmer.attraction=0,simmer.iterations=floor(LAYOUT_MAXITER*0.15),crunch.iterations=floor(LAYOUT_MAXITER*0.1),cooldown.iterations=floor(LAYOUT_MAXITER*0.25),expansion.iterations=floor(LAYOUT_MAXITER*0.25),liquid.iterations=floor(LAYOUT_MAXITER*0.25)))
        }
        
        if(LAYOUT_REINGOLD_TILFORD){
            lAggr <- layout.reingold.tilford(gAggr)
        }
        
        if(LAYOUT_KAMADA_KAWAI){
            lAggr <- layout.kamada.kawai(gAggr, niter=LAYOUT_MAXITER)
        }
        if(LAYOUT_SPRING){
            lAggr <- layout.spring(gAggr,repulse=T)
        }
        
        if(LAYOUT_COMBINED){
            #We try to use the DRL to scale and we use it as seed for a Kamada-Kawai with few iterations
            ltmp <- layout.drl(gAggr,options=list(simmer.attraction=0,simmer.iterations=floor(LAYOUT_MAXITER*0.15),crunch.iterations=floor(LAYOUT_MAXITER*0.1),cooldown.iterations=floor(LAYOUT_MAXITER*0.25),expansion.iterations=floor(LAYOUT_MAXITER*0.25),liquid.iterations=floor(LAYOUT_MAXITER*0.25)))
            
            lAggr <- layout.kamada.kawai(gAggr, niter=LAYOUT_MAXITER,start=ltmp)
        }
        
        #For compatibility with the other option, we assign lAggr to each layout, aggregate included
        for(l in 1:(LAYERS+1)){
            layouts[[l]] <- lAggr
        }
    }else{
        print("Independent layout option.")
        for(l in 1:(LAYERS+1)){
            layouts[[l]] <- matrix(c(1),ncol=3,nrow=Nodes)
            
            print(paste("  Layout for layer",l,"..."))
            #Each layout is calculated separately    
            if(LAYOUT_FRUCHTERMAN_REINGOLD){
                layouts[[l]] <- layout.fruchterman.reingold.grid(g[[l]],weights=E(g[[l]])$weight,niter=LAYOUT_MAXITER,area=vcount(g[[l]])^1.,repulserad=vcount(gAggr)^1.3,dim=2)
            }
            if(LAYOUT_LGL){
                layouts[[l]] <- layout.lgl(g[[l]],maxiter=LAYOUT_MAXITER)
            }
            if(LAYOUT_DRL){
                layouts[[l]] <- layout.drl(g[[l]],options=list(simmer.attraction=0,simmer.iterations=floor(LAYOUT_MAXITER*0.15),crunch.iterations=floor(LAYOUT_MAXITER*0.1),cooldown.iterations=floor(LAYOUT_MAXITER*0.25),expansion.iterations=floor(LAYOUT_MAXITER*0.25),liquid.iterations=floor(LAYOUT_MAXITER*0.25)))
            }
            
            if(LAYOUT_REINGOLD_TILFORD){
                layouts[[l]] <- layout.reingold.tilford(g[[l]])
            }
            
            if(LAYOUT_KAMADA_KAWAI){
                layouts[[l]] <- layout.kamada.kawai(g[[l]], niter=LAYOUT_MAXITER)
            }
            if(LAYOUT_SPRING){
                layouts[[l]] <- layout.spring(g[[l]],repulse=T)
            }
            
            if(LAYOUT_COMBINED){
                #We try to use the DRL to scale and we use it as seed for a Kamada-Kawai with few iterations
                ltmp <- layout.drl(g[[l]],options=list(simmer.attraction=0,simmer.iterations=floor(LAYOUT_MAXITER*0.15),crunch.iterations=floor(LAYOUT_MAXITER*0.1),cooldown.iterations=floor(LAYOUT_MAXITER*0.25),expansion.iterations=floor(LAYOUT_MAXITER*0.25),liquid.iterations=floor(LAYOUT_MAXITER*0.25)))
                
                layouts[[l]] <- layout.kamada.kawai(g[[l]], niter=LAYOUT_MAXITER,start=ltmp)
            }
        }
    }
}else{
    print("Layouting: external files.")
    for(l in 1:LAYERS){
        layouts[[l]] <- matrix(c(1),nrow=Nodes,ncol=2)
        layouts[[l]] <- layerLayout[[l]]
    }
    
    #giving the layout of the aggregate from external file makes no sense if it is different from the other layers
    #and it is also annoying to be constrained to specify the aggregate, if one does not want to show it.
    #Therefore, here I prefer to assign manually the layout of the first layer to the aggregate.
    #So far, I accept this possibility just for sake of completeness, but a correct use of muxViz should avoid
    #situations like this..

    layouts[[LAYERS+1]] <- layouts[[1]]
}

#Make it a 3-columns object
for(l in 1:(LAYERS+1)){
    layouts[[l]] <- cbind(layouts[[l]][,1:2],1)
}

if(!LAYOUT_EXTERNAL && !GEOGRAPHIC_LAYOUT){
    for(l in 1:(LAYERS+1)){
        if(min(layouts[[l]][,1],na.rm=T) < XMIN) XMIN = min(layouts[[l]][,1],na.rm=T)
        if(min(layouts[[l]][,2],na.rm=T) < YMIN) YMIN = min(layouts[[l]][,2],na.rm=T)
        if(max(layouts[[l]][,1],na.rm=T) > XMAX) XMAX = max(layouts[[l]][,1],na.rm=T)
        if(max(layouts[[l]][,2],na.rm=T) > YMAX) YMAX = max(layouts[[l]][,2],na.rm=T)
    }
}

print("Layouting finished. Proceeding with openGL plot of each layer.")

rgl.clear()

 for(l in 1:(LAYERS+1)){
    if(l==LAYERS+1){
        if(!AGGREGATE_SHOW){
            #if we don't want to show the aggregate, we must skip the rest
            next
        }
    }
    if(l<LAYERS+1){
        print(paste("Layer: ",l))
    }else{
        print(paste("Layer: Aggregate"))    
    }
    V(g[[l]])$vertex.label.color <- rgb(47,47,47,0,maxColorValue = 255)
    
    #this set the transparency level of edges and nodes.. it can be customized
    E(g[[l]])$alpha <- floor(EDGE_TRANSP*255)
    V(g[[l]])$alpha <- floor(NODE_TRANSP*255)

    #generate a random color for this layer
    Rcolor <- sample(0:255, 1, replace=T)
    Gcolor <- sample(0:255, 1, replace=T)
    Bcolor <- sample(0:255, 1, replace=T)

    #assign the color to the layer
    E(g[[l]])$red <- Rcolor
    E(g[[l]])$green <- Gcolor
    E(g[[l]])$blue <- Bcolor
    V(g[[l]])$red <- Rcolor
    V(g[[l]])$green <- Gcolor
    V(g[[l]])$blue <- Bcolor

    E(g[[l]])$color<-rgb(E(g[[l]])$red, E(g[[l]])$green, E(g[[l]])$blue, E(g[[l]])$alpha, maxColorValue=255)
    V(g[[l]])$color <- rgb(V(g[[l]])$red, V(g[[l]])$green, V(g[[l]])$blue, V(g[[l]])$alpha, maxColorValue=255)

    #other assignments
    E(g[[l]])$curve<- EDGE_BENDING

    if(!NODE_LABELS_SHOW){
        V(g[[l]])$label <- ""
    }else{
        V(g[[l]])$label <- nodesLabel[[l]]
    }

    V(g[[l]])$size <- NODE_DEFAULT_SIZE
    if(NODE_SIZE_PROPORTIONAL_TO_STRENGTH) V(g[[l]])$size = graph.strength(g[[l]]);
    if(NODE_SIZE_PROPORTIONAL_TO_LOGSTRENGTH) V(g[[l]])$size = 1+2*log(1+graph.strength(g[[l]]));
    if(NODE_SIZE_PROPORTIONAL_TO_LOGLOGSTRENGTH) V(g[[l]])$size = NODE_DEFAULT_SIZE*log(1+log(1+graph.strength(g[[l]])));
    if(NODE_SIZE_PROPORTIONAL_TO_LOGPAGERANK) V(g[[l]])$size = 1+1.2*log(1+Nodes*page.rank.old(g[[l]]));

    E(g[[l]])$size <- EDGE_DEFAULT_SIZE;
    if(WEIGHTED){
        if(EDGE_SIZE_PROPORTIONAL_TO_WEIGHT) E(g[[l]])$size <- E(g[[l]])$weight
        if(EDGE_SIZE_PROPORTIONAL_TO_LOGWEIGHT) E(g[[l]])$size <- log(1+E(g[[l]])$weight)
        if(EDGE_SIZE_PROPORTIONAL_TO_LOGLOGWEIGHT) E(g[[l]])$size <- EDGE_DEFAULT_SIZE*log(1+log(1+E(g[[l]])$weight))
    }
        
    #rescale the layout to allow superposition with shift along z-axis
    print("  Normalizing coordinates...")    
    layouts[[l]][,1] <- 2*(layouts[[l]][,1] - XMIN)/(XMAX-XMIN) - 1 + (l-1)*LAYER_SHIFT
    layouts[[l]][,2] <- 2*(layouts[[l]][,2] - YMIN)/(YMAX-YMIN) - 1
    
    if(AGGREGATE_SHOW){
        layouts[[l]][,3] <- -1 + 2*l/(LAYERS+1)
    }else{
        layouts[[l]][,3] <- -1 + 2*l/LAYERS
    }

    if(NODE_COLOR_BY_COMMUNITY){
        print("  Detecting communities for node coloring")    

        if(COMMUNITY_EDGE_BETWEENNESS){
            wt <- edge.betweenness.community(g[[l]],modularity=TRUE)
            wmemb <- community.to.membership(g[[l]], wt$merges,steps=which.max(wt$modularity)-1)
        }
        if(COMMUNITY_RANDOM_WALK_TRAP){
            wt <- walktrap.community(g[[l]],modularity=TRUE)
            wmemb <- community.to.membership(g[[l]], wt$merges,steps=which.max(wt$modularity)-1)
        }
        if(COMMUNITY_INFOMAP){
            wt <- walktrap.community(g[[l]],modularity=TRUE)
            wmemb <- community.to.membership(g[[l]], wt$merges,steps=which.max(wt$modularity)-1)
            
            wt <- infomap.community(g[[l]],modularity=TRUE)
            wmemb$membership <- membership(wt) - 1
            comList <- communities(wt)
            wmemb$csize <- numeric(length(comList))
            for(com in 1:length(wmemb$csize)){
                wmemb$csize[com] <- length(comList[[com]])
            }
        }
        print(paste("  Modularity: ",modularity(wt)))
        maxCom <- max(wmemb$membership) + 1
                            
        if(COMMUNITY_MIN_SIZE>0){
            #Merge community smaller than chosen resolution to a unique community
            #This will improve the coloring scheme when there are many isoloted nodes/components
            mergedNodes <- 0
            for(n in 1:length(wmemb$membership)){
                if( wmemb$csize[ wmemb$membership[n]+1 ] <= COMMUNITY_MIN_SIZE ){
                    wmemb$membership[n]  <- -1
                    mergedNodes <- mergedNodes + 1
                }
            }
    
            print(paste("  There are", mergedNodes, "nodes in communities smaller than",COMMUNITY_MIN_SIZE))
            print("  Merging...")
                
            maxCom <- max(wmemb$membership) + 1
            mergeComID <-  maxCom + 1
            wmemb$membership[wmemb$membership==-1] <- mergeComID
            wmemb$csize[mergeComID] <- mergedNodes
            wmemb$csize <- wmemb$csize[1:mergeComID]
        }
        
        print(paste("  Communities with >",COMMUNITY_MIN_SIZE,"nodes:",maxCom-1))
        
        tmpColor <- rainbow( max(wmemb$membership) + 2, alpha=NODE_TRANSP, start=runif(1) )[ wmemb$membership + 1 ]
        
        #setting a random start should avoid coloring nodes in the same way on different layers
        V(g[[l]])$color <- tmpColor
    }
    
    print("  openGL phase...")
    #plot the graph with openGL    
    rglplot.igraph(g[[l]], layout=layouts[[l]],
                      vertex.size=V(g[[l]])$size, 
                      vertex.color=V(g[[l]])$color,
                      vertex.label=V(g[[l]])$label,
                      vertex.label.dist=0.4 + 0.01*graph.strength(g[[l]]),
                      vertex.label.font=2,
                      vertex.label.cex=2, 
                      vertex.label.color=V(g[[l]])$vertex.label.color,
                      edge.width=E(g[[l]])$size, 
                      edge.color=E(g[[l]])$color, 
                      edge.arrow.size=LAYER_ARROW_SIZE, 
                      edge.arrow.width=LAYER_ARROW_WIDTH, 
                      edge.curved=E(g[[l]])$curve,
                      rescale=F)
                                            
    print(paste("  Layout of layer: finished."))
}

fileNamePNG <- paste(inputList,"_",OSMType,".png",sep="")
if(GEOGRAPHIC_LAYOUT && (GEOGRAPHIC_BOUNDARIES_SHOW || GEOGRAPHIC_BOUNDARIES_AGGREGATE_SHOW)){
    print(paste("  Downloading geographic area..."))
    #create a map with openstreetmap and save to a file for later use
    rescaleFactor <- (YMAX-YMIN)/(XMAX-XMIN)
    #H0/W0 = H/W  --> H = W/W0 * H0 = W*rescaleFactor in terms of Cartesian coords
    pngWidth = 720
    pngHeight = pngWidth*rescaleFactor
    png(filename=fileNamePNG,width=pngWidth,height=pngHeight)
    map = openmap(c(lat=LATMAX,   lon=LONGMIN), c(lat= LATMIN,   lon=LONGMAX), minNumTiles=18,type=OSMType)
    plot(map)
    dev.off()
}


if(LAYER_SHOW){
    for( l in 1:(LAYERS+1)){
        #This draws a plan to be used as layer
        if(AGGREGATE_SHOW){
            d <- -1 + 2*l/(LAYERS+1)
        }else{
            d <- -1 + 2*l/LAYERS
        }

        x <- c(-1,-1,1,1) + (l-1)*LAYER_SHIFT
        y <- c(1,-1,-1,1)
        z <- c(d,d,d,d)
        
        if(l<LAYERS+1){
            #planes3d(0,0,1, -d , alpha=LAYER_TRANSP, col=LAYER_COLOR)
            if(GEOGRAPHIC_LAYOUT && GEOGRAPHIC_BOUNDARIES_SHOW){
                quads3d(x,y,z, alpha=LAYER_TRANSP, col=LAYER_COLOR,texcoords=cbind(c(0,0,1,1), -c(0,1,1,0)), texture=fileNamePNG)
            }else{
                quads3d(x,y,z, alpha=LAYER_TRANSP, col=LAYER_COLOR)
            }
        }else{
            if(AGGREGATE_SHOW){
                #planes3d(0,0,1, -d , alpha=LAYER_AGGREGATE_TRANSP, col=LAYER_AGGREGATE_COLOR)
                if(GEOGRAPHIC_LAYOUT && GEOGRAPHIC_BOUNDARIES_AGGREGATE_SHOW){
                    quads3d(x,y,z, alpha=LAYER_AGGREGATE_TRANSP, col=LAYER_AGGREGATE_COLOR,texcoords=cbind(c(0,0,1,1), -c(0,1,1,0)), texture=fileNamePNG)
                }else{
                    quads3d(x,y,z, alpha=LAYER_AGGREGATE_TRANSP, col=LAYER_AGGREGATE_COLOR)                    
                }
            }else{
                next
            }
        }
        
        if(LAYER_ID_SHOW_BOTTOMLEFT){
            text3d(-1+(l-1)*LAYER_SHIFT, -1, d+0.1,text=layerLabel[[l]][1],adj = 0.2, color="black", family="sans", cex=LAYER_ID_FONTSIZE)
        }
        if(LAYER_ID_SHOW_TOPLEFT){
            text3d(-1+(l-1)*LAYER_SHIFT, 1, d+0.1,text=layerLabel[[l]][1],adj = 0.2, color="black", family="sans", cex=LAYER_ID_FONTSIZE)
        }
        if(LAYER_ID_SHOW_BOTTOMRIGHT){
            text3d(1+(l-1)*LAYER_SHIFT, -1, d+0.1,text=layerLabel[[l]][1],adj = 0.2, color="black", family="sans", cex=LAYER_ID_FONTSIZE)
        }
        if(LAYER_ID_SHOW_TOPRIGHT){
            text3d(1+(l-1)*LAYER_SHIFT, 1, d+0.1,text=layerLabel[[l]][1],adj = 0.2, color="black", family="sans", cex=LAYER_ID_FONTSIZE)
        }
    }

}

if(!LAYOUT_INDEPENDENT){
    if(INTERLINK_SHOW & INTERLINK_SHOW_FRACTION>0){
        print("Adding interlayer links.")
        #to be generalized to allow cross-interlink and absence of interlinks for some nodes
        for( l in 1:(LAYERS-1) ){
            layerLinesX <- matrix(c(0),nrow=Nodes,ncol=2)
            layerLinesY <- matrix(c(0),nrow=Nodes,ncol=2)
            layerLinesZ <- matrix(c(0),nrow=Nodes,ncol=2)

            layerLinesX <- cbind(layouts[[l]][,1] + (l-1)*LAYER_SHIFT,layouts[[l+1]][,1] + l*LAYER_SHIFT)
            layerLinesY <- cbind(layouts[[l]][,2],layouts[[l+1]][,2])
            layerLinesZ <- cbind(layouts[[l]][,3],layouts[[l+1]][,3])

            for(i in 1:Nodes){
                if(runif(1)>1-INTERLINK_SHOW_FRACTION){ 
                    segments3d(
                        layerLinesX[i,],
                        layerLinesY[i,],
                        layerLinesZ[i,],
                        lwd=INTERLINK_WIDTH, 
                        col=INTERLINK_COLOR, 
                        lty=INTERLINK_TYPE,
                        alpha=INTERLINK_TRANSP)
                }
            }
        }
    }
}

M <- matrix(0, ncol=4,nrow=4)
M[1,] <- c(0.54,0,0.84,0)
M[2,] <- c(0.33,0.92,-0.22,0)
M[3,] <- c(-0.77,0.39,0.5,0)
M[4,] <- c(0,0,0,1)

par3d(FOV=PLOT_FOV, userMatrix=M)
bg3d(BACKGROUND_COLOR)
title3d(PLOT_TITLE,PLOT_SUBTITLE,'','','')
rgl.snapshot(FILE_RGL_SNAPSHOT) 
print("Finalizing rendering...")
  
#  dev.off()

if(EXPORT_MOVIE){
    f <- par3dinterp( zoom = c(1,1.5,2.5,3) )
    M <- par3d("userMatrix")
    
    movie3d( par3dinterp( 
                        userMatrix=list(M,
                                                rotate3d(M, pi/2, 1, 0, 0),
                                                rotate3d(M, pi/2, 0, 1, 0),
                                                rotate3d(M, pi/2, 0, 0, 1),
                                                M
                                                 ) ),
                        duration=8, movie=FILE_RGL_MOVIE,dir="." )
}
