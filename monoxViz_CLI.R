#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# MonoxVisual: Visualization of Monoplex Networks with R & openGL
#
# Version: 0.1
# Last update: Jan 2014
# Authors: Manlio De Domenico and Serafina Agnello
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

#Insert here the input network
fileNetwork <- "examples/co4_sample3_N100.edges"
labelNetwork <- "network"
layoutNetwork <- ""

#Network type
DIRECTED <- F
WEIGHTED <- F

#Choose the layout
USE_OPENGL <- T                                             #if F, use the standard R device with iraph
LAYOUT_FRUCHTERMAN_REINGOLD <- F    #for networks with < 1000 nodes
LAYOUT_LGL <- F                                              #for networks with > 1000 nodes
LAYOUT_DRL <- F                                              #for networks with > 1000 nodes
LAYOUT_SPRING <- F                                       #for networks with < 100 nodes
LAYOUT_KAMADA_KAWAI <- F                        #for networks with < 100 nodes
LAYOUT_REINGOLD_TILFORD <- F                #for networks with < 1000 nodes
LAYOUT_COMBINED <- T                                 #for complicated networks...
LAYOUT_MAXITER <- 1000
LAYOUT_DIMENSION <- 3                                #3D or 2D

#Output options
FILE_RGL_SNAPSHOT <- "monoxViz.png"

#Choose the community detection algorithm
COMMUNITY_EDGE_BETWEENNESS <- F
COMMUNITY_RANDOM_WALK_TRAP <- F
COMMUNITY_INFOMAP <- T

#Graphic options
LAYER_SHOW <- F
NODE_LABELS_SHOW <- F
GEOGRAPHIC_BOUNDARIES_SHOW <- T
OSMType <- "stamen-watercolor"               #this fix the type of map to be used in the background
#osm-bbike-german
#bing
#osm
#mapquest-aerial
#stamen-watercolor
#stamen-toner
#mapbox
#osm-transport

RESCALE_WEIGHT <- F

NODE_TRANSP <- 0.2
EDGE_TRANSP <- 0.2

PLOT_TITLE <- ""
PLOT_SUBTITLE <- ""
PLOT_FOV <- 0  #deg, can be changed with mouse
LAYER_COLOR <- "gray"
LAYER_LABEL_PREFIX <- "L"
LAYER_TRANSP <- 0.9
LAYER_ARROW_SIZE <- 0.5
LAYER_ARROW_WIDTH <- 1
LAYER_ID_SHOW_TOPLEFT <- F
LAYER_ID_SHOW_BOTTOMLEFT<- T
LAYER_ID_SHOW_TOPRIGHT <- F
LAYER_ID_SHOW_BOTTOMRIGHT<- F
LAYER_ID_FONTSIZE <- 0.9
NODE_DEFAULT_SIZE <- 2
NODE_SIZE_PROPORTIONAL_TO_LOGPAGERANK <- F
NODE_SIZE_PROPORTIONAL_TO_STRENGTH <- F
NODE_SIZE_PROPORTIONAL_TO_LOGSTRENGTH <- F
NODE_SIZE_PROPORTIONAL_TO_LOGLOGSTRENGTH <- T
NODE_COLOR_BY_COMMUNITY <- T
COMMUNITY_MIN_SIZE <- 1
EDGE_DEFAULT_SIZE <- 1
EDGE_SIZE_PROPORTIONAL_TO_WEIGHT <- F
EDGE_SIZE_PROPORTIONAL_TO_LOGWEIGHT <- T
EDGE_SIZE_PROPORTIONAL_TO_LOGLOGWEIGHT <- F
EDGE_BENDING <- 0.2

INTERLINK_COLOR <- "black"
INTERLINK_TYPE <- "dotted"
INTERLINK_WIDTH <- 1

BACKGROUND_COLOR <- "#FFFFFF"


###############
#End Parameters
###############

fileName <- fileNetwork
layerLabel <- labelNetwork
layerLayoutFile <- layoutNetwork
layerEdges <-  read.table(fileName, header=F)

if(layerLabel=="" || is.na(layerLabel)){
    layerLabel <- paste(LAYER_LABEL_PREFIX, 1)
}

#Find the minimum and maximum node ID in the multiplex
idmin <- 1e100
idmax <- 0
offset = 0;

if( min(layerEdges[,1:2]) < idmin) idmin <- min(layerEdges[,1:2])
if( max(layerEdges[,1:2]) > idmax) idmax <- max(layerEdges[,1:2])

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
ZMAX <- -1e10
XMIN <- 1e10
YMIN <- 1e10
ZMIN <- 1e10
LONGMAX <- -1e10
LATMAX <- -1e10
LONGMIN <- 1e10
LATMIN <- 1e10

#If layout is specified correctly
if(layerLayoutFile!="" && (!is.na(layerLayoutFile))){
    layerTable <- read.table(layerLayoutFile, header=T)
    layerLayout <- matrix(c(1),nrow=Nodes,ncol=2)
    
    if(length(layerTable$nodeLat)==Nodes && length(layerTable$nodeLong)==Nodes){
        print(paste("Layout is geographic. Converting."))
        #Get boundaries
        longBounds = c(min(layerTable$nodeLong),max(layerTable$nodeLong))
        latBounds = c(min(layerTable$nodeLat),max(layerTable$nodeLat))            

        #These lines allow to fix manually the boundaries.. it could be useful in some cases
        #layerTable$nodeLat[layerTable$nodeLat>73] <- 73
        #layerTable$nodeLat[layerTable$nodeLat<30] <- 30

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
        layerLayout[layerTable$nodeID + offset,1:2] <- cbind(layerTable$nodeX,layerTable$nodeY)

        if(min(layerTable$nodeX,na.rm=T) < XMIN) XMIN = min(layerTable$nodeX,na.rm=T)
        if(min(layerTable$nodeY,na.rm=T) < YMIN) YMIN = min(layerTable$nodeY,na.rm=T)
        if(max(layerTable$nodeX,na.rm=T) > XMAX) XMAX = max(layerTable$nodeX,na.rm=T)
        if(max(layerTable$nodeY,na.rm=T) > YMAX) YMAX = max(layerTable$nodeY,na.rm=T)
        
        GEOGRAPHIC_LAYOUT <- GEOGRAPHIC_LAYOUT && T
        print(paste("Layout specified correctly from external file",layerLayoutFile))
    }else{
        print(paste("Layout not specified correctly. Proceeding with automatic layouting."))
        LAYOUT_EXTERNAL <- F
        GEOGRAPHIC_LAYOUT <- F
    }

    if(length(layerTable$nodeLabel)==Nodes){
        print(paste("Nodes' labels specified correctly from external file",layerLayoutFile))            
        #Assign labels to nodes
        nodesLabel[layerTable$nodeID + offset] <- as.character(layerTable$nodeLabel)
        print("Assigned labels.")
    }else{
        print(paste("Nodes' labels not specified correctly. Proceeding with automatic labeling."))
        nodesLabel <- 1:Nodes
    }
}else{
    print(paste("Layout not specified correctly. Proceeding with automatic layouting."))
    LAYOUT_EXTERNAL <- F
    GEOGRAPHIC_LAYOUT <- F

    print(paste("Nodes' labels not specified correctly. Proceeding with automatic labeling."))
    #Assign labels to nodes
    nodesLabel <- 1:Nodes
}

#create an undirected edges list (both directions specified) if requested
if(!DIRECTED){
    if(ncol(layerEdges)==3){
        #swap columns
        tmpedges <- layerEdges[,c(2,1,3)]
        #change the name of the columns, or merge will not work
        colnames(tmpedges) <- c("V1","V2","V3")
        #merge
        layerEdges<-merge(layerEdges,tmpedges,all=T)
    }else{
        #swap columns
        tmpedges <- layerEdges[,c(2,1)]
        #change the name of the columns, or merge will not work
        colnames(tmpedges) <- c("V1","V2")
        #merge
        layerEdges<-merge(layerEdges,tmpedges,all=T)
    }
}

if(offset>0){
    layerEdges[,1] <- layerEdges[,1] + offset
    layerEdges[,2] <- layerEdges[,2] + offset
}

if(WEIGHTED){
    if(RESCALE_WEIGHT){
        if(ncol(layerEdges)==3){
            print("Rescaling weights...")
            layerEdges[,3] <- layerEdges[,3]/min(layerEdges[,3])
        }
    }
}

#We need the (weighted) adjacency matrix to build the aggregate network    
A_layer <- matrix(0,ncol=Nodes,nrow=Nodes)

#For weighted network, and for our requirements this should be done
if(ncol(layerEdges)==3){
    for(i in 1:nrow(layerEdges)) A_layer[ layerEdges[i,1], layerEdges[i,2] ] <- layerEdges[i,3]
}else{
    for(i in 1:nrow(layerEdges)) A_layer[ layerEdges[i,1], layerEdges[i,2] ] <- 1
}
    
if(WEIGHTED){
    g <- graph.adjacency(A_layer,weighted=WEIGHTED)
}else{
    g <- graph.adjacency(A_layer,weighted=NULL)   
}

print(paste("Layer: ",fileName,"   Name:",layerLabel))
print(paste("Layer Directed: ",DIRECTED))
print(paste("Layer Weighted: ",WEIGHTED))
print(paste(nrow(layerEdges),"Edges"))

layouts <- matrix(c(1),ncol=3,nrow=Nodes)

#Check if the layouts are specified by external files, otherwise proceed with the automatic ones
if(!LAYOUT_EXTERNAL){
    print("Independent layout option.")
        
    #Each layout is calculated separately    
    if(LAYOUT_FRUCHTERMAN_REINGOLD){
        layouts <- layout.fruchterman.reingold.grid(g,weights=E(g)$weight,niter=LAYOUT_MAXITER,area=vcount(g)^1.,repulserad=vcount(g)^1.3,dim=LAYOUT_DIMENSION)
    }
    if(LAYOUT_LGL){
        layouts <- layout.lgl(g,maxiter=LAYOUT_MAXITER)
    }
    if(LAYOUT_DRL){
        layouts <- layout.drl(g,options=list(simmer.attraction=0,simmer.iterations=floor(LAYOUT_MAXITER*0.15),crunch.iterations=floor(LAYOUT_MAXITER*0.1),cooldown.iterations=floor(LAYOUT_MAXITER*0.25),expansion.iterations=floor(LAYOUT_MAXITER*0.25),liquid.iterations=floor(LAYOUT_MAXITER*0.25)),dim=LAYOUT_DIMENSION)
    }
    
    if(LAYOUT_REINGOLD_TILFORD){
        layouts <- layout.reingold.tilford(g)
    }
    
    if(LAYOUT_KAMADA_KAWAI){
        layouts <- layout.kamada.kawai(g, niter=LAYOUT_MAXITER,dim=LAYOUT_DIMENSION)
    }
    if(LAYOUT_SPRING){
        layouts <- layout.spring(g,repulse=T)
    }
    
    if(LAYOUT_COMBINED){
        #We try to use the DRL to scale and we use it as seed for a Kamada-Kawai with few iterations
        ltmp <- layout.drl(g,options=list(simmer.attraction=0,simmer.iterations=floor(LAYOUT_MAXITER*0.15),crunch.iterations=floor(LAYOUT_MAXITER*0.1),cooldown.iterations=floor(LAYOUT_MAXITER*0.25),expansion.iterations=floor(LAYOUT_MAXITER*0.25),liquid.iterations=floor(LAYOUT_MAXITER*0.25)),dim=LAYOUT_DIMENSION)
        
        layouts <- layout.kamada.kawai(g, niter=LAYOUT_MAXITER,start=ltmp,dim=LAYOUT_DIMENSION)
    }
}else{
    print("Layouting: external files.")
    layouts <- matrix(c(1),nrow=Nodes,ncol=2)
    layouts <- layerLayout
}

if(LAYOUT_DIMENSION==2){
    #Make it a 3-columns object
    layouts <- cbind(layouts[,1:2],1)
}

if(LAYOUT_EXTERNAL && GEOGRAPHIC_LAYOUT){
    #Make it a 3-columns object
    layouts <- cbind(layouts[,1:2],1)
}

if(!LAYOUT_EXTERNAL && !GEOGRAPHIC_LAYOUT){
    if(min(layouts[,1],na.rm=T) < XMIN) XMIN = min(layouts[,1],na.rm=T)
    if(min(layouts[,2],na.rm=T) < YMIN) YMIN = min(layouts[,2],na.rm=T)
    if(min(layouts[,3],na.rm=T) < ZMIN) ZMIN = min(layouts[,3],na.rm=T)
    if(max(layouts[,1],na.rm=T) > XMAX) XMAX = max(layouts[,1],na.rm=T)
    if(max(layouts[,2],na.rm=T) > YMAX) YMAX = max(layouts[,2],na.rm=T)
    if(max(layouts[,3],na.rm=T) > ZMAX) ZMAX = max(layouts[,3],na.rm=T)
}

print("Layouting finished.")

if(USE_OPENGL){
    rgl.clear()
}

V(g)$vertex.label.color <- rgb(47,47,47,0,maxColorValue = 255)

#this set the transparency level of edges and nodes.. it can be customized
E(g)$alpha <- floor(EDGE_TRANSP*255)
V(g)$alpha <- floor(NODE_TRANSP*255)

#generate a random color for this layer
Rcolor <- sample(0:255, 1, replace=T)
Gcolor <- sample(0:255, 1, replace=T)
Bcolor <- sample(0:255, 1, replace=T)

#assign the color to the layer
E(g)$red <- Rcolor
E(g)$green <- Gcolor
E(g)$blue <- Bcolor
V(g)$red <- Rcolor
V(g)$green <- Gcolor
V(g)$blue <- Bcolor

E(g)$color<-rgb(E(g)$red, E(g)$green, E(g)$blue, E(g)$alpha, maxColorValue=255)
V(g)$color <- rgb(V(g)$red, V(g)$green, V(g)$blue, V(g)$alpha, maxColorValue=255)

#other assignments
E(g)$curve<- EDGE_BENDING

if(!NODE_LABELS_SHOW){
    V(g)$label <- ""
}else{
    V(g)$label <- nodesLabel
}

V(g)$size <- NODE_DEFAULT_SIZE
if(NODE_SIZE_PROPORTIONAL_TO_STRENGTH) V(g)$size = graph.strength(g);
if(NODE_SIZE_PROPORTIONAL_TO_LOGSTRENGTH) V(g)$size = 1+2*log(1+graph.strength(g));
if(NODE_SIZE_PROPORTIONAL_TO_LOGLOGSTRENGTH) V(g)$size = NODE_DEFAULT_SIZE*log(1+log(1+graph.strength(g)));
if(NODE_SIZE_PROPORTIONAL_TO_LOGPAGERANK) V(g)$size = 1+1.2*log(1+Nodes*page.rank.old(g));
    
E(g)$size <- EDGE_DEFAULT_SIZE;
if(WEIGHTED){
    if(EDGE_SIZE_PROPORTIONAL_TO_WEIGHT) E(g)$size <- E(g)$weight
    if(EDGE_SIZE_PROPORTIONAL_TO_LOGWEIGHT) E(g)$size <- log(1+E(g)$weight)
    if(EDGE_SIZE_PROPORTIONAL_TO_LOGLOGWEIGHT) E(g)$size <- EDGE_DEFAULT_SIZE*log(1+log(1+E(g)$weight))
}
    
#rescale the layout to allow superposition with shift along z-axis
print("  Normalizing coordinates...")    
layouts[,1] <- 2*(layouts[,1] - XMIN)/(XMAX-XMIN) - 1
layouts[,2] <- 2*(layouts[,2] - YMIN)/(YMAX-YMIN) - 1
if(LAYOUT_DIMENSION==3 && !GEOGRAPHIC_LAYOUT){
    layouts[,3] <- 2*(layouts[,3] - ZMIN)/(ZMAX-ZMIN) - 1 
}else{
    layouts[,3] <- 1
}

if(NODE_COLOR_BY_COMMUNITY){
    print("  Detecting communities for node coloring")    

    if(COMMUNITY_EDGE_BETWEENNESS){
        wt <- edge.betweenness.community(g,modularity=TRUE)
        wmemb <- community.to.membership(g, wt$merges,steps=which.max(wt$modularity)-1)
    }
    if(COMMUNITY_RANDOM_WALK_TRAP){
        wt <- walktrap.community(g,modularity=TRUE)
        wmemb <- community.to.membership(g, wt$merges,steps=which.max(wt$modularity)-1)
    }
    if(COMMUNITY_INFOMAP){
        wt <- walktrap.community(g,modularity=TRUE)
        wmemb <- community.to.membership(g, wt$merges,steps=which.max(wt$modularity)-1)
        
        wt <- infomap.community(g,modularity=TRUE)
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
    V(g)$color <- tmpColor
}

if(USE_OPENGL){    
    print("  openGL phase...")
    #plot the graph with openGL    
    rglplot.igraph(g, layout=layouts,
                        vertex.size=V(g)$size, 
                        vertex.color=V(g)$color,
                        vertex.label=V(g)$label,
                        vertex.label.dist=0.4 + 0.01*graph.strength(g),
                        vertex.label.font=2,
                        vertex.label.cex=2, 
                        vertex.label.color=V(g)$vertex.label.color,
                        edge.width=E(g)$size, 
                        edge.color=E(g)$color, 
                        edge.arrow.size=LAYER_ARROW_SIZE, 
                        edge.arrow.width=LAYER_ARROW_WIDTH, 
                        edge.curved=E(g)$curve,
                        rescale=F)
                                            
    print(paste("  Layout of layer: finished."))
    
    fileNamePNG <- paste(fileNetwork,"_",OSMType,".png",sep="")
    if(GEOGRAPHIC_LAYOUT && GEOGRAPHIC_BOUNDARIES_SHOW){
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
        x <- c(-1,-1,1,1)
        y <- c(1,-1,-1,1)
        z <- c(1,1,1,1)
        
        #planes3d(0,0,1, -d , alpha=LAYER_TRANSP, col=LAYER_COLOR)
        if(GEOGRAPHIC_LAYOUT && GEOGRAPHIC_BOUNDARIES_SHOW){
            quads3d(x,y,z, alpha=LAYER_TRANSP, col=LAYER_COLOR,texcoords=cbind(c(0,0,1,1), -c(0,1,1,0)), texture=fileNamePNG)
        }else{
            quads3d(x,y,z, alpha=LAYER_TRANSP, col=LAYER_COLOR)
        }
        
        if(LAYER_ID_SHOW_BOTTOMLEFT){
            text3d(-1, -1, d+0.1,text=layerLabel,adj = 0.2, color="black", family="sans", cex=LAYER_ID_FONTSIZE)
        }
        if(LAYER_ID_SHOW_TOPLEFT){
            text3d(-1, 1, d+0.1,text=layerLabel,adj = 0.2, color="black", family="sans", cex=LAYER_ID_FONTSIZE)
        }
        if(LAYER_ID_SHOW_BOTTOMRIGHT){
            text3d(1, -1, d+0.1,text=layerLabel,adj = 0.2, color="black", family="sans", cex=LAYER_ID_FONTSIZE)
        }
        if(LAYER_ID_SHOW_TOPRIGHT){
            text3d(1, 1, d+0.1,text=layerLabel,adj = 0.2, color="black", family="sans", cex=LAYER_ID_FONTSIZE)
        }
    }
        
    par3d(FOV=PLOT_FOV)
    bg3d(BACKGROUND_COLOR)
    title3d(PLOT_TITLE,PLOT_SUBTITLE,'','','')
    rgl.snapshot(FILE_RGL_SNAPSHOT) 
    print("Finalizing rendering...")
      
    #  dev.off()
}else{
    plot.igraph(g, layout=layouts,
                        vertex.size=V(g)$size, 
                        vertex.color=V(g)$color,
                        vertex.label=V(g)$label,
                        vertex.label.dist=0.4 + 0.01*graph.strength(g),
                        vertex.label.font=2,
                        vertex.label.cex=2, 
                        vertex.label.color=V(g)$vertex.label.color,
                        edge.width=E(g)$size, 
                        edge.color=E(g)$color, 
                        edge.arrow.size=LAYER_ARROW_SIZE, 
                        edge.arrow.width=LAYER_ARROW_WIDTH, 
                        edge.curved=E(g)$curve,
                        rescale=F)
                                            
    print(paste("  Layout of layer: finished."))
}