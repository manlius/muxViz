#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# MuxVisual: Visualization of Multiplex Networks with R & openGL
#
# Version: 0.1
# Last update: Nov 2013
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

###############
#Input Parameters
###############

LAYERS <- 4

edges <- vector("list",LAYERS)

#Insert here the file list with input networks
fileName <- readLines("layers_list.txt")

for(l in 1:LAYERS){
    edges[[l]] <-  read.table(fileName[l], header=F)
}

#Network type
DIRECTED <- F
WEIGHTED <- F

#Choose the layout
LAYOUT_FRUCHTERMAN_REINGOLD <- T    #for networks with < 1000 nodes
LAYOUT_LGL <- F                                              #for networks with > 1000 nodes
LAYOUT_DRL <- F                                              #for networks with > 1000 nodes
LAYOUT_SPRING <- F                                       #for networks with < 100 nodes
LAYOUT_KAMADA_KAWAI <- F                        #for networks with < 100 nodes
LAYOUT_REINGOLD_TILFORD <- F                #for networks with < 1000 nodes
LAYOUT_COMBINED <- F                                 #for complicated networks...
LAYOUT_MAXITER <- 1000
LAYOUT_BY_FIRST_LAYER <- F                      #otherwise default use the aggregated
LAYOUT_INDEPENDENT <- F                           #if each layer can be layouted separately

#Output options
FILE_RGL_SNAPSHOT <- "muxViz.png"
FILE_RGL_MOVIE <- "muxViz_movie.png"
EXPORT_MOVIE <- F

#Graphic options
LAYER_SHOW <- T
NODE_LABELS_SHOW <- F
INTERLINK_SHOW <- F
INTERLINK_SHOW_FRACTION <- 0.01 #this allows to show only a few (randomly chosen) interlinks
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
LAYER_TRANSP <- 0.1
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
COMMUNITY_MIN_SIZE <- 0     
EDGE_DEFAULT_SIZE <- 2
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


#Find the minimum and maximum node ID in the multiplex
idmin <- 1e100
idmax <- 0
offset = 0;

for(l in 1:LAYERS){
    if( min(edges[[l]]) < idmin) idmin <- min(edges[[l]])
    if( max(edges[[l]]) > idmax) idmax <- max(edges[[l]])
}

if(idmin == 0) offset = 1;
Nodes = idmax + offset

print(paste("Minimum node ID: ",idmin))
print(paste("Maximum node ID: ",idmax-offset))
print(paste("Using offset: ",offset))

#Create the graph objects
g <- vector("list",LAYERS)

Adj_aggr <- matrix(0,ncol=Nodes,nrow=Nodes)

for(l in 1:LAYERS){    
    #create an undirected edges list (both directions specified) if requested
    if(!DIRECTED){
        if(ncol(edges[[l]])==3){
            #swap columns
            tmpedges <- edges[[l]][,c(2,1,3)]
            #change the name of the columns, or merge will not work
            colnames(tmpedges) <- c("V1","V2","V3")
            #merge
            edges[[l]]<-merge(edges[[l]],tmpedges,all=T)
        }else{
            #swap columns
            tmpedges <- edges[[l]][,c(2,1)]
            #change the name of the columns, or merge will not work
            colnames(tmpedges) <- c("V1","V2")
            #merge
            edges[[l]]<-merge(edges[[l]],tmpedges,all=T)
        }
    }
    
    if(offset>0){
        edges[[l]][,1] <- edges[[l]][,1] + offset
        edges[[l]][,2] <- edges[[l]][,2] + offset
    }

    if(WEIGHTED){
        if(RESCALE_WEIGHT){
            if(ncol(edges[[l]])==3){
                print("Rescaling weights...")
                edges[[l]][,3] <- edges[[l]][,3]/min(edges[[l]][,3])
            }
        }
    }

    #We need the (weighted) adjacency matrix to build the aggregate network    
    A_layer <- matrix(0,ncol=Nodes,nrow=Nodes)

    #For weighted network, and for our requirements this should be done
    if(ncol(edges[[l]])==3){
        for(i in 1:nrow(edges[[l]])) A_layer[ edges[[l]][i,1], edges[[l]][i,2] ] <- edges[[l]][i,3]
    }else{
        for(i in 1:nrow(edges[[l]])) A_layer[ edges[[l]][i,1], edges[[l]][i,2] ] <- 1
    }
        
    if(WEIGHTED){
        g[[l]] <- graph.adjacency(A_layer,weighted=WEIGHTED)
    }else{
        g[[l]] <- graph.adjacency(A_layer,weighted=NULL)   
    }

    if(l==1){
        A_layer1 <- A_layer
    }

    Adj_aggr <- Adj_aggr + A_layer
    
    print(paste("Layer ",l," D: ",DIRECTED))
    print(paste("Layer ",l," W: ",WEIGHTED))
    print(paste(nrow(edges[[l]]),"Edges in layer: ",l))
}


layouts <- vector("list",LAYERS)

if(!LAYOUT_INDEPENDENT){    
    print("Constrained layout option.")
    if(LAYOUT_BY_FIRST_LAYER){
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
    
    #For compatibility with the other option, we assign lAggr to each layout
    for(l in 1:LAYERS){
        layouts[[l]] <- lAggr
    }
}else{
    print("Independent layout option.")
    for(l in 1:LAYERS){
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


print("Layouting finished. Proceeding with openGL plot of each layer.")

rgl.clear()

 for(l in 1:LAYERS){
    print(paste("Layer: ",l))
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
        V(g[[l]])$label <- V(g[[l]])
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
    #Make the 3-columns object
    layouts[[l]] <- cbind(layouts[[l]],1)
    
    layouts[[l]][,1] <- 2*(layouts[[l]][,1] - min(layouts[[l]][,1]))/(max(layouts[[l]][,1]) - min(layouts[[l]][,1])) - 1
    layouts[[l]][,2] <- 2*(layouts[[l]][,2] - min(layouts[[l]][,2]))/(max(layouts[[l]][,2]) - min(layouts[[l]][,2])) - 1
    layouts[[l]][,3] <- -1 + 2*l/LAYERS

    if(NODE_COLOR_BY_COMMUNITY){
        print("  Detecting communities for node coloring")    
        wt <- walktrap.community(g[[l]],modularity=TRUE)
        print(paste("  Modularity: ",modularity(wt)))
        wmemb <- community.to.membership(g[[l]], wt$merges,steps=which.max(wt$modularity)-1)
        
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
                      
    print(paste("  Layout of layer ",l,"finished."))
}

if(LAYER_SHOW){
    for( l in 1:LAYERS ){
        #This draws a plan to be used as layer
        d <- -1 + 2*l/LAYERS
        planes3d(0,0,1, -d , alpha=LAYER_TRANSP, col=LAYER_COLOR)

        if(LAYER_ID_SHOW_BOTTOMLEFT){
            text3d(-1, -1, d+0.1,text=paste(LAYER_LABEL_PREFIX, l),adj = 0.2, color="black", family="sans", cex=LAYER_ID_FONTSIZE)
        }
        if(LAYER_ID_SHOW_TOPLEFT){
            text3d(-1, 1, d+0.1,text=paste(LAYER_LABEL_PREFIX, l),adj = 0.2, color="black", family="sans", cex=LAYER_ID_FONTSIZE)
        }
        if(LAYER_ID_SHOW_BOTTOMRIGHT){
            text3d(1, -1, d+0.1,text=paste(LAYER_LABEL_PREFIX, l),adj = 0.2, color="black", family="sans", cex=LAYER_ID_FONTSIZE)
        }
        if(LAYER_ID_SHOW_TOPRIGHT){
            text3d(1, 1, d+0.1,text=paste(LAYER_LABEL_PREFIX, l),adj = 0.2, color="black", family="sans", cex=LAYER_ID_FONTSIZE)
        }
    }
}

if(!LAYOUT_INDEPENDENT){
    if(INTERLINK_SHOW & INTERLINK_SHOW_FRACTION>0){
        print("Adding interlayer links.")
        #to be generalized to allow cross-interlink and absence of interlinks for some nodes
        for( l in 1:(LAYERS-1) ){
            for( i in 1:Nodes ){
               if(runif(1)>1-INTERLINK_SHOW_FRACTION){ 
                    segments3d(
                    c(layouts[[l]][i,1],layouts[[l+1]][i,1]),
                    c(layouts[[l]][i,2],layouts[[l+1]][i,2]),
                    c(layouts[[l]][i,3],layouts[[l+1]][i,3]),
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
