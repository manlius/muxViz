library(rgl)
library(RColorBrewer)
library(colorspace)
library(igraph)
library(rgl)
library(mapproj)
library(maps)
library(OpenStreetMap)
library(shiny)
library(ShinyDash)
#library(shinydashboard)
#library(shinyIncubator)
library(digest)
library(googleVis)
library(lattice)
library(fields)
library(clue)
library(gplots)
library(rCharts)
library(ggplot2)
library(d3Network)


##################################################
# Global variables
##################################################

#This is to avoid pushing a button and starting all the other ones..
btnCalculateMotifsValue <- 0
btnCalculateCorrelationDiagnosticsValue <- 0
btnCalculateCentralityDiagnosticsValue <- 0
btnCentralityDiagnosticsAnalysisValue <- 0
btnCalculateCommunityDiagnosticsValue <- 0
btnImportNetworksValue <- 0
btnImportTimelineValue <- 0
btnRenderDynamicsSnapshotsValue <- 0
#btnFFMPEGDynamicsSnapshotsValue <- 0
btnApplyLayoutValue <- 0
btnRenderNetworksValue <- 0
btnExportRenderingValue <- 0
btnExportRenderingSVGValue <- 0
btnExportRenderingPDFValue <- 0
btnExportRenderingWebValue <- 0
btnExportRenderingClassicPNGValue <- 0
btnExportRenderingClassicPDFValue <- 0
btnAnularVizValue <- 0
btnCalculateReducibilityValue <- 0
btnSaveSessionValue <- 0
btnResetLightsValue <- 0
btnRenderNetworkOfLayersValue <- 0

#Other variables
fileInput <- NULL
LAYERS <- 0
multilayerEdges <- NULL
layerEdges <- vector("list",LAYERS+1)
fileName <- vector("list",LAYERS)
layerLabel <- vector("list",LAYERS+1)
layerColor <- vector("list",LAYERS)
layerColorAlpha <- vector("list",LAYERS)
layerLayoutFile <- vector("list",LAYERS)
layerLayout <- vector("list",LAYERS+1)
nodesLabel <- vector("list",LAYERS+1)
layout.non <- NULL
layerTable <- NULL
g <- vector("list",LAYERS+1)
g.multi <- NULL
layout.multi <- NULL
AdjMatrix.multi <- NULL
layouts <- vector("list",LAYERS+1)
AdjMatrix <- vector("list",LAYERS+1)

listDiagnosticsSingleLayer <- data.frame()
listDiagnostics <- data.frame()
listCommunities <- data.frame()
listDiagnosticsMerge <- data.frame()
sumCommunitiesMerge <- data.frame()
listCommunitiesMerge <- data.frame()
listInterPearson <- data.frame()
listInterSpearman <- data.frame()
listOverlap <- data.frame()
listMotifs <- data.frame()

#the timeline for visualization of dynamical processes
dfTimeline <- data.frame()

#default properties (color and size) of the network
defaultVsize <- vector("list",LAYERS+1)
defaultEsize <- vector("list",LAYERS+1)
defaultVcolor <- vector("list",LAYERS+1)
defaultEcolor <- vector("list",LAYERS+1)

#other global vars
LAYOUT_BY_LAYER_ID <- 0
LAYOUT_EXTERNAL <- F
GEOGRAPHIC_LAYOUT <- F
LAYOUT_INDEPENDENT <- F
LAYOUT_DIMENSION <- 2
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
commonRunif <- 1

minNodeID <- -1
maxNodeID <- -1
offsetNode <- -1
Nodes <- 0
Edges <- 0

orientationRGL <- NULL

#==== Network type
DIRECTED <- F
WEIGHTED <- F

diagnosticsMultiplexOK <- F
diagnosticsSingleLayerOK <- F
diagnosticsOK <- F
communityOK <- F


welcomeFunction <- function(){
    muxVizVersion <- "1.0.2"
    muxVizUpdate <- "4 Oct 2015"

    cat("\n")    
    cat(":::::::::::::::::::::::::::::::::::::::::::::::::::::::::\n")
    cat("::: Welcome to muxViz\n")
    #cat("==========================\n")
    cat(":::::::::::::::::::::::::::::::::::::::::::::::::::::::::\n")
    cat("\n")
    cat(":: muxViz: Tool for Multilayer Analysis and Visualization\n")
    cat(":: Copyright (C) 2013-2015 Manlio De Domenico\n")
    cat(":: School of Computer Science and Mathematics\n")
    cat(":: Universitat Rovira i Virgili (Tarragona, Spain)\n")
    cat("\n")
    cat(":: This software is released under GNU GPL v3:\n")
    cat(":: http://www.gnu.org/copyleft/gpl.html\n")
    cat("\n")
    cat(paste(":: Version:",muxVizVersion,"\n"))
    cat(paste(":: Last update:",muxVizUpdate,"\n"))
    cat("\n")
    cat(paste(":: You are running from",Sys.info()["sysname"],"\n"))
    cat(paste("::",Sys.info()["version"],"\n"))
    cat(paste("::",version["version.string"][[1]],"\n"))
    cat("\n")
}

buildPath <- function(folder,objname){
    if( Sys.info()["sysname"]=="Windows" ){
        return( paste(getwd(),folder,objname,sep="\\") )
    }else{
        return( paste(getwd(),folder,objname,sep="/") )        
    }
}

concatenatePath <- function(folder,objname){
    if( Sys.info()["sysname"]=="Windows" ){
        return( paste(folder,objname,sep="\\") )
    }else{
        return( paste(folder,objname,sep="/") )        
    }
}

getExecutablePath <- function(exec_name){
    path <- ""
    if( Sys.info()["sysname"]=="Windows" ){
        path <- buildPath("bin",paste0(exec_name,"_windows"))    
    }else if( Sys.info()["sysname"]=="Linux" ){
        path <- buildPath("bin",paste0(exec_name,"_linux"))
    }else{
        path <- buildPath("bin",paste0(exec_name,"_macosx"))
    }
    
    return(path)
}

text3dwrap <- function( coordsMatrix3D, labels, wraplength, wrapoffset, layerscale, adj, color, family, cex){
    #insert a "\n" in very long lines, for later processing
    labels <- unlist(lapply(labels, function(x) paste(strwrap(x,wraplength), collapse="\n")))

    dy <- layerscale/(8*sqrt(Nodes)) + wrapoffset
    
    #look for lines where the char "\n" is present
    res <- grep("\n",labels)
    if(length(res)>0){
        #if any..
        newCoordsMatrix3D <- coordsMatrix3D[-res,]
        newLabels <- labels[-res]
       
        for(idx in res){
            labels.tmp <- strsplit(labels[idx], '\n')[[1]]
            
            for(n in 1:length(labels.tmp)){
                
                x <- coordsMatrix3D[idx,1]
                y <- coordsMatrix3D[idx,2] - dy*(n-1)
                z <- coordsMatrix3D[idx,3]

                newCoordsMatrix3D <- rbind( newCoordsMatrix3D, t(as.matrix(c(x,y,z))) )
                newLabels <- c( newLabels, labels.tmp[n] )
            }
        }
        
        text3d(newCoordsMatrix3D, text=newLabels, adj=adj, color=color, family=family, cex=cex)
        print( cbind(newCoordsMatrix3D, newLabels) )
    }else{
        #nothing to do, just plot
        text3d(coordsMatrix3D, text=labels, adj=adj, color=color, family=family, cex=cex)
        print( cbind(coordsMatrix3D, labels) )
    }
}

addalpha <- function(colors, alpha=1.0) {
  r <- col2rgb(colors, alpha=T)
  # Apply alpha
  r[4,] <- alpha*255
  r <- r/255.0
  return(rgb(r[1,], r[2,], r[3,], r[4,]))
}


#http://inside.mines.edu/fs_home/gmurray/ArbitraryAxisRotation/
Rotx <- function(th){ 
    th<-th*pi/360.
    Rx<-matrix(nrow=3,ncol=3,0)
    Rx[2,2] <- cos(th)
    Rx[2,3] <- -sin(th)
    Rx[3,2] <- -Rx[2,3]
    Rx[3,3] <- Rx[1,1]
    Rx[1,1] <- 1
    return(Rx) 
}

Roty <- function(th){ 
    th<-th*pi/360.
    Ry<-matrix(nrow=3,ncol=3,0)
    Ry[1,1] <- cos(th)
    Ry[1,3] <- sin(th)
    Ry[3,1] <- -Ry[1,3]
    Ry[3,3] <- Ry[1,1]
    Ry[2,2] <- 1
    return(Ry) 
}

Rotz <- function(th){ 
    th<-th*pi/360.
    Rz<-matrix(nrow=3,ncol=3,0)
    Rz[1,1] <- cos(th)
    Rz[1,2] <- -sin(th)
    Rz[2,1] <- -Rz[1,2]
    Rz[2,2] <- Rz[1,1]
    Rz[3,3] <- 1
    return(Rz) 
}

#to open/save a session.. still experimental
#https://github.com/jcheng5/shiny-resume
#print(objects(all.names = TRUE))
#http://www.inside-r.org/packages/cran/session/docs
    
#to make a CRAN package in the next future
#http://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/

shinyServer(function(input, output, session) {
    welcomeFunction()
   
    commonRunif <<- runif(1)
    #commonRunif <- 0.0148374617565423
    #print(paste("SEED:",commonRunif))

    result <- tryCatch({

        output$communityParameters <- renderUI({
            #control the parameters to show while choosing a community detection method
            if(input$radCommunityAlgorithm == "COMMUNITY_MULTIPLEX" && input$radMultiplexModel=='MULTIPLEX_IS_EDGECOLORED'){
                textInput("txtGamma", label=HTML("Resolution parameter (for multislice community detection):"), "1")
            }
        })
            
        #Fill the table summarizing the config file
        output$dataTable <- renderDataTable({
            inFile <- "mux_dataset.csv"
            
            if (is.null(inFile))
                return(NULL)
    
            #return(gvisTable(read.csv(inFile, header=T, sep=";"),options=list(page='enable',pageSize=100)))
            return(read.csv(inFile, header=T, sep=";"))
        })

        output$dataPieChart <- renderChart2({
            inFile <- "mux_dataset.csv"
            
            if (is.null(inFile))
                return(NULL)

            data <- read.csv(inFile, header=T, sep=";")
            distrib <- table(data$Type)
            dfChart <- data.frame(Type=names(distrib), N=as.numeric(distrib))
            dfChart$perc <- round(100*dfChart$N/sum(dfChart$N),2)
            doughnut <- nPlot(N~Type, data = dfChart, type = "pieChart")
            doughnut$chart(tooltipContent = "#! function(key, y, e, graph){return '<h4>' + 'Category: ' + key + '</h4>' + '<p>'+ 'Networks: ' + e.point.N + ' (' + e.point.perc + '%)'} !#")
            doughnut$set(width = 600, height = 400) # mk changed width to 800 and height to 500

            doughnut$chart(donut = TRUE)

            return(doughnut)
        })


#        output$dataPieChart <- renderGvis({
#            inFile <- "mux_dataset.csv"
#            
#            if (is.null(inFile))
#                return(NULL)
#
#            data <- read.csv(inFile, header=T, sep=";")
#            distrib <- table(data$Type)
#            dfChart <- data.frame(Type=names(distrib), N=as.numeric(distrib))
#
#            doughnut <- gvisPieChart(dfChart, 
#                      options=list(
#                        width=600,
#                        height=300,
#                        colors="['black','orange', '#2b8cbe', 
#                        'red', '#756bb1', '#31a354', 'gray']",
#                        pieSliceText='percentage',
#                        title='Multiplex Data Types',
#                        pieHole=0.3),
#                      chartid="doughnut")
#            return(doughnut)
#        })

        output$dataScatterPlot <- renderChart2({
            inFile <- "mux_dataset.csv"
            
            if (is.null(inFile))
                return(NULL)

            data <- read.csv(inFile, header=T, sep=";")
            data$logEdges <- log10(data$Edges)
            data$logNodes <- log10(data$Nodes)
            data$logLayers <- log10(data$Layers)
            
            rplot <- nPlot(logEdges~logNodes, data=data, 
                                group="Type", type="scatterChart", opacity=list(const=0.7), height=400,width=600)
            rplot$yAxis(axisLabel="log10 #Edges")
            rplot$xAxis(axisLabel="log10 #Nodes")
            rplot$chart(size = '#! function(d){return d.logLayers} !#')
            rplot$chart(tooltipContent = "#! function(key, x, y, e){ return  '<h3>Network:</h3> ' + e.point.Name + '<br><b>Category:</b> ' + key + '<br><b>Ref:</b> ' + e.point.Reference } !#")
            rplot$chart(forceY=c(floor(min(data$logEdges)),floor(max(data$logEdges))+1), 
                                forceX=c(floor(min(data$logNodes)),floor(max(data$logNodes))+1))
            return(rplot)
        })
        
      	################################################
      	# Network of layers
      	################################################

        observe({
            input$btnRenderNetworkOfLayers
            #print(multilayerEdges)
            if(nrow(multilayerEdges)==0 || LAYERS<=0) return()
                
            #if(btnRenderNetworkOfLayersValue==input$btnRenderNetworkOfLayers) return()

            isolate({
                progress <- shiny::Progress$new(session)
                on.exit(progress$close())
                progress$set(message = 'Building the network...', value = 0.2)
                Sys.sleep(1)
    
    
                #this is not the optimal approach.. but for the networks handled by muxViz shoudl be enough
                #alternatively: use igraph to create the weighted network, then convert again to data.frame
                #see http://lists.nongnu.org/archive/html/igraph-help/2013-02/msg00079.html
                dfNoN <- data.frame()
                
                sub.multi <- multilayerEdges #multilayerEdges[ multilayerEdges[,2]!=multilayerEdges[,4], ]
                sub.multi$V1 <- NULL
                sub.multi$V3 <- NULL
                #print(sub.multi)
                
                colnames(sub.multi) <- c("from", "to", "weight")
                g.non <- graph.data.frame(sub.multi, directed=DIRECTED)
                g.non <- simplify(g.non, edge.attr.comb="sum")
                
                dfNoN <- as.data.frame( cbind( get.edgelist(g.non) , E(g.non)$weight) )
                colnames(dfNoN) <- c("from", "to", "weight")
                dfNoN$from <- as.numeric(dfNoN$from) - 1
                dfNoN$to <- as.numeric(dfNoN$to) - 1
                dfNoN$weight <- log(1+as.numeric(dfNoN$weight))
    
                if(nrow(dfNoN)>0){
                    comm.non <- as.numeric(membership(multilevel.community(as.undirected(g.non))))
                    dfNodes <- data.frame(ID=1:LAYERS, name=unlist(layerLabel)[1:LAYERS], group=comm.non)
                    #print(dfNodes)
                    output$networkOfLayersPlot <- renderPrint({
                        return(d3ForceNetwork(Nodes = dfNodes,
                                        Links = dfNoN,
                                        Source = "from", Target = "to",
                                        Value = "weight", NodeID = "name",
                                        Group = "group", width = 600, height = 600,
                                        opacity = 0.8, standAlone = FALSE, zoom=TRUE,
                                        parentElement = '#networkOfLayersPlot'))
                    })
                }else{
                    progress$set(message = 'No network of layers from the data...', value = 0.5)
                    Sys.sleep(1)
                }
    
                btnRenderNetworkOfLayersValue <<- input$btnRenderNetworkOfLayers
    
                progress$set(detail = 'Done!', value = 1)
                Sys.sleep(2)
            })
        })

      	################################################
      	# Layers table
      	################################################
        
        #Read the configuration file and set the global variables
        importNetworksFromConfigurationFile <- function(){
            if (input$btnImportNetworks == 0) return(NULL)

            #if the table with paths to layers is valid, read the edgelist from each file
            if(length(input$project_file)>0){
                if(!file.exists(input$project_file$datapath)){
                    progress <- shiny::Progress$new(session)
                    on.exit(progress$close())
                    progress$set(message = paste('ERROR! File',input$project_file$datapath,'does not exist.'), value = 0.01)
                    Sys.sleep(10)
                    return(NULL)
                }
                fileInput <<- readLines(input$project_file$datapath)

                LAYERS <<- 0

                #By default the input is undirected and unweighted, check only variations from this assumption
                if(input$selEdgeListType == "Directed"){
                    DIRECTED <<- TRUE
                }else{
                    DIRECTED <<- FALSE
                }
                if(input$chkEdgeListWeighted){
                    WEIGHTED <<- TRUE
                }else{
                    WEIGHTED <<- FALSE
                }

                if(input$radMultiplexModel=="MULTIPLEX_IS_EDGECOLORED"){
                    print("Network model is edge-colored. Expecting one edges list per layer.")
                    #the number of lines in the config equals the number of layers
                    LAYERS <<- length(fileInput)
                }else{
                    print("Network model is not edge-colored. Expecting one multilayer edges list in extended format.")
                    #only one line in the config, must calculate from attributes (ie, LayerInfoPath file)
                    layer.info.file <- strsplit(fileInput[1],';')[[1]][2]

                    if(!file.exists(layer.info.file)){
                        progress <- shiny::Progress$new(session)
                        on.exit(progress$close())
                        progress$set(message = paste('ERROR! File',layer.info.file,'does not exist.'), value = 0.01)
                        Sys.sleep(10)
                        return(NULL)
                    }                    

                    layer.info <- read.table(layer.info.file, header=T, sep=as.character(input$txtEdgeListFileSep))
                    LAYERS <<- nrow(layer.info)
                }
                
                layerEdges <<- vector("list",LAYERS+1)
                fileName <<- vector("list",LAYERS)
                layerLabel <<- vector("list",LAYERS+1)
                layerLayoutFile <<- vector("list",LAYERS)
                layerLayout <<- vector("list",LAYERS+1)
                nodesLabel <<- vector("list",LAYERS+1)


                print(paste("Expected layers:", LAYERS))

                if(input$radMultiplexModel=="MULTIPLEX_IS_EDGECOLORED"){
                    for(l in 1:LAYERS){
                        fileName[l] <<- strsplit(fileInput[l],';')[[1]][1]
    
                        if(!file.exists(fileName[[l]][1])){
                            progress <- shiny::Progress$new(session)
                            on.exit(progress$close())
                            progress$set(message = paste('ERROR! File',fileName[[l]][1],'does not exist.'), value = 0.01)
                            Sys.sleep(10)
                            return(NULL)
                        }                    
                        
                        layerLabel[l] <<- strsplit(fileInput[l],';')[[1]][2]
                        layerLayoutFile[l] <<- strsplit(fileInput[l],';')[[1]][3]
    
                        if(!file.exists(layerLayoutFile[[l]][1])){
                            progress <- shiny::Progress$new(session)
                            on.exit(progress$close())
                            progress$set(message = paste('ERROR! File',layerLayoutFile[[l]][1],'does not exist.'), value = 0.01)
                            Sys.sleep(10)
                            return(NULL)
                        }                    
                        
                        print(paste("file",fileName[[l]][1]))
                        layerEdges[[l]] <<-  read.table(fileName[[l]][1], header=input$chkEdgeListFileHeader, sep=as.character(input$txtEdgeListFileSep))
                        print("  Edges list imported...")
                        #print(layerEdges[[l]][1:3,])
                        
                        if(input$chkEdgeListLabel){
                            #edges list are with labeled nodes instead of sequential integer IDs, we transform it
                            #according to the layout file
                            print("  Input is label-based: converting to sequential integer IDs...")
                            
                            if(layerLayoutFile[[l]][1] !="" && (!is.na(layerLayoutFile[[l]][1])) && file.exists(layerLayoutFile[[l]][1])){
                                layerTable <- read.table(layerLayoutFile[[l]][1], header=T, sep=as.character(input$txtEdgeListFileSep))
    
                                if("nodeLabel" %in% colnames(layerTable)){
                                    layerTable$nodeID <- 1:nrow(layerTable)
                                    convTable = setNames(layerTable$nodeID, as.character(layerTable$nodeLabel))
                                    for(i in 1:2) layerEdges[[l]][,i] <<- convTable[ as.character(unlist(layerEdges[[l]][,i])) ]
                                    
                                    write.table(layerEdges[[l]], paste(fileName[[l]][1],".rel",sep=""), quote=F, row.names=F, col.names=F)
                                    
                                    print("  Done!")
                                }else{
                                    progress <- shiny::Progress$new(session)
                                    on.exit(progress$close())
                                    progress$set(message = paste('ERROR! Layout file',layerLayoutFile[[l]][1],'is not in a valid format (missing nodeLabel column). This format is required when edges lists use labeled nodes instead of sequential integer IDs.'), value = 0.01)
                                    print("  Error: invalid layout format")
                                    Sys.sleep(20)
                                    return(NULL)
                                }                            
                            }else{
                                progress <- shiny::Progress$new(session)
                                on.exit(progress$close())
                                progress$set(message = paste('ERROR! Layout file',layerLayoutFile[[l]][1],'is not specified or does not exist. This file is required when edges lists use labeled nodes instead of sequential integer IDs.'), value = 0.01)
                                print("  Error: invalid layout file")
                                Sys.sleep(20)
                                return(NULL)
                            }
                        }else{
                            #check if the input is numeric, as expected, or raise errors:
                            for(i in 1:ncol(layerEdges[[l]])){
                                if( !is.numeric(layerEdges[[l]][,i]) ){
                                    progress <- shiny::Progress$new(session)
                                    on.exit(progress$close())
                                    progress$set(message = paste('ERROR! Edges list (',fileName[[l]][1],') is not specified by nodes with sequential integer IDs or weights (if any) are not numeric. If you use labels instead of sequential integer IDs you have to check the corresponding box before importing the networks.'), value = 0.01)
                                    print("  Error: invalid edges list file")
                                    Sys.sleep(20)
                                    return(NULL)
                                }
                            }
                        }
                        
                        if(layerLabel[[l]][1]=="" || is.na(layerLabel[[l]][1])){
                            layerLabel[[l]][1] <<- as.character(paste(input$txtLAYER_LABEL_PREFIX, l))
                        }
                        
                        print("  Basic safety checks passed!")                        
                    }
                }else{
                    #model is not edge-colored, one line expected
                    
                    for(l in 1:LAYERS){
                        #assign same file name to all layers, for compatibility. We will not use a lot this vector
                        fileName[l] <<- strsplit(fileInput[1],';')[[1]][1]
                    }
                    if(!file.exists(fileName[[1]][1])){
                        progress <- shiny::Progress$new(session)
                        on.exit(progress$close())
                        progress$set(message = paste('ERROR! File',fileName[[1]][1],'does not exist.'), value = 0.01)
                        Sys.sleep(10)
                        return(NULL)
                    }

                    layer.info.file <- strsplit(fileInput[1],';')[[1]][2]
                    layer.info <- read.table(layer.info.file, header=T, sep=as.character(input$txtEdgeListFileSep))
                    multilayerEdges <<- read.table(fileName[[1]][1], header=input$chkEdgeListFileHeader, sep=as.character(input$txtEdgeListFileSep))
     
                    if(ncol(multilayerEdges)==4){ 
                        multilayerEdges$V5 <<- rep(1, nrow(multilayerEdges)) 
                    }else{                    
                        if(!WEIGHTED){
                            multilayerEdges$V5 <<- rep(1, nrow(multilayerEdges))
                        }
                    }
                    #format: node layer node layer [weight]

                    for(l in 1:LAYERS){
                        layerLayoutFile[l] <<- strsplit(fileInput[1],';')[[1]][3]

                        if(!file.exists(layerLayoutFile[[l]][1])){
                            progress <- shiny::Progress$new(session)
                            on.exit(progress$close())
                            progress$set(message = paste('ERROR! File',layerLayoutFile[[l]][1],'does not exist.'), value = 0.01)
                            Sys.sleep(10)
                            return(NULL)
                        }                    

                        print(paste("file",fileName[[l]][1]))
                        
                        if(input$chkEdgeListLabel){
                            #edges list are with labeled nodes instead of sequential integer IDs, we transform it
                            #according to the layout file
                            print("  Input is label-based: converting to sequential integer IDs...")
                            
                            if(layerLayoutFile[[l]][1] !="" && (!is.na(layerLayoutFile[[l]][1])) && file.exists(layerLayoutFile[[l]][1])){
                                layerTable <- read.table(layerLayoutFile[[l]][1], header=T, sep=as.character(input$txtEdgeListFileSep))
    
                                if("nodeLabel" %in% colnames(layerTable)){
                                    if("layerLabel" %in% colnames(layer.info)){
                                        #convert nodes
                                        layerTable <- read.table(layerLayoutFile[[l]][1], header=T, sep=as.character(input$txtEdgeListFileSep))                                    
                                        layerTable$nodeID <- 1:nrow(layerTable)
                                        convTable = setNames(layerTable$nodeID, as.character(layerTable$nodeLabel))

                                        #convert layers, do this only one time, no need for each layer
                                        if(l==1){            
                                            #print(multilayerEdges)                        
                                            layer.info$layerID <- 1:nrow(layer.info)
                                            convLayerTable = setNames(layer.info$layerID, as.character(layer.info$layerLabel))
                                            multilayerEdges[,1] <<- as.numeric(convTable[ as.character(unlist(multilayerEdges[,1])) ])
                                            multilayerEdges[,3] <<- as.numeric(convTable[ as.character(unlist(multilayerEdges[,3])) ])
                                            multilayerEdges[,2] <<- as.numeric(convLayerTable[ as.character(unlist(multilayerEdges[,2])) ])
                                            multilayerEdges[,4] <<- as.numeric(convLayerTable[ as.character(unlist(multilayerEdges[,4])) ])
                                                                                        
                                            write.table(multilayerEdges, paste0(fileName[[l]][1],".rel"), quote=F, row.names=F, col.names=F)
                                            #print(multilayerEdges)
                                        }                                        
                                        
                                        layerEdges[[l]] <<- multilayerEdges[ multilayerEdges[,2]==l & multilayerEdges[,4]==l, c(1,3,5)]                                        
                                        write.table(layerEdges[[l]], paste0(fileName[[l]][1],"_layer",l,".rel"), quote=F, row.names=F, col.names=F)
                                       # print(paste("Edge list for layer",l))
                                        #print(layerEdges[[l]])
                                        print("  Done!")
                                    }else{
                                        progress <- shiny::Progress$new(session)
                                        on.exit(progress$close())
                                        progress$set(message = paste('ERROR! Layer info file',layer.info.file,'is not in a valid format (missing layerLabel column). This format is required when edges lists use labeled layers instead of sequential integer IDs.'), value = 0.01)
                                        print("  Error: invalid layer info format")
                                        Sys.sleep(20)
                                        return(NULL)                                        
                                    }
                                }else{
                                    progress <- shiny::Progress$new(session)
                                    on.exit(progress$close())
                                    progress$set(message = paste('ERROR! Layout file',layerLayoutFile[[l]][1],'is not in a valid format (missing nodeLabel column). This format is required when edges lists use labeled nodes instead of sequential integer IDs.'), value = 0.01)
                                    print("  Error: invalid layout format")
                                    Sys.sleep(20)
                                    return(NULL)
                                }                            
                            }else{
                                progress <- shiny::Progress$new(session)
                                on.exit(progress$close())
                                progress$set(message = paste('ERROR! Layout file',layerLayoutFile[[l]][1],'is not specified or does not exist. This file is required when edges lists use labeled nodes instead of sequential integer IDs.'), value = 0.01)
                                print("  Error: invalid layout file")
                                Sys.sleep(20)
                                return(NULL)
                            }
                        }else{
                            #check if the input is numeric, as expected, or raise errors:
                            for(i in 1:ncol(multilayerEdges)){
                                if( !is.numeric(multilayerEdges[,i]) ){
                                    progress <- shiny::Progress$new(session)
                                    on.exit(progress$close())
                                    progress$set(message = paste('ERROR! Edges list (',fileName[[l]][1],') is not specified by nodes/layers with sequential integer IDs or weights (if any) are not numeric. If you use labels instead of sequential integer IDs you have to check the corresponding box before importing the networks.'), value = 0.01)
                                    print("  Error: invalid edges list file")
                                    Sys.sleep(20)
                                    return(NULL)
                                }
                            }
                            
                            layerEdges[[l]] <<- multilayerEdges[ multilayerEdges[,2]==l & multilayerEdges[,4]==l, c(1,3,5)]
                        }

                        #it could be done more efficiently, but to mantain compatibility with existing structure I need this.
                        #for the size of networks muxViz can deal, this is perfectly fine
                        if("layerLabel" %in% colnames(layer.info)){
                            layerLabel[l] <<- as.character(layer.info[ as.numeric(layer.info$layerID)==l, ]$layerLabel)
                        }
                        
                        if(layerLabel[[l]][1]=="" || is.na(layerLabel[[l]][1])){
                            layerLabel[[l]][1] <<- as.character(paste(input$txtLAYER_LABEL_PREFIX, l))
                        }
                    }
                }
                
                layerLabel[[LAYERS+1]][1] <<- input$txtLAYER_AGGREGATE_LABEL_PREFIX

                
                #Find the minimum and maximum node ID in the multiplex
                idmin <- 1e100
                idmax <- 0
                offset <- 0
                cntEdges <- 0
                
                if(input$radMultiplexModel=="MULTIPLEX_IS_EDGECOLORED"){
                    for(l in 1:LAYERS){
                        if( min(layerEdges[[l]][,1:2],na.rm=T) < idmin) idmin <- min(layerEdges[[l]][,1:2],na.rm=T)
                        if( max(layerEdges[[l]][,1:2],na.rm=T) > idmax) idmax <- max(layerEdges[[l]][,1:2],na.rm=T)
        
                        cntEdges <- cntEdges + nrow(layerEdges[[l]])
                    }
                    
                    Edges <<- cntEdges
                }else{
                    idmin <- min( min(multilayerEdges[,1], na.rm=T), min(multilayerEdges[,3], na.rm=T) )
                    idmax <- max( max(multilayerEdges[,1], na.rm=T), max(multilayerEdges[,3], na.rm=T) )
                    
                    Edges <<- nrow(multilayerEdges)
                }
                
                                
                if(idmin == 0) offset <- 1
    
                Nodes <<- idmax + offset
                offsetNode <<- offset
                minNodeID <<- idmin
                maxNodeID <<- idmax
                
                #reset flags and tables
                diagnosticsMultiplexOK <<- F
                diagnosticsSingleLayerOK <<- F
                diagnosticsOK <<- F
                communityOK <<- F
                listDiagnosticsSingleLayer <<- data.frame()
                listDiagnostics <<- data.frame()
                listCommunities <<- data.frame()

                
                #External layouts. Check if all external layouts have been provided:
                LAYOUT_EXTERNAL <<- !is.na(all(layerLayoutFile != "")) 
                GEOGRAPHIC_LAYOUT <<- LAYOUT_EXTERNAL
                XMAX <<- -1e10
                YMAX <<- -1e10
                ZMAX <<- -1e10
                XMIN <<- 1e10
                YMIN <<- 1e10
                ZMIN <<- 1e10
                LONGMAX <<- -1e10
                LATMAX <<- -1e10
                LONGMIN <<- 1e10
                LATMIN <<- 1e10
                
                print("Network properties set up")
                print("Verifying external layout...")

                if(input$radMultiplexModel=="MULTIPLEX_IS_EDGECOLORED"){
                    #If each layout is specified correctly
                    for(l in 1:LAYERS){
                        if(layerLayoutFile[[l]][1] !="" && (!is.na(layerLayoutFile[[l]][1]))){
                            layerTable <- read.table(layerLayoutFile[[l]][1], header=T, sep=as.character(input$txtEdgeListFileSep))
                            if(input$chkEdgeListLabel) layerTable$nodeID <- 1:nrow(layerTable)
                            layerLayout[[l]] <<- matrix(c(1),nrow=Nodes,ncol=2)
                            
                            if(length(layerTable$nodeLat)==Nodes && length(layerTable$nodeLong)==Nodes){
                                print(paste("Layout for layer",l,"is geographic. Converting."))
                                #Get boundaries
                                longBounds = c(min(layerTable$nodeLong,na.rm=T),max(layerTable$nodeLong,na.rm=T))
                                latBounds = c(min(layerTable$nodeLat,na.rm=T),max(layerTable$nodeLat,na.rm=T))
                    
                                if(min(layerTable$nodeLong,na.rm=T) < LONGMIN) LONGMIN <<- min(layerTable$nodeLong,na.rm=T)
                                if(min(layerTable$nodeLat,na.rm=T) < LATMIN) LATMIN <<- min(layerTable$nodeLat,na.rm=T)
                                if(max(layerTable$nodeLong,na.rm=T) > LONGMAX) LONGMAX <<- max(layerTable$nodeLong,na.rm=T)
                                if(max(layerTable$nodeLat,na.rm=T) > LATMAX) LATMAX <<- max(layerTable$nodeLat,na.rm=T)
                                                                                
                                print(paste("  Latitude boundaries: ",LATMIN,LATMAX))
                                print(paste("  Longitude boundaries: ",LONGMIN,LONGMAX))
                                
                                #The input layout is geographic, we must convert it to cartesian
                                sphCoordinates <- list()
                                sphCoordinates$x <- layerTable$nodeLong
                                sphCoordinates$y <- layerTable$nodeLat
                                cartCoordinates <- mapproject(sphCoordinates,proj="mercator")
                                
                                layerTable$nodeX <- cartCoordinates$x
                                layerTable$nodeY <- cartCoordinates$y
                            }else{
                                #layout is not geographic
                                GEOGRAPHIC_LAYOUT <<- F
                            }
                            
                            if(length(layerTable$nodeX)==Nodes && length(layerTable$nodeY)==Nodes){
                                layerLayout[[l]][layerTable$nodeID + offsetNode,1:2] <<- cbind(layerTable$nodeX,layerTable$nodeY)
                    
                                if(min(layerTable$nodeX,na.rm=T) < XMIN) XMIN <<- min(layerTable$nodeX,na.rm=T)
                                if(min(layerTable$nodeY,na.rm=T) < YMIN) YMIN <<- min(layerTable$nodeY,na.rm=T)
                                if(max(layerTable$nodeX,na.rm=T) > XMAX) XMAX <<- max(layerTable$nodeX,na.rm=T)
                                if(max(layerTable$nodeY,na.rm=T) > YMAX) YMAX <<- max(layerTable$nodeY,na.rm=T)
                                
                                GEOGRAPHIC_LAYOUT <<- GEOGRAPHIC_LAYOUT && T
                                print(paste("Layout for layer",l,"specified correctly from external file",layerLayoutFile[[l]][1]))
                            }else{
                                print(paste("Layout for layer",l,"not specified correctly. Proceeding with automatic layouting."))
                                LAYOUT_EXTERNAL <<- F
                                GEOGRAPHIC_LAYOUT <<- F
                            }
                    
                            if(length(layerTable$nodeLabel)==Nodes){
                                print(paste("Nodes' labels for layer",l,"specified correctly from external file",layerLayoutFile[[l]][1]))            
                                #Assign labels to nodes
                                nodesLabel[[l]][layerTable$nodeID + offsetNode] <<- as.character(layerTable$nodeLabel)
                                print("Assigned labels.")
                            }else{
                                print(paste("Nodes' labels for layer",l,"not specified correctly. Proceeding with automatic labeling."))
                                nodesLabel[[l]] <<- 1:Nodes
                            }
                        }else{
                            print(paste("Layout for layer",l,"not specified correctly. Proceeding with automatic layouting."))
                            LAYOUT_EXTERNAL <<- F
                            GEOGRAPHIC_LAYOUT <<- F
                    
                            print(paste("Nodes' labels for layer",l,"not specified correctly. Proceeding with automatic labeling."))
                            #Assign labels to nodes
                            nodesLabel[[l]] <<- 1:Nodes
                        }
                    }
                }else{                    
                    #just one layout file, let's fix l and work with that
                    l <- 1
                    if(layerLayoutFile[[l]][1] !="" && (!is.na(layerLayoutFile[[l]][1]))){
                        layerTable <- read.table(layerLayoutFile[[l]][1], header=T, sep=as.character(input$txtEdgeListFileSep))
                        if(input$chkEdgeListLabel) layerTable$nodeID <- 1:nrow(layerTable)
                            
                        layerLayout[[l]] <<- matrix(c(1),nrow=Nodes,ncol=2)
                        
                        if(length(layerTable$nodeLat)==Nodes && length(layerTable$nodeLong)==Nodes){
                            print(paste("Layout for is geographic. Converting."))
                            #Get boundaries
                            longBounds = c(min(layerTable$nodeLong,na.rm=T),max(layerTable$nodeLong,na.rm=T))
                            latBounds = c(min(layerTable$nodeLat,na.rm=T),max(layerTable$nodeLat,na.rm=T))
                
                            if(min(layerTable$nodeLong,na.rm=T) < LONGMIN) LONGMIN <<- min(layerTable$nodeLong,na.rm=T)
                            if(min(layerTable$nodeLat,na.rm=T) < LATMIN) LATMIN <<- min(layerTable$nodeLat,na.rm=T)
                            if(max(layerTable$nodeLong,na.rm=T) > LONGMAX) LONGMAX <<- max(layerTable$nodeLong,na.rm=T)
                            if(max(layerTable$nodeLat,na.rm=T) > LATMAX) LATMAX <<- max(layerTable$nodeLat,na.rm=T)
                                                                            
                            print(paste("  Latitude boundaries: ",LATMIN,LATMAX))
                            print(paste("  Longitude boundaries: ",LONGMIN,LONGMAX))
                            
                            #The input layout is geographic, we must convert it to cartesian
                            sphCoordinates <- list()
                            sphCoordinates$x <- layerTable$nodeLong
                            sphCoordinates$y <- layerTable$nodeLat
                            cartCoordinates <- mapproject(sphCoordinates,proj="mercator")
                            
                            layerTable$nodeX <- cartCoordinates$x
                            layerTable$nodeY <- cartCoordinates$y
                        }else{
                            GEOGRAPHIC_LAYOUT <<- F
                        }
                        
                        if(length(layerTable$nodeX)==Nodes && length(layerTable$nodeY)==Nodes){
                            layerLayout[[l]][layerTable$nodeID + offsetNode,1:2] <<- cbind(layerTable$nodeX,layerTable$nodeY)
                
                            if(min(layerTable$nodeX,na.rm=T) < XMIN) XMIN <<- min(layerTable$nodeX,na.rm=T)
                            if(min(layerTable$nodeY,na.rm=T) < YMIN) YMIN <<- min(layerTable$nodeY,na.rm=T)
                            if(max(layerTable$nodeX,na.rm=T) > XMAX) XMAX <<- max(layerTable$nodeX,na.rm=T)
                            if(max(layerTable$nodeY,na.rm=T) > YMAX) YMAX <<- max(layerTable$nodeY,na.rm=T)
                            
                            GEOGRAPHIC_LAYOUT <<- GEOGRAPHIC_LAYOUT && T
                            print(paste("Layout specified correctly from external file",layerLayoutFile[[l]][1]))
                        }else{
                            print(paste("Layout not specified correctly. Proceeding with automatic layouting."))
                            LAYOUT_EXTERNAL <<- F
                            GEOGRAPHIC_LAYOUT <<- F
                        }
                
                        if(length(layerTable$nodeLabel)==Nodes){
                            print(paste("Nodes' labels specified correctly from external file",layerLayoutFile[[l]][1]))            
                            #Assign labels to nodes
                            nodesLabel[[l]][layerTable$nodeID + offsetNode] <<- as.character(layerTable$nodeLabel)
                            print("Assigned labels.")
                        }else{
                            print(paste("Nodes' labels not specified correctly. Proceeding with automatic labeling."))
                            nodesLabel[[l]] <<- 1:Nodes
                        }
                    }else{
                        print(paste("Layout not specified correctly. Proceeding with automatic layouting."))
                        LAYOUT_EXTERNAL <<- F
                        GEOGRAPHIC_LAYOUT <<- F
                
                        print(paste("Nodes' labels not specified correctly. Proceeding with automatic labeling."))
                        #Assign labels to nodes
                        nodesLabel[[l]] <<- 1:Nodes
                    }
                    
                    for(l in 2:LAYERS){
                        nodesLabel[[l]] <<- nodesLabel[[1]]
                        layerLayout[[l]] <<- layerLayout[[1]]
                    }
                }
                                
                #giving the layout of the aggregate from external file makes no sense if it is different from other layers
                #and it is also annoying to be constrained to specify the aggregate, if one does not want to show it.
                #Therefore, here I prefer to assign manually the layout of the first layer to the aggregate.
                #So far, I accept this possibility just for sake of completeness, but a correct use of muxViz should avoid
                #situations like this..
                layerLayout[[LAYERS+1]] <<- layerLayout[[1]]
                nodesLabel[[LAYERS+1]] <<- nodesLabel[[1]]
            }
        }
    
        #Dynamically create the selectInput after the networks have been imported
        output$selOutputLayerID <- renderUI({
            if (input$btnImportNetworks == 0 || length(input$project_file)==0)
                return(NULL)
    
            if(LAYERS<=0){
                importNetworksFromConfigurationFile()
            }
            tmpChoice <- c("None")
    
            for(l in 1:LAYERS){
                tmpChoice <- c(tmpChoice,paste("Layer",l,layerLabel[l]))
            }
            selectInput("selInputLayerID", HTML("Layer ID (valid only if type By_LayerID is selected):"), 
                choices = tmpChoice
                )
        })
    
        #Dynamically create the selectInput after the networks have been imported
        output$selAnularVizOutputLayerID <- renderUI({
            if (input$btnImportNetworks == 0 || length(input$project_file)==0)
                return(NULL)
    
            if(LAYERS<=0){
                importNetworksFromConfigurationFile()
            }
            tmpChoice <- c("Multiplex")
            tmpChoice <- c(tmpChoice,"Aggregate")
            tmpChoice <- c(tmpChoice,"Max entropy")
    
            for(l in 1:LAYERS){
                tmpChoice <- c(tmpChoice,paste("Layer",l,layerLabel[l]))
            }
            
            selectInput("selAnularVizInputLayerID", HTML("Sort nodes according to layer ID in single-descriptor visualization:"), 
                choices = tmpChoice
                )
        })

        #Dynamically create the selectInput after the diagnostics have been calculated
        output$selVizNodeSizeOutputID <- renderUI({
            if (input$btnImportNetworks == 0 || input$btnCalculateCentralityDiagnostics == 0 || length(input$project_file)==0)
                return(NULL)
    
            tmpChoice <- "Uniform"
            
            for( attrib in attributes(listDiagnostics[[1]])$names ){        
                if( (attrib!="Node" && attrib!="Label" && attrib!="Layer") && length(unique(listDiagnostics[[1]][,attrib]))>1 ) tmpChoice <- c(tmpChoice,attrib)
            }
            
            selectInput("selVizNodeSizeID", HTML("Node size proportional to:"), 
                choices = tmpChoice
                )
        })

        #Dynamically create the selectInput after the diagnostics have been calculated
        output$selVizNodeColorOutputID <- renderUI({
            if (input$btnImportNetworks == 0 || input$btnCalculateCentralityDiagnostics == 0 || length(input$project_file)==0)
                return(NULL)
    
            tmpChoice <- NULL
            
            for( attrib in attributes(listDiagnostics[[1]])$names ){        
                if( (attrib!="Node" && attrib!="Label" && attrib!="Layer") && length(unique(listDiagnostics[[1]][,attrib]))>1 ) tmpChoice <- c(tmpChoice,attrib)
            }
            
            selectInput("selVizNodeColorID", HTML("Node color proportional to:"), 
                choices = tmpChoice
                )
        })

        #Dynamically create the selectInput after the diagnostics have been calculated
        output$selVizNodeColorTopOutputID <- renderUI({
            if (input$btnImportNetworks == 0 || input$btnCalculateCentralityDiagnostics == 0 || length(input$project_file)==0)
                return(NULL)
    
            tmpChoice <- NULL
            
            for( attrib in attributes(listDiagnostics[[1]])$names ){        
                if( (attrib!="Node" && attrib!="Label" && attrib!="Layer") && length(unique(listDiagnostics[[1]][,attrib]))>1 ) tmpChoice <- c(tmpChoice,attrib)
            }
            
            selectInput("selVizNodeColorTopID", HTML("Rank calculated from:"), 
                choices = tmpChoice
                )
        })
    
        #Dynamically create the selectInput after the diagnostics have been calculated
        output$selAnularVizOutputFeatureID <- renderUI({
            if (input$btnImportNetworks == 0 || input$btnCalculateCentralityDiagnostics == 0 || length(input$project_file)==0)
                return(NULL)
    
            tmpChoice <- NULL
            
            for( attrib in attributes(listDiagnostics[[1]])$names ){        
                if( (attrib!="Node" && attrib!="Label" && attrib!="Layer") && length(unique(listDiagnostics[[1]][,attrib]))>1 ) tmpChoice <- c(tmpChoice,attrib)
            }
            
            selectInput("selAnularVizInputFeatureID", HTML("Sort nodes according to centrality in multiplex visualization:"), 
                choices = tmpChoice
                )
        })

        #Dynamically create the selectInput after the diagnostics have been calculated
        output$selDiagnosticsCentralityVizOutputID <- renderUI({
            if (input$btnImportNetworks == 0 || input$btnCalculateCentralityDiagnostics == 0 || length(input$project_file)==0)
                return(NULL)
    
            tmpChoice <- NULL
            
            for( attrib in attributes(listDiagnostics[[1]])$names ){        
                if( (attrib!="Node" && attrib!="Label" && attrib!="Layer") && length(unique(listDiagnostics[[1]][,attrib]))>1 ) tmpChoice <- c(tmpChoice,attrib)
            }
            
            selectInput("selDiagnosticsCentralityVizID", HTML("Select the centrality descriptor to analyze:"), 
                choices = tmpChoice
                )
        })

        output$selDiagnosticsCentralityVizScatterOutputID <- renderUI({
            if (input$btnImportNetworks == 0 || input$btnCalculateCentralityDiagnostics == 0 || length(input$project_file)==0)
                return(NULL)
    
            tmpChoice <- NULL
            
            for( attrib in attributes(listDiagnostics[[1]])$names ){        
                if( (attrib!="Node" && attrib!="Label" && attrib!="Layer") && length(unique(listDiagnostics[[1]][,attrib]))>1 ) tmpChoice <- c(tmpChoice,attrib)
            }
            
            selectInput("selDiagnosticsCentralityVizScatterID", HTML("Select the centrality descriptor to analyze:"), 
                choices = tmpChoice
                )
        })


        output$selDiagnosticsCentralityVizScatterSizeOutputID <- renderUI({
            if (input$btnImportNetworks == 0 || input$btnCalculateCentralityDiagnostics == 0 || length(input$project_file)==0)
                return(NULL)
    
            tmpChoice <- "Uniform"
            
            for( attrib in attributes(listDiagnostics[[1]])$names ){        
                if( (attrib!="Node" && attrib!="Label" && attrib!="Layer") && length(unique(listDiagnostics[[1]][,attrib]))>1 ) tmpChoice <- c(tmpChoice,attrib)
            }
            
            selectInput("selDiagnosticsCentralityVizScatterSizeID", HTML("Circle radius proportional to:"), 
                choices = tmpChoice
                )
        })
                        
        #Create a summary of the config file and the networks, when the Import button is pushed
        output$projectSummaryHTML <- reactive({
            if (input$btnImportNetworks == 0 || length(input$project_file)==0)
                return(list())
                
            if(DIRECTED){
                layerType <- "Directed"
            }else{
                layerType <- "Undirected"
            }
            if(WEIGHTED){
                layerType <- paste(layerType,"and Weighted")
            }
    
            if(LAYOUT_EXTERNAL){
                extLayout <- "TRUE"
            }else{
                extLayout <- "FALSE"
            }
            if(GEOGRAPHIC_LAYOUT){
                geoLayout <- paste("TRUE","( Lat bounds:",LATMIN,"/",LATMAX,"Long bounds:",LONGMIN,"/",LONGMAX,")")
            }else{
                geoLayout <- "FALSE"
            }
    
            #be careful: "=" is required instead of "<-"
            return(list(
                sumLayers = as.character(LAYERS),
                sumLayerType = layerType,
                sumNodes = as.character(Nodes),
                sumMinNodeID = as.character(minNodeID),
                sumMaxNodeID = as.character(maxNodeID),
                sumEdges = as.character(Edges),
                sumIsLayoutExternal = extLayout,
                sumIsLayoutGeographic = geoLayout
            ))
        })
    
        #Fill the table summarizing the config file
        output$layersTable <- renderTable({
            input$btnImportNetworks        
            inFile <- input$project_file
            
            if (is.null(inFile))
                return(NULL)
    
            tmplayerTable <- read.csv(inFile$datapath, header=input$chkConfigFileHeader, sep=input$txtConfigFileSep)
            
            if(input$radMultiplexModel=="MULTIPLEX_IS_EDGECOLORED"){
                colnames(tmplayerTable) <- c("EdgeListPath", "Label", "LayoutPath")
            }else{
                colnames(tmplayerTable) <- c("EdgeListPath", "LayerInfoPath", "LayoutPath")   
            }
            
            return(tmplayerTable)
        })
    
        #Create a summary of the timeline file and the networks, when the Import button is pushed
        output$projectTimelineHTML <- reactive({
            if (input$btnImportTimeline == 0 || input$btnRenderNetworks == 0 || length(input$project_file)==0)
                return(list())

            #be careful: "=" is required instead of "<-"
            return(list(
                timelineTimesteps = max(dfTimeline$timeStep)-min(dfTimeline$timeStep)+1,
                timelineAffectNodes = sum(dfTimeline$entity=="node"),
                timelineAffectEdges = sum(dfTimeline$entity=="edge")
            ))
        })


      	################################################
      	# Create the graph objects
      	################################################
        
        #This should be called AFTER importNetworksFromConfigurationFile, where the 
        #relevant variables are filled with the data

        buildNetworks <- function(){
            if (input$btnImportNetworks == 0 || LAYERS<=0) return(NULL)
              
            g <<- vector("list",LAYERS+1)
            AdjMatrix <<- vector("list",LAYERS+1)
            
            #Adj_aggr <- matrix(0,ncol=Nodes,nrow=Nodes)
            AdjMatrix[[LAYERS+1]] <<- matrix(0,ncol=Nodes,nrow=Nodes)
            
            for(l in 1:LAYERS){    
                #account for the possibility of having layers with no intra-links
                if(nrow(layerEdges[[l]])==0){
                    if(ncol(layerEdges[[l]])==2){ colnames(layerEdges[[l]]) <<- c("from","to") }                
                    if(ncol(layerEdges[[l]])==3){ colnames(layerEdges[[l]]) <<- c("from","to","weight") }
                    if(ncol(layerEdges[[l]])>3){
                        print("ERROR! More than 3 columns whereas equal or smaller than 3 expected.")
                        return(NULL)
                    }                
                    #generate the network object
                    g[[l]] <<- graph.empty(directed=DIRECTED, n=Nodes)
                    AdjMatrix[[l]] <<- get.adjacency(g[[l]]) #I use this to avoid class incompatibility 
                }else{
                    if(ncol(layerEdges[[l]])==2){
                        layerEdges[[l]] <<- cbind(layerEdges[[l]], rep(1, nrow(layerEdges[[l]])))
                        colnames(layerEdges[[l]]) <<- c("from","to")
                    }                
                    if(ncol(layerEdges[[l]])==3){
                        colnames(layerEdges[[l]]) <<- c("from","to","weight")
                    }
                    if(ncol(layerEdges[[l]])>3){
                        print("ERROR! More than 3 columns whereas equal or smaller than 3 expected.")
                        return(NULL)
                    }                
                    
                    if(offsetNode>0){
                        layerEdges[[l]][,1] <<- layerEdges[[l]][,1] + offsetNode
                        layerEdges[[l]][,2] <<- layerEdges[[l]][,2] + offsetNode
                    }
                
                    #todo: here the code for cutting/threshold, if any
                
                    if(WEIGHTED){
                        if(input$chkRESCALE_WEIGHT){
                            if(ncol(layerEdges[[l]])==3){
                                print("Rescaling weights...")
                                layerEdges[[l]][,3] <<- layerEdges[[l]][,3]/min(layerEdges[[l]][,3],na.rm=T)
                            }
                        }
                    }
                
                    #generate the network object
                    g[[l]] <<- graph.data.frame(layerEdges[[l]], directed=DIRECTED, vertices=1:Nodes)
                    AdjMatrix[[l]] <<- get.adjacency(g[[l]], attr="weight")
    
                    #update the aggregate
                    AdjMatrix[[LAYERS+1]] <<- AdjMatrix[[LAYERS+1]] + AdjMatrix[[l]]
                }
                
                print(paste("Layer ",l,": ",fileName[[l]][1],"   Name:",layerLabel[[l]][1]))
                print(paste("Layer ",l," Directed: ",DIRECTED))
                print(paste("Layer ",l," Weighted: ",WEIGHTED))
                print(paste(nrow(layerEdges[[l]]),"Edges in layer (excluding inter-links): ",l))
            }
            
            #the aggregate
            
            #only if the network is interdependent
            if(input$radMultiplexModel=="MULTIPLEX_IS_INTERDEPENDENT"){
                #in this case the aggregate is just the input network itself
                g[[LAYERS+1]] <<- graph.data.frame( data.frame(from=multilayerEdges[,1], to=multilayerEdges[,3], weight=multilayerEdges[,5]) , directed=DIRECTED, vertices=1:Nodes)
            }else{
                g[[LAYERS+1]] <<- graph.adjacency(AdjMatrix[[LAYERS+1]],weighted=T)
            }

            #if the network is non-edge colored (ie, there are interlinks)
            if(input$radMultiplexModel!="MULTIPLEX_IS_EDGECOLORED"){
                #the trick is to build a huge network where the number of nodes is NxL

                #For interdependent networks we will apply the layout to each layer separately
                #For interconnected multiplex and general multilayer we will apply the layout to the aggregate
                #Finally, we will assign nodes and edges properties to their corresponding elements in the huge network
                #and we will use that for plotting
                
                multilayerEdges.tmp <- multilayerEdges
                
                #the idea is to relabel nodes from 1 to NL
                multilayerEdges.tmp[,1] <- multilayerEdges.tmp[,1] + Nodes*(multilayerEdges.tmp[,2]-1)
                multilayerEdges.tmp[,3] <- multilayerEdges.tmp[,3] + Nodes*(multilayerEdges.tmp[,4]-1)
                multilayerEdges.tmp[,2] <- NULL
                multilayerEdges.tmp[,4] <- NULL
                colnames(multilayerEdges.tmp) <- c("from", "to", "weight")
                
                g.multi <<- graph.data.frame(multilayerEdges.tmp, directed=DIRECTED, vertices=1:(Nodes*LAYERS))
                
                #print(V(g.multi))
                #print(E(g.multi))
                
                multilayerEdges.tmp <- NULL
            }
    
            btnImportNetworksValue <<- input$btnImportNetworks
        }
    
        observe({
            if(input$btnImportNetworks==0)
                return()
    
            isolate({
                progress <- shiny::Progress$new()
                on.exit(progress$close())
                progress$set(message = 'Importing edges lists...', value = 0.2)
                Sys.sleep(1)
                res <- importNetworksFromConfigurationFile()
                if(is.null(res)){
                    progress$set(message = 'Errors occurred while reading input...', value = 0.2)
                    Sys.sleep(5)
                    return()
                }

                progress$set(detail = 'Building the network...', value = 0.6)
                Sys.sleep(1)
                buildNetworks()

                #Fill the table summarizing the config file
                output$edgelistTable <- renderGvis({
                    if(input$btnImportNetworks==0 || LAYERS==0 || input$chkOutputEdgelistTable==FALSE)
                        return(NULL)
        
                    progress$set(detail = 'Creating tables...', value = 0.9)
                    #Sys.sleep(2)

                    listEdgelistMerge <- NULL
                    if(input$radMultiplexModel=="MULTIPLEX_IS_EDGECOLORED"){
                        for(l in 1:LAYERS){
                            thisEdge <- cbind(get.edgelist(g[[l]]), E(g[[l]])$weight)
                            for(n in 1:nrow(thisEdge)){
                                listEdgelistMerge <- rbind(listEdgelistMerge,data.frame(cbind(Layer = l, nodeID1 = thisEdge[n,1],nodeID2 = thisEdge[n,2], Node1 = nodesLabel[[l]][thisEdge[n,1]], Node2 = nodesLabel[[l]][thisEdge[n,2]], Weight = thisEdge[n,3])))
                            }
                        }
                        #print(listEdgelistMerge)
                    }else{
                        listEdgelistMerge <- multilayerEdges
                        colnames(listEdgelistMerge) <- c("Node1", "Layer1", "Node2", "Layer2", "Weight")
                    }

                    gvisTable(listEdgelistMerge,options=googleVisEdgelistTableOptions())
                })   
        
                btnImportNetworksValue <<- input$btnImportNetworks
                
                progress$set(detail = 'Import Completed!', value = 1)
                Sys.sleep(2)
            })
        })

        observe({
            if(input$btnRenderNetworks==0 || input$btnApplyLayout==0 || input$btnImportNetworks == 0 || LAYERS<=0) return()
                
            if(btnResetLightsValue==input$btnResetLights) return()

            progress <- shiny::Progress$new(session)
            on.exit(progress$close())
            progress$set(message = 'Resetting lights...', value = 0.2)
            Sys.sleep(1)

            flag <- F
            while(!flag){
                tryCatch({
                    print("Popping lights...")
                    rgl.pop("lights")},
                    error=function(e){
                        print("Warning: no more lights to pop")
                        },
                    finally={flag=T}
                    )
            }
            #rgl.light(theta = 0, phi = 0, viewpoint.rel = TRUE, ambient = "#FFFFFF", 
            #            diffuse = "#FFFFFF", specular = "#FFFFFF")
                        
            btnResetLightsValue <<- input$btnResetLights

            progress$set(detail = 'Done!', value = 1)
            Sys.sleep(2)
        })
        
        observe({
            if(input$btnRenderNetworks==0 || input$btnApplyLayout==0 || input$btnImportNetworks == 0 || LAYERS<=0) return()
                
            if(btnImportTimelineValue==input$btnImportTimeline) return()

            if(input$btnImportTimeline==0) return()
            
            if(is.null(input$timeline_file$datapath)) return()
    
            isolate({
                progress <- shiny::Progress$new(session)
                on.exit(progress$close())
                progress$set(message = 'Importing timeline...', value = 0.2)
                Sys.sleep(1)
                res <- importTimelineFromFile()
                if(is.null(res)){
                    progress$set(message = 'Errors occurred while reading input...', value = 0.2)
                    Sys.sleep(5)
                    return()
                }
                                        
                btnImportTimelineValue <<- input$btnImportTimeline
                
                progress$set(detail = 'Import Completed!', value = 1)
                Sys.sleep(2)
            })
        })
        
        importTimelineFromFile <- function(){
            if(length(input$timeline_file)>0){
                if(!file.exists(input$timeline_file$datapath)){
                    progress <- shiny::Progress$new(session)
                    on.exit(progress$close())
                    progress$set(message = paste('ERROR! File',input$timeline_file$datapath,'does not exist.'), value = 0.01)
                    Sys.sleep(10)
                    return(NULL)
                }
            }

            fileTimeline <<- input$timeline_file$datapath
            dfTimeline <<-  read.table(fileTimeline, header=TRUE, sep=as.character(input$txtTimelineFileSep))
            
            if(!(all(as.integer(as.character(dfTimeline$layerID)) >= 1) & all(as.integer(as.character(dfTimeline$layerID)) <= (LAYERS+1)))){
                progress <- shiny::Progress$new(session)
                on.exit(progress$close())
                progress$set(message = paste('ERROR! There are unknown layers in the timeline file'), value = 0.01)
                Sys.sleep(10)
                return(NULL)
            }

            if(!(all(as.integer(as.character(dfTimeline[dfTimeline$entity=="node",]$nodeID)) >= minNodeID) & all(as.integer(as.character(dfTimeline[dfTimeline$entity=="node",]$nodeID)) <= maxNodeID))){
                progress <- shiny::Progress$new(session)
                on.exit(progress$close())
                progress$set(message = paste('ERROR! There are unknown nodes in the timeline file'), value = 0.01)
                Sys.sleep(10)
                return(NULL)
            }
            
            print(paste("Timeline imported! Found",nrow(dfTimeline),"entries"))
        }
        
        #Export the visualization for each snapshot of the timeline
        observe({
            #print(paste(input$btnImportTimeline,input$btnRenderNetworks,input$btnApplyLayout,input$btnImportNetworks,LAYERS,length(dfTimeline),btnRenderDynamicsSnapshotsValue,input$btnRenderDynamicsSnapshots))
            if(btnRenderDynamicsSnapshotsValue==input$btnRenderDynamicsSnapshots) return()
            
            if(input$btnImportTimeline==0 || input$btnRenderNetworks==0 || length(dfTimeline)==0) return()

            isolate({
                progress <- shiny::Progress$new(session)
                on.exit(progress$close())
    
                progress$set(detail = 'Building the dynamics...', value = 0.1)
                Sys.sleep(1)
                
                #order the timeline by timestep
                #dfTimeline[order(dfTimeline$timeStep),]
                                
                #get the list of all (unique) timesteps
                timestepsList <- sort(unique(dfTimeline$timeStep))

                #re-set the default color and size
                if(length(input$txtTimelineDefaultNodesColor)>0){
                    for(l in 1:(LAYERS+1)) V(g[[l]])$color <- input$txtTimelineDefaultNodesColor
                }else{
                    for(l in 1:(LAYERS+1)) V(g[[l]])$color <- defaultVcolor[[l]]
                }
                
                if(length(input$txtTimelineDefaultNodesSize)>0){
                    for(l in 1:(LAYERS+1)) V(g[[l]])$size <- as.numeric(input$txtTimelineDefaultNodesSize)
                }else{
                    for(l in 1:(LAYERS+1)) V(g[[l]])$size <- defaultVsize[[l]]
                }

                if(!input$chkNODE_LABELS_SHOW){
                    for(l in 1:(LAYERS+1)) V(g[[l]])$label <- ""
                }else{
                    for(l in 1:(LAYERS+1)) V(g[[l]])$label <- nodesLabel[[l]]
                }

                if(input$chkNODE_ISOLATED_HIDE){
                    #this piece of code must be executed after the above one, to change the size of isolated
                    #nodes to zero, and also their label to ""
                    
                    if(input$radMultiplexModel == "MULTIPLEX_IS_EDGECOLORED"){
                        for(l in 1:(LAYERS+1)){
                            arrayStrength <- graph.strength(g[[l]],mode="total")
                            V(g[[l]])[arrayStrength==0.]$size <- 0
                            V(g[[l]])[arrayStrength==0.]$label <- ""
                        }
                    }else{
                        if(input$chkNODE_ISOLATED_HIDE_INTERLINKS){
                            #account for degree in the multiplex
                            
                            arrayStrength <- graph.strength(g.multi,mode="total")
                            idxtohide <- which(arrayStrength==0.)
                            inlayers <- floor((idxtohide-1)/Nodes) + 1
                            innodes <- (idxtohide-1) %% Nodes + 1
                            
                            for(l in 1:LAYERS){
                                idxs <- which(inlayers==l)
                                nodes2hide <- which(V(g[[l]]) %in% innodes[idxs])
                                V(g[[l]])[nodes2hide]$size <- 0
                                V(g[[l]])[nodes2hide]$label <- ""
                            }
                            
                            #aggregate must be done separately
                            arrayStrength <- graph.strength(g[[LAYERS+1]],mode="total")
                            V(g[[LAYERS+1]])[arrayStrength==0.]$size <- 0
                            V(g[[LAYERS+1]])[arrayStrength==0.]$label <- ""
                        }else{
                            #do not account for interlinks, just intralinks
                            for(l in 1:(LAYERS+1)){
                                arrayStrength <- graph.strength(g[[l]],mode="total")
                                V(g[[l]])[arrayStrength==0.]$size <- 0
                                V(g[[l]])[arrayStrength==0.]$label <- ""
                            }
                        }
                    }
                }

                if(length(input$txtTimelineDefaultEdgesColor)>0){
                    for(l in 1:(LAYERS+1)) E(g[[l]])$color <- input$txtTimelineDefaultEdgesColor
                }else{
                    for(l in 1:(LAYERS+1)) E(g[[l]])$color <- defaultEcolor[[l]]
                }

                if(length(input$txtTimelineDefaultEdgesSize)>0){
                    for(l in 1:(LAYERS+1)) E(g[[l]])$size <- as.numeric(input$txtTimelineDefaultEdgesSize)
                }else{
                    for(l in 1:(LAYERS+1)) E(g[[l]])$size <- defaultEsize[[l]]
                }


                print("Rendering dynamics")

                for(timestep in min(timestepsList):max(timestepsList)){                    
                    #extract all the rows corresponding to this timestep and create a new dataframe
                    tmpdfTimeline <- dfTimeline[dfTimeline$timeStep==timestep,]
                                        
                    print(paste("  Timeline (",100*which(timestepsList==timestep)/(max(timestepsList)-min(timestepsList)+1),"%",") -> timestep:",timestep))

                    xmin.tmp <- 1e100
                    xmax.tmp <- -1e100
                    ymin.tmp <- 1e100
                    ymax.tmp <- -1e100

                    if(input$chkPLOT_WITH_RGL){
                        rgl.clear()
                        tryCatch(rgl.pop("lights"),error=function(e) print("Warning: no lights to pop"))
                        rgl.light(theta = 0, phi = 0, viewpoint.rel = TRUE, ambient = "#FFFFFF", 
                       diffuse = "#FFFFFF", specular = "#FFFFFF")
                    }else{                        
                        for(l in 1:LAYERS){
                            xmin.tmp <- min(xmin.tmp, min(layouts[[l]][,1]))
                            xmax.tmp <- max(xmax.tmp, max(layouts[[l]][,1]))
                            ymin.tmp <- min(ymin.tmp, min(layouts[[l]][,2]))
                            ymax.tmp <- max(ymax.tmp, max(layouts[[l]][,2]))
                        }
                        
                        if(input$chkAGGREGATE_SHOW){
                            l <- LAYERS+1
                            xmin.tmp <- min(xmin.tmp, min(layouts[[l]][,1]))
                            xmax.tmp <- max(xmax.tmp, max(layouts[[l]][,1]))
                            ymin.tmp <- min(ymin.tmp, min(layouts[[l]][,2]))
                            ymax.tmp <- max(ymax.tmp, max(layouts[[l]][,2]))
                        }
                        
                        xmin.tmp <- min(xmin.tmp*0.95, xmin.tmp*1.05)
                        xmax.tmp <- max(xmax.tmp*0.95, xmax.tmp*1.05)
                        ymin.tmp <- min(ymin.tmp*0.95, ymin.tmp*1.05)
                        ymax.tmp <- max(ymax.tmp*0.95, ymax.tmp*1.05)                        
                    }

                    #for each time step, we have to modify the state of the network 
                    #nodes
                    tmpdfTimelineNode <- tmpdfTimeline[tmpdfTimeline$entity=="node",]
                    #print(tmpdfTimelineNode)
                    if(nrow(tmpdfTimelineNode)>0){
                        for(r in 1:nrow(tmpdfTimelineNode)){
                            l <- as.integer(as.character(tmpdfTimelineNode[r,]$layerID))
                            n <- which(V(g[[l]])==as.integer(as.character(tmpdfTimelineNode[r,]$nodeID)))

                            #we have to rescale node's default size, not the previous one..
                            defNodeSize <- 1
                            if(length(input$txtTimelineDefaultNodesSize)>0){
                                defNodeSize <- as.numeric(input$txtTimelineDefaultNodesSize)
                            }else{
                                defNodeSize <- defaultVsize[[l]][n]
                            }

                            V(g[[l]])$size[n] <- defNodeSize * as.double(as.character(tmpdfTimelineNode[r,]$sizeFactor))
                            V(g[[l]])$color[n] <- paste("#",tmpdfTimelineNode[r,]$color,sep='')
                        }
                    }else{
                        #no changes in the state of the nodes
                    }
                             
                    #edges
                    tmpdfTimelineEdge <- tmpdfTimeline[tmpdfTimeline$entity=="edge",]
                    #print(tmpdfTimelineNode)
                    if(nrow(tmpdfTimelineEdge)>0){
                        for(r in 1:nrow(tmpdfTimelineEdge)){
                            l <- as.integer(as.character(tmpdfTimelineEdge[r,]$layerID))
                            pair <- as.character(tmpdfTimelineEdge[r,]$nodeID)
                            n1 <- which(V(g[[l]])==as.integer( unlist(strsplit(pair,"-"))[1] ))
                            n2 <- which(V(g[[l]])==as.integer( unlist(strsplit(pair,"-"))[2] ))

                            #we have to rescale node's default size, not the previous one..
                            defEdgeSize <- 1
                            if(length(input$txtTimelineDefaultEdgesSize)>0){
                                defEdgeSize <- as.numeric(input$txtTimelineDefaultEdgesSize)
                            }else{
                                defEdgeSize <- AdjMatrix[[l]][n1,n2]
                            }

                            E(g[[l]])[n1 %--% n2]$size <- defEdgeSize * as.double(as.character(tmpdfTimelineEdge[r,]$sizeFactor))
                            E(g[[l]])[n1 %--% n2]$color <- paste("#",tmpdfTimelineEdge[r,]$color,sep='')
                        }
                    }else{
                        #no changes in the state of the edges
                    }

                    vecInactiveLayers <- as.numeric(strsplit(input$txtLAYERS_ACTIVE, ",")[[1]])

                    timelineFolder <- concatenatePath( concatenatePath("export","timeline"), input$txtProjectName)

                    #create the folder if it does not exist
                    dir.create(buildPath("export","timeline"), showWarnings = FALSE)
                    dir.create(timelineFolder, showWarnings = FALSE)

                    FILE_RGL_SNAPSHOT <- buildPath(timelineFolder,paste(input$txtProjectName,"_",sprintf("%05d",timestep),".png",sep=""))
                    
                    if(!input$chkPLOT_WITH_RGL){
                        width <- as.numeric(input$txtExportRenderingClassicPNGWidth)
                        height <- as.numeric(input$txtExportRenderingClassicPNGHeight)
                        dpi <- as.numeric(input$txtExportRenderingClassicPNGResolution)
        
                        if(input$chkTIMELINE_RENDER_TO_FILE){
                            png(filename=FILE_RGL_SNAPSHOT, width=width, height=height, res=dpi)
                        }

                        par(mar=c(0, 0, 0, 0), xaxs='i', yaxs='i') 
                        par(oma=c(0, 0, 0, 0))

                        plot(x=NULL, y=NULL, type="n", 
                            xlim=c(xmin.tmp,xmax.tmp), ylim=c(ymin.tmp,ymax.tmp)
                            )
                        rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col =input$txtBACKGROUND_COLOR)

                    }

                    #now render the network
                    for(l in 1:(LAYERS+1)){
                        if( l %in% vecInactiveLayers ){
                            #skip layers set to be inactive
                            next
                        }

                        #progress$set(message = paste('Layer',l,'...'), value = 0.05 + 0.85*l/(LAYERS+1))
        
                        if(l==(LAYERS+1)){
                            if((!input$chkAGGREGATE_SHOW || LAYERS==1) || (input$chkPLOT_AS_EDGE_COLORED && LAYOUT_DIMENSION==3)){
                                #if we don't want to show the aggregate, we must skip the rest
                                #we must skip also if the layers is just 1
                                next
                            }
                        }
                        if(l<LAYERS+1){
                            print(paste("    Timeline Layer: ",l))
                        }else{
                            print(paste("    Timeline Layer: Aggregate"))    
                        }

                        #other assignments                        
                        V(g[[l]])$vertex.label.color <- input$txtNODE_LABELS_FONT_COLOR
                        E(g[[l]])$curve<- as.numeric(input$txtEDGE_BENDING)
                        V(g[[l]])$shape <- "circle"
                        V(g[[l]])$shape[V(g[[l]])$size==0] <- "none"                    
                        V(g[[l]])$framecolor <- input$txtNODE_FRAME_COLOR
                        if(input$txtNODE_FRAME_COLOR==""){ V(g[[l]])$framecolor <- V(g[[l]])$color }
                        
                        if(input$chkNODE_LABELS_SHOW_WRAP){
                            V(g[[l]])$label2 <- lapply(lapply(V(g[[l]])$label, function(x) strwrap(x,as.numeric(input$txtNODE_LABELS_WRAP))), function(x) paste(x, collapse='\n'))
                        }else{
                            V(g[[l]])$label2 <- V(g[[l]])$label
                        }
        
                        if(input$chkPLOT_WITH_RGL){
                            print("      openGL phase...")
                            #plot the graph with openGL    
                            #print(layouts[[l]])
                            #V(g[[l]])$label <- ""
    
                            rglplot(g[[l]], layout=layouts[[l]],
                                                vertex.size=V(g[[l]])$size, 
                                                vertex.color=V(g[[l]])$color,
                                                vertex.label="",#V(g[[l]])$label,
                                                vertex.label.dist=as.numeric(input$txtNODE_LABELS_DISTANCE), #,+ 0.01*V(g[[l]])$size,
                                                vertex.label.font=2,
                                                vertex.label.cex=0, 
                                                vertex.label.color=V(g[[l]])$vertex.label.color,
                                                edge.width=E(g[[l]])$size, 
                                                edge.color=E(g[[l]])$color, 
                                                edge.arrow.size=as.numeric(input$txtLAYER_ARROW_SIZE), 
                                                edge.arrow.width=as.numeric(input$txtLAYER_ARROW_WIDTH), 
                                                edge.curved=E(g[[l]])$curve,
                                                rescale=F)
                        }else{
                            print("  Standard device output...")
        
                            #plot the graph with openGL    
                            #print(layouts[[l]])
                            plot(g[[l]], layout=layouts[[l]],
                                            vertex.size=V(g[[l]])$size, 
                                            vertex.shape=V(g[[l]])$shape,
                                            vertex.color=V(g[[l]])$color,
                                            vertex.frame.color=V(g[[l]])$framecolor,
                                            vertex.label=V(g[[l]])$label2,
                                            vertex.label.dist=as.numeric(input$txtNODE_LABELS_DISTANCE), #,+ 0.01*V(g[[l]])$size,
                                            vertex.label.font=2,
                                            vertex.label.cex=as.numeric(input$txtNODE_LABELS_FONT_SIZE), 
                                            vertex.label.color=V(g[[l]])$vertex.label.color,
                                            edge.width=E(g[[l]])$size, 
                                            edge.color=E(g[[l]])$color, 
                                            edge.arrow.size=as.numeric(input$txtLAYER_ARROW_SIZE), 
                                            edge.arrow.width=as.numeric(input$txtLAYER_ARROW_WIDTH), 
                                            edge.curved=E(g[[l]])$curve,
                                            rescale=F, add=T)
                                            
                                title(main=input$txtPLOT_TITLE, sub=input$txtPLOT_SUBTITLE)
                        }
                                                                                        
                        print(paste("    Layout of layer: finished."))
                    }

                    if(input$chkINTERLINK_SHOW && LAYERS>1){
                        if(input$radMultiplexModel!="MULTIPLEX_IS_EDGECOLORED"){
                            print("Adding interlayer links.")
    
                            #set to 0 the width of intra-layer links
                            E(g.multi)$width <- as.numeric(input$txtINTERLINK_WIDTH)*E(g.multi)$weight
                            E(g.multi)[which(multilayerEdges[,2]==multilayerEdges[,4])]$width <- 0
                            
                            #the same for interlinks from and to inactive layers 
                            for(l in vecInactiveLayers){
                                E(g.multi)[which(multilayerEdges[,2]==l | multilayerEdges[,4]==l)]$width <- 0
                            }
                            
                            #setup the layout for g.multi by merging the layout of each layer, in order
                            layout.multi <<- matrix(0, ncol=3, nrow=Nodes*LAYERS)

                            for(l in 1:LAYERS){
                                layout.multi[ (1 + (l-1)*Nodes):(l*Nodes), 1] <<- layouts[[l]][, 1]
                                layout.multi[ (1 + (l-1)*Nodes):(l*Nodes), 2] <<- layouts[[l]][, 2]
                                layout.multi[ (1 + (l-1)*Nodes):(l*Nodes), 3] <<- layouts[[l]][, 3]
                            }
        
                            if(input$chkPLOT_WITH_RGL){
                                #Print the interlinks by superimposing the g.multi
                                rglplot(g.multi, layout=layout.multi,
                                                    vertex.size=0, 
                                                    vertex.label="",
                                                    vertex.label.cex=0,
                                                    edge.width=E(g.multi)$width, 
                                                    edge.color=input$txtINTERLINK_COLOR, 
                                                    edge.arrow.size=as.numeric(input$txtLAYER_ARROW_SIZE), 
                                                    edge.arrow.width=as.numeric(input$txtLAYER_ARROW_WIDTH), 
                                                    edge.curved=as.numeric(input$txtEDGE_BENDING),
                                                    edge.lty = input$selINTERLINK_TYPE,
                                                    rescale=F)
                                #edge/node transparancy not yet supported by rglplot
                                #alpha=as.numeric(input$txtINTERLINK_TRANSP))
                            }else{
                                plot(g.multi, layout=layout.multi,
                                            vertex.size=0, 
                                            vertex.shape="none",
                                            vertex.label="",
                                            vertex.label.cex=0,
                                            edge.width=E(g.multi)$width, 
                                            edge.color=addalpha(input$txtINTERLINK_COLOR,as.numeric(input$txtINTERLINK_TRANSP)), 
                                            edge.arrow.size=as.numeric(input$txtLAYER_ARROW_SIZE), 
                                            edge.arrow.width=as.numeric(input$txtLAYER_ARROW_WIDTH), 
                                            edge.curved=as.numeric(input$txtEDGE_BENDING),
                                            edge.lty = input$selINTERLINK_TYPE,
                                            rescale=F, add=T)
                            }
                        }                
                    }                

                    if(input$chkPLOT_WITH_RGL){
                        #Call the visualization of other graphics
                        FinalizeRenderingMultiplex(progress)
                    
                        if(!is.na(tmpdfTimeline$labelStep[1])){
                            #assuming that all labels for this timestep are identical, as it should be..
                            title3d(input$txtPLOT_TITLE, tmpdfTimeline$labelStep[1],'','','')
        
                            print(paste("    Exporting snapshot",tmpdfTimeline$labelStep[1],"..."))
                        }
                    
                        rgl.snapshot(FILE_RGL_SNAPSHOT) 
                    }else{
                        if(input$chkTIMELINE_RENDER_TO_FILE){
                            dev.off()
                        }
                    }
                    #Sys.sleep(1)
                }
                
                progress$set(message = 'Rendering Completed!', value = 1)
                Sys.sleep(2)
            
                btnRenderDynamicsSnapshotsValue <<- input$btnRenderDynamicsSnapshots
            })
        })


      	################################################
      	# Motifs
      	################################################
        observe({
            if(input$btnCalculateMotifs==0 || input$btnImportNetworks == 0 ||  LAYERS<=1)
                return()
            
            if(btnCalculateMotifsValue==input$btnCalculateMotifs) return()
    
            isolate({
                progress <- shiny::Progress$new(session)
                on.exit(progress$close())
                
                #to output full numbers
                options(scipen=999)
                
                progress$set(message = 'Setting up the algorithms...', value = 0.2)                
                inputFile <- paste(input$txtProjectName,"_fanmod.edges",sep="")
                
                #fanmod format assume 0-starting labeling for nodes, 1-starting for layers
                mergedEdgelist <- data.frame()
                layerLabels <- NULL
                for(l in 1:LAYERS){
                    mergedEdgelist <- rbind(mergedEdgelist, data.frame(from=layerEdges[[l]][,1]-1, to=layerEdges[[l]][,2]-1, layer=l))
                    layerLabels <- c(layerLabels, layerLabel[[l]])
                }
                write.table(file=inputFile, mergedEdgelist, row.names=F, col.names=F, quote=F)     
                resultFile <- paste(input$txtProjectName,"_fanmod.csv",sep="")
                
                progress$set(message = 'Calculating motifs...', value = 0.5)

                nullModelID <- 2
                if(input$selMotifNullModel=="Local const"){
                    nullModelID <- 2
                }else if(input$selMotifNullModel=="Global const"){
                    nullModelID <- 1
                }else if(input$selMotifNullModel=="No regard"){
                    nullModelID <- 0
                }
                
                exePath <- getExecutablePath("fanmod")
                exeFlags <- paste(as.numeric(input$selMotifSize),
                                              as.numeric(input$txtMotifSamples),
                                              1,
                                              inputFile,
                                              as.numeric(DIRECTED),
                                              0,
                                              1,
                                              nullModelID,
                                              0,
                                              1,
                                              0,
                                              as.numeric(input$txtMotifRandomNetworks),
                                              as.numeric(input$txtMotifRandomExchangePerEdges),
                                              as.numeric(input$txtMotifRandomExchangeAttempts),
                                              resultFile,
                                              0,
                                              0
                                    )
                
                #call fanmod
                #print( paste(exePath,exeFlags) )
                system(paste(exePath,exeFlags),intern=T)
                
                #read output. Here I could redirect the output inside the R environment.. but
                #for compatibility with the rest of the code I prefer to read a file
                #ID,Adj-Matrix,Frequency,Mean-Freq,Standard-Dev,Z-Score,p-Value
                motifsTable <- read.table(resultFile, header=T, sep=",", colClasses=c("character","character",rep("numeric",5)))
                
                progress$set(message = 'Rendering the results...', value = 0.8)
                
                #sorting
                if(input$selMotifResultsSortBy=="Frequency"){
                    motifsTable <- motifsTable[order(-motifsTable[,3]),]
                }else if(input$selMotifResultsSortBy=="Z-score"){
                    motifsTable <- motifsTable[order(-motifsTable[,6]),]
                }else if(input$selMotifResultsSortBy=="p-value"){
                    motifsTable <- motifsTable[order(motifsTable[,7]),]
                }
                
                #cutting
                if(input$chkMotifAbsZscore){
                    motifsTable <- motifsTable[ abs(motifsTable[,6])> as.numeric(input$txtMotifAbsZscore), ]
                }
                if(input$chkMotifPvalue){
                    motifsTable <- motifsTable[ abs(motifsTable[,7])< as.numeric(input$txtMotifPvalue), ]
                }
                if(input$chkMotifFrequency){
                    motifsTable <- motifsTable[ abs(motifsTable[,3])> as.numeric(input$txtMotifFrequency), ]
                }

                motifsTable$Mean.Freq <- NULL
                motifsTable$Standard.Dev <- NULL
                
                #creating figures
                rgb.palette <- colorRampPalette(brewer.pal(brewer.pal.info$maxcolors[row.names(brewer.pal.info)==input$selMotifColorPalette],input$selMotifColorPalette))(LAYERS)
                
                motifsTable$Motif <- rep("",nrow(motifsTable))
                for(r in 1:nrow(motifsTable)){
                    motif_name <- motifsTable[r,]$Adj.Matrix
                    outpng <- concatenatePath(concatenatePath(buildPath("www","img"),"motifs"), paste0(motif_name,".png"))
                    
                    png(outpng,width=128,height=128)
                    #print(motif_name)
                    #print(t(matrix(as.numeric(strsplit(motif_name,"")[[1]]),ncol=as.numeric(input$selMotifSize))))
                    g.motif <- graph.adjacency( t(matrix(as.numeric(strsplit(motif_name,"")[[1]]),ncol=as.numeric(input$selMotifSize))) )
                    E(g.motif)$color <- 1
                    g.motif <- simplify(g.motif,edge.attr.comb=list(color="sum"))
                    par(mar=c(0, 0, 0, 0), xaxs='i', yaxs='i') 
                    par(oma=c(0, 0, 0, 0))
                    plot(x=NULL, y=NULL, type="n",
                        xlim=c(-1,1), ylim=c(-1,1)
                        )

                    g.layout <- layout.circle(g.motif)
                    g.layout[,1] <- 0.95*(g.layout[,1] - min(g.layout[,1]))/(max(g.layout[,1])-min(g.layout[,1])) - 0.95/2
                    g.layout[,2] <- 0.95*(g.layout[,2] - min(g.layout[,2]))/(max(g.layout[,2])-min(g.layout[,2])) - 0.95/2
                    
                    plot(g.motif, layout=g.layout,
                        vertex.label="",
                        vertex.color="#A0A0A0",
                        vertex.frame.color="#A0A0A0",
                        edge.color=rgb.palette[E(g.motif)$color], 
                        edge.arrow.size=1, 
                        edge.arrow.width=1.5,
                        edge.width=3,
                        rescale=F, xlim=c(-1,1), ylim=c(-1,1)
                        )
                    dev.off()

                    motifsTable[r,]$Motif <- paste0("<img src='img/motifs/",motif_name,".png' width='128' height='128'>")
                }

                listMotifs <<- motifsTable
                listMotifs$Motif <<- NULL

                output$motifsColorLegend <- renderPlot({
                    mydf <- data.frame(layer=1:LAYERS, fake=1, color=rgb.palette)
                    p <- ggplot(mydf, aes(x=layer, y=fake, fill=layer)) + geom_tile() + 
                            scale_fill_gradientn(colours = rgb.palette) +  
                            ylab("") + 
                            xlab("") + 
                            scale_x_discrete(breaks=c(1:LAYERS), limits=c(1:LAYERS), labels=layerLabels) + 
                            scale_y_discrete(breaks=NULL, limits=c(0,1)) + 
                            guides(fill=FALSE) + 
                            theme_bw() + 
                            theme( plot.background = element_blank(),
                                panel.grid.major = element_blank() ,
                                panel.grid.minor = element_blank() ,
                                panel.border = element_blank() ,
                                panel.background = element_blank(),
                                axis.line = element_blank(),
                                text = element_text(size=25),
                                axis.text.x = element_text(angle=90, vjust=1)
                              )
                    print(p)
                }, height = 250)
                
                output$motifsGvisTable <- renderGvis({
                    gvisTable(motifsTable,options=googleVisMotifsSummaryTableOptions())
                })   
                
                #to reset output options
                options(scipen=0)
                progress$set(message = 'Calculation Completed!', value = 1)
                Sys.sleep(2)
            })
        })
        
                                    
      	################################################
      	# Calculate diagnostics
      	################################################

        observe({
            if(input$btnCalculateCorrelationDiagnostics==0 || input$btnImportNetworks == 0 ||  LAYERS==0)
                return()
            
            if(btnCalculateCorrelationDiagnosticsValue==input$btnCalculateCorrelationDiagnostics) return()
    
            isolate({
                progress <- shiny::Progress$new(session)
                on.exit(progress$close())
    
                #cell LAYERS+1 contains the data for the multiplex
    
                if(input$chkEXPORT_MATRIX_PLOT){
                    progress$set(message = 'Exporting matrices as images...', value = 0.05)
                    Sys.sleep(2)
                    for(l in 1:(LAYERS+1)){
                        if(l<=LAYERS){
                            outfilem <- buildPath("export",paste(input$txtProjectName,"_layer",l,".png",sep=""))
                            
                            progress$set(message = paste('Exporting image for layer ',l,'...',sep=""), value = 0.05 + 0.9*l/(LAYERS+1))  
                        }else{
                            outfilem <- buildPath("export",paste(input$txtProjectName,"_layer","aggr",".png",sep=""))
                        progress$set(message = paste('Exporting image for aggregated...',sep=""), value = 0.05 + 0.9*l/(LAYERS+1))  
                        }
    
                        #Build the plot 
                        #rgb.palette <- colorRampPalette(c("white", "blue", "red"), space = "rgb") 
                        rgb.palette <- colorRampPalette(brewer.pal(brewer.pal.info$maxcolors[row.names(brewer.pal.info)==input$selAssortativityTypeColorPalette],input$selAssortativityTypeColorPalette))
                        
                        png(outfilem, width=550, height=400)
                        myImagePlot(AdjMatrix[[l]], xLabels=rep("",Nodes), yLabels=rep("",Nodes), ColorRamp=rgb.palette(120)) #,title=c("")
                        #levelplot(AdjMatrix[[l]], main="", xlab="", ylab="", col.regions=rgb.palette(120), cuts=100, at=seq(0,1,0.01))
                        dev.off()
                    }
                    
                    progress$set(message = 'Exporting Completed!', value = 1)
                    Sys.sleep(1)
                }
                
                ###############
                ## Global
                ###############            
                
                if(input$chkMULTIPLEX_OVERLAPPING && LAYERS>1){
                    #create the config file for calling Octave's computation
                    createOctaveConfigFile()
                    progress$set(message = 'Calculating overlapping...', value = 0.05)
                    
                    #call octave
                    system("octave -qf octave/muxMultisliceOverlapping.m",intern=T)
                    
                    #read output. Here I could redirect the output inside the R environment.. but
                    #for compatibility with the rest of the code I prefer to read a file
                    resultFile <- paste(input$txtProjectName,"_overlapping.txt",sep="")
                    avgGlobalOverlapping <- paste(round(as.numeric(read.table(resultFile)[1,1])*100,3)," %")
                    if(file.exists(resultFile)) file.remove(resultFile)
                        
                    output$globalDiagnosticsOverlapping <- reactive({
                        if (input$btnCalculateCorrelationDiagnostics == 0 || input$btnImportNetworks == 0 || length(input$project_file)==0)
                            return(list())
    
                        #be careful: "=" is required instead of "<-"
                        return(list(
                            sumAvgGlobalOverlapping = as.character(avgGlobalOverlapping)
                        ))
                    })
                    
                    #call octave again
                    system("octave -qf octave/muxMultisliceOverlappingMatrix.m",intern=T)
                    
                    #read output.
                    resultFile <- paste(input$txtProjectName,"_overlapping_matrix.txt",sep="")
                    
                    Layer <- NULL
                    for(l in 1:LAYERS) Layer = c(Layer,as.character(layerLabel[[l]]))
    
                    overlapMatrix <- matrix(scan(resultFile, n = LAYERS*LAYERS), ncol=LAYERS, nrow=LAYERS, byrow = TRUE, dimnames=list(NULL, Layer))
                    if(file.exists(resultFile)) file.remove(resultFile)
    
                    outfile0 <- buildPath("tmp","image_overlap.png")
                    png(outfile0, width=550, height=400)
                    rgb.palette <- colorRampPalette(brewer.pal(brewer.pal.info$maxcolors[row.names(brewer.pal.info)==input$selAssortativityTypeColorPalette],input$selAssortativityTypeColorPalette))
                    myImagePlot(overlapMatrix,xLabels=Layer,yLabels=Layer,title=c("Mean overlapping"),ColorRamp=rgb.palette(120))
                #,zlim=c(-1,1)
                    dev.off()
    
                   output$overlappingSummaryImage <- renderImage({
                        list(src = outfile0,
                            contentType = 'image/png',
                            width = 550,
                            height = 400,
                            alt = "")
                          }, 
                          deleteFile = FALSE
                        )  
                    #if(file.exists(outfile0)) file.remove(outfile0)
    
                    overlapMatrix <- data.frame(overlapMatrix)
                    overlapMatrix <- cbind(data.frame(Layer),overlapMatrix)
                    listOverlap <<- overlapMatrix
    
                    output$overlappingSummaryTable <- renderGvis({
                        if(input$btnCalculateCorrelationDiagnostics==0 || input$btnImportNetworks == 0 || LAYERS==0)
                            return(NULL)
                        
                        gvisTable(overlapMatrix,options=googleVisOverlapMatrixSummaryTableOptions())
                    })   
                }
    
                if(input$chkMULTIPLEX_INTERASSORTATIVITY_PEARSON && LAYERS>1){
                    progress$set(message = 'Calculating Pearson...', value = 0.05)
                    
                    #create the config file for calling Octave's computation
                    createOctaveConfigFile()
    
                    #call octave
                    system("octave -qf octave/muxMultisliceInterAssortativityPearson.m",intern=T)
                    
                    #read output
                    resultFile <- paste(input$txtProjectName,"_interassortativity_pearson",input$selAssortativityType,".txt",sep="")
    
                    Layer <- NULL
                    for(l in 1:LAYERS) Layer = c(Layer,as.character(layerLabel[[l]]))
    
                    interPearson <- matrix(scan(resultFile, n = LAYERS*LAYERS), ncol=LAYERS, nrow=LAYERS, byrow = TRUE, dimnames=list(NULL, Layer))
                    if(file.exists(resultFile)) file.remove(resultFile)
    
                    outfile <- buildPath("tmp","image_pearson.png")
                    png(outfile, width=550, height=400)
                    rgb.palette <- colorRampPalette(brewer.pal(brewer.pal.info$maxcolors[row.names(brewer.pal.info)==input$selAssortativityTypeColorPalette],input$selAssortativityTypeColorPalette))

                    myImagePlot(interPearson,xLabels=Layer,yLabels=Layer,title=c("Inter-layer assortativity: Pearson"),ColorRamp=rgb.palette(120))
                #,zlim=c(-1,1)
                    dev.off()
    
                   output$interPearsonSummaryImage <- renderImage({
                        list(src = outfile,
                            contentType = 'image/png',
                            width = 550,
                            height = 400,
                            alt = "")
                          }, 
                          deleteFile = FALSE
                        )
                    #if(file.exists(outfile)) file.remove(outfile)
    
                    interPearson <- data.frame(interPearson)
                    interPearson <- cbind(data.frame(Layer),interPearson)
                    listInterPearson <<- interPearson
                    
                    output$interPearsonSummaryTable <- renderGvis({
                        if(input$btnCalculateCorrelationDiagnostics==0 || input$btnImportNetworks == 0 || LAYERS==0)
                            return(NULL)
                        
                        gvisTable(interPearson,options=googleVisInterPearsonSummaryTableOptions())
                    })   
                }
    
                if(input$chkMULTIPLEX_INTERASSORTATIVITY_SPEARMAN && LAYERS>1){
                    progress$set(message = 'Calculating Spearman...', value = 0.05)
                    
                    #create the config file for calling Octave's computation
                    createOctaveConfigFile()
    
                    #call octave
                    system("octave -qf octave/muxMultisliceInterAssortativitySpearman.m",intern=T)
                    
                    #read output
                    resultFile <- paste(input$txtProjectName,"_interassortativity_spearman",input$selAssortativityType,".txt",sep="")
    
                    Layer <- NULL
                    for(l in 1:LAYERS) Layer = c(Layer,as.character(layerLabel[[l]]))
    
                    interSpearman <- matrix(scan(resultFile, n = LAYERS*LAYERS), ncol=LAYERS, nrow=LAYERS, byrow = TRUE, dimnames=list(NULL, Layer))
                    if(file.exists(resultFile)) file.remove(resultFile)
    
                    outfile2 <- buildPath("tmp","image_spearman.png")
                    png(outfile2, width=550, height=400)
                    rgb.palette <- colorRampPalette(brewer.pal(brewer.pal.info$maxcolors[row.names(brewer.pal.info)==input$selAssortativityTypeColorPalette],input$selAssortativityTypeColorPalette))
                    myImagePlot(interSpearman,xLabels=Layer,yLabels=Layer,title=c("Inter-layer assortativity: Spearman"),ColorRamp=rgb.palette(120))
                #,zlim=c(-1,1)
                    dev.off()
    
                    output$interSpearmanSummaryImage <- renderImage({
                        list(src = outfile2,
                            contentType = 'image/png',
                            width = 550,
                            height = 400,
                            alt = "")
                          }, 
                          deleteFile = FALSE
                        )
                    #if(file.exists(outfile2)) file.remove(outfile2)
                    
                    interSpearman <- data.frame(interSpearman)
                    interSpearman <- cbind(data.frame(Layer),interSpearman)
                    listInterSpearman <<- interSpearman
                    
                    output$interSpearmanSummaryTable <- renderGvis({
                        if(input$btnCalculateCorrelationDiagnostics==0 || input$btnImportNetworks == 0 || LAYERS==0)
                            return(NULL)
                        
                        gvisTable(interSpearman,options=googleVisInterSpearmanSummaryTableOptions())
                    })   
                }            

                btnCalculateCorrelationDiagnosticsValue <<- input$btnCalculateCorrelationDiagnostics
                progress$set(message = 'Correlation Diagnostics Completed!', value = 1)
                Sys.sleep(2)

            })
        })
        
        observe({
            if(input$btnCalculateCentralityDiagnostics==0 || input$btnImportNetworks == 0 ||  LAYERS==0)
                return()
            
            if(btnCalculateCentralityDiagnosticsValue==input$btnCalculateCentralityDiagnostics) return()
    
            isolate({
                progress <- shiny::Progress$new(session)
                on.exit(progress$close())
                   
                ###############
                ## Centrality
                ###############
                if(input$chkNODE_CENTRALITY_STRENGTH || input$chkNODE_CENTRALITY_PAGERANK || input$chkNODE_CENTRALITY_EIGENVECTOR || input$chkNODE_CENTRALITY_HUB || input$chkNODE_CENTRALITY_AUTHORITY || input$chkNODE_CENTRALITY_KATZ || input$chkNODE_CENTRALITY_KCORE || input$chkNODE_CENTRALITY_MULTIPLEXITY){
    
                    progress$set(message = 'Calculating centrality...', value = 0.05)
                    listDiagnostics <<- NULL
                    diagnosticsOK <<- T
                    listDiagnosticsMerge <<- NULL
                    
                    if(input$chkNODE_CENTRALITY_MULTIPLEX){
                        #calculation in the multiplex. For the moment the output is obtained calling octave.
                        #the output will be stored in [[l]] for the multiplex and [[LAYERS+1]] for the aggregated.
                        listDiagnostics <<- GetCentralityDataFrameArray("Multiplex") 
                        diagnosticsMultiplexOK <<- T
                    }else{
                        #calculation per layer. No needs to specify the weight attribute because the g objects
                        #are built assuming weighted input (where weight is 1 for binary networks), and each measure
                        #assume by default the weight attribute of E(g)
                        
                        listDiagnostics <<- GetCentralityDataFrameArray("SingleLayer")
                        diagnosticsSingleLayerOK <<- T
                    }
    
                    for(l in 1:(LAYERS+1)){
                        listDiagnosticsMerge <<- rbind(listDiagnosticsMerge,listDiagnostics[[l]])
                    }                            
    
                    progress$set(message = 'Creating tables...', value = 0.95)
                    Sys.sleep(1)
                    
                    #Fill the table summarizing centrality 
                    output$centralityTable <- renderGvis({
                        if(input$btnCalculateCentralityDiagnostics==0 || input$btnImportNetworks == 0 || LAYERS==0)
                            return(NULL)
                        
                        return(gvisTable(listDiagnosticsMerge,options=list(page='enable',pageSize=Nodes)))
                        #googleVisCentralityTableOptions()))
                    })   
                }
                
                btnCalculateCentralityDiagnosticsValue <<- input$btnCalculateCentralityDiagnostics
                
                progress$set(message = 'Centrality Completed!', value = 1)
                Sys.sleep(2)
            })
        })
        
        observe({
            if(input$btnCalculateCommunityDiagnostics==0 || input$btnImportNetworks == 0 ||  LAYERS==0)
                return()
            
            if(btnCalculateCommunityDiagnosticsValue==input$btnCalculateCommunityDiagnostics) return()
    
            isolate({
                progress <- shiny::Progress$new(session)
                on.exit(progress$close())
                
                ###############
                ## Community
                ###############
                #if(input$chkPERFORM_COMMUNITY_DETECTION){
                    progress$set(message = 'Calculating community structure...', value = 0.05)
                    print("  Detecting communities...")    
                    listCommunities <<- NULL
                    sumCommunities <- NULL
                    listCommunitiesMerge <<- NULL
                    sumCommunitiesMerge <<- NULL
                    communityOK <<- T
    
                    if(input$radCommunityAlgorithm=="COMMUNITY_MULTIPLEX"){
                        #calculation in the multiplex. For the moment the output is obtained calling octave.
                        #the output will be stored in [[l]] for the multiplex and [[LAYERS+1]] for the aggregated.
    
                        for(l in 1:LAYERS){
                            listCommunities[[l]] <<- data.frame(Layer = rep(paste(l,"Multi",sep="-"),Nodes))
                            listCommunities[[l]] <<- cbind(listCommunities[[l]],data.frame(Node = 1:Nodes))
                            listCommunities[[l]] <<- cbind(listCommunities[[l]],data.frame(Label=nodesLabel[[l]]))
                        }
                        l <- LAYERS+1
                        listCommunities[[l]] <<- data.frame(Layer = rep("Aggr",Nodes))
                        listCommunities[[l]] <<- cbind(listCommunities[[l]],data.frame(Node = 1:Nodes))
                        listCommunities[[l]] <<- cbind(listCommunities[[l]],data.frame(Label=nodesLabel[[l]]))
                        
                        sumCommunities[[1]] <- data.frame(Layer = "Multi")
                        sumCommunities[[2]] <- data.frame(Layer = "Aggr")
    
                        createOctaveConfigFile()
                        #call octave
                        system("octave -qf octave/muxMultisliceCommunity.m",intern=T)
                        #read output.
                        resultFile <- paste(input$txtProjectName,"_community_membership.txt",sep="")
                        wmemb_membership <- matrix(scan(resultFile, n = Nodes*LAYERS), ncol=LAYERS, nrow=Nodes, byrow = TRUE)   
                        if(file.exists(resultFile)) file.remove(resultFile)             
                        resultFile <- paste(input$txtProjectName,"_community_modularity.txt",sep="")
                        wtmod <- as.numeric(read.table(resultFile)[1,1])
                        if(file.exists(resultFile)) file.remove(resultFile)
    
                        print(paste("  Modularity: ",wtmod))
                        maxCom <- max(wmemb_membership)
                        numComms <- maxCom
    
                        resultFile <- paste(input$txtProjectName,"_community_membership_aggregate.txt",sep="")
                        wmemb_membership_aggregate <- matrix(scan(resultFile, n = Nodes), ncol=1, nrow=Nodes, byrow = TRUE)        
                        if(file.exists(resultFile)) file.remove(resultFile)        
                        resultFile <- paste(input$txtProjectName,"_community_modularity_aggregate.txt",sep="")
                        wtmod_aggregate <- as.numeric(read.table(resultFile)[1,1])
                        if(file.exists(resultFile)) file.remove(resultFile)
    
                        print(paste("  Modularity aggregate: ",wtmod_aggregate))
                        maxComAggr <- max(wmemb_membership_aggregate)
                        numCommsAggr <- maxComAggr
                            
                        #eventual community merging, if any, here.
    
                        for(l in 1:LAYERS){                                                                        
                            listCommunities[[l]] <<- cbind(listCommunities[[l]],data.frame(Community=wmemb_membership[,l]))
                            listCommunitiesMerge <<- rbind(listCommunitiesMerge,listCommunities[[l]])
                        }
                        listCommunities[[LAYERS+1]] <<- cbind(listCommunities[[LAYERS+1]],data.frame(Community=wmemb_membership_aggregate))
                        listCommunitiesMerge <<- rbind(listCommunitiesMerge,listCommunities[[LAYERS+1]])
                        #print(listCommunities)
    
                        #Multiplex
                        l <- 1
                        sumCommunities[[l]] <- cbind(sumCommunities[[l]],data.frame(Communities = numComms))                        
                        sumCommunities[[l]] <- cbind(sumCommunities[[l]],data.frame(Modularity = round(wtmod,3)))
                        sumCommunitiesMerge <<- rbind(sumCommunitiesMerge,sumCommunities[[l]])
                        #Aggregate: change numcoms and modularity here
                        l <- 2
                        sumCommunities[[l]] <- cbind(sumCommunities[[l]],data.frame(Communities = numCommsAggr))                        
                        sumCommunities[[l]] <- cbind(sumCommunities[[l]],data.frame(Modularity = round(wtmod_aggregate,3)))
                        sumCommunitiesMerge <<- rbind(sumCommunitiesMerge,sumCommunities[[l]])
    
                        #print(listCommunitiesMerge)
                    }else{                    
                        #calculation per layer. No needs to specify the weight attribute because the g objects
                        #are built assuming weighted input (where weight is 1 for binary networks), and each measure
                        #assume by default the weight attribute of E(g)
                        for(l in 1:(LAYERS)){
                            listCommunities[[l]] <<- data.frame(Layer = rep(l,Nodes))
                            listCommunities[[l]] <<- cbind(listCommunities[[l]],data.frame(Node = 1:Nodes))
                            listCommunities[[l]] <<- cbind(listCommunities[[l]],data.frame(Label=nodesLabel[[l]]))
    
                            sumCommunities[[l]] <- data.frame(Layer = as.character(l))
                        }
    
                        listCommunities[[LAYERS+1]] <<- data.frame(Layer = rep("Aggr",Nodes))
                        listCommunities[[LAYERS+1]] <<- cbind(listCommunities[[LAYERS+1]],data.frame(Node = 1:Nodes))
                        listCommunities[[LAYERS+1]] <<- cbind(listCommunities[[LAYERS+1]],data.frame(Label=nodesLabel[[LAYERS+1]]))
    
                        sumCommunities[[LAYERS+1]] <- data.frame(Layer = "Aggr")
                        
                        for(l in 1:(LAYERS+1)){
                            if(input$radCommunityAlgorithm=="COMMUNITY_LOUVAIN"){
                                #i need this trick to obtain a wmemb structure for this algorithm
                                wt <- walktrap.community(g[[l]],modularity=TRUE)
                                wmemb <- community.to.membership(g[[l]], wt$merges,steps=which.max(wt$modularity)-1)

                                wt <- multilevel.community(as.undirected(g[[l]]))
                                wmemb$membership <- membership(wt) - 1
                                comList <- communities(wt)
                                wmemb$csize <- numeric(length(comList))
                                for(com in 1:length(wmemb$csize)){
                                    wmemb$csize[com] <- length(comList[[com]])
                                }                            
                            }

                            if(input$radCommunityAlgorithm=="COMMUNITY_INFOMAP"){
                                #i need this trick to obtain a wmemb structure for this algorithm
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
                            
                            if(input$radCommunityAlgorithm=="COMMUNITY_RANDOM_WALK_TRAP"){
                                wt <- walktrap.community(g[[l]],modularity=TRUE)
                                wmemb <- community.to.membership(g[[l]], wt$merges,steps=which.max(wt$modularity)-1)                            
                            }
                            
                            if(input$radCommunityAlgorithm=="COMMUNITY_EDGE_BETWEENNESS"){
                                wt <- edge.betweenness.community(g[[l]],modularity=TRUE)
                                wmemb <- community.to.membership(g[[l]], wt$merges,steps=which.max(wt$modularity)-1)
                            }
                            
                            print(paste("  Modularity: ",modularity(wt)))
                            maxCom <- max(wmemb$membership) + 1
                            numComms <- maxCom
                            
                            if(as.numeric(input$txtCOMMUNITY_MIN_SIZE)>0){
                                #Merge community smaller than chosen resolution to a unique community
                                #This will improve the coloring scheme when there are many isoloted nodes/components
                                mergedNodes <- 0
                                for(n in 1:length(wmemb$membership)){
                                    if( wmemb$csize[ wmemb$membership[n]+1 ] <= as.numeric(input$txtCOMMUNITY_MIN_SIZE) ){
                                        wmemb$membership[n]  <- -1
                                        mergedNodes <- mergedNodes + 1
                                    }
                                }
                        
                                print(paste("  There are", mergedNodes, "nodes in communities smaller than",as.numeric(input$txtCOMMUNITY_MIN_SIZE)))
                                print("  Merging...")
                                    
                                maxCom <- max(wmemb$membership) + 1
                                mergeComID <-  maxCom + 1
                                wmemb$membership[wmemb$membership==-1] <- mergeComID
                                wmemb$csize[mergeComID] <- mergedNodes
                                wmemb$csize <- wmemb$csize[1:mergeComID]
                            }
                            
                            print(paste("  Communities with >",as.numeric(input$txtCOMMUNITY_MIN_SIZE),"nodes:",maxCom-1))
                            
                            listCommunities[[l]] <<- cbind(listCommunities[[l]],data.frame(Community=(1+wmemb$membership)))
    
                            sumCommunities[[l]] <- cbind(sumCommunities[[l]],data.frame(Communities = numComms))                        
                            sumCommunities[[l]] <- cbind(sumCommunities[[l]],data.frame(Modularity = round(modularity(wt),3)))
                        }
    
                        for(l in 1:(LAYERS+1)){
                            listCommunitiesMerge <<- rbind(listCommunitiesMerge,listCommunities[[l]])
                            sumCommunitiesMerge <<- rbind(sumCommunitiesMerge,sumCommunities[[l]])
                        }
                        #print(listCommunitiesMerge)
                    }
                                    
                    progress$set(message = 'Creating tables...', value = 0.95)
                    Sys.sleep(1)


                    matComm <- matrix(nrow=LAYERS, ncol=Nodes, 0)
                    Layer <- NULL
                    for(l in 1:LAYERS) Layer = c(Layer,as.character(layerLabel[[l]]))
                    for(l in 1:(LAYERS)){
                        matComm[l,] <- listCommunities[[l]]$Community
                    }
                    outfile <- buildPath("tmp","image_comms.png")
                    png(outfile, width=600, height=400)
                    rgb.palette <- colorRampPalette(brewer.pal(brewer.pal.info$maxcolors[row.names(brewer.pal.info)==input$selCommunityColorPalette],input$selCommunityColorPalette))(max(matComm))
                    heatmap.2(matComm,labRow=Layer,labCol=1:Nodes,cexRow=200/Nodes,cexCol=40/Nodes,col=rgb.palette,symm=F,dendrogram="none",trace="none",offsetCol=-0.4,offsetRow=-0.4)
                    #myImagePlot(matComm,xLabels=Nodes,yLabels=Layer,title=c("Community Membership"),ColorRamp=rgb.palette)
                #,zlim=c(-1,1)
                    dev.off()
    
                   output$matCommunitySummaryImage <- renderImage({
                        list(src = outfile,
                            contentType = 'image/png',
                            width = 600,
                            height = 400,
                            alt = "")
                          }, 
                          deleteFile = FALSE
                        )



                    #Fill the table summarizing the community
                    output$communityTable <- renderGvis({
                        if(input$btnCalculateCommunityDiagnostics==0 || input$btnImportNetworks == 0 || LAYERS==0)
                            return(NULL)
                        
                        gvisTable(listCommunitiesMerge,options=list(page='enable',pageSize=Nodes))
                        #googleVisCommunityTableOptions())
                    })
                    
                    #Fill the table summarizing the community
                    output$communitySummaryTable <- renderGvis({
                        if(input$btnCalculateCommunityDiagnostics==0 || input$btnImportNetworks == 0 || LAYERS==0)
                            return(NULL)
                        
                        gvisTable(sumCommunitiesMerge,options=list(page='enable',pageSize=Nodes))
                        #googleVisCommunitySummaryTableOptions())
                    })
                #}
                
                btnCalculateCommunityDiagnosticsValue <<- input$btnCalculateCommunityDiagnostics
    
                progress$set(message = 'Community Detection Completed!', value = 1)
                Sys.sleep(2)
            })  
        })
    
      	################################################
      	# Annular Visualization
      	################################################
    
        observe({
            if(input$btnCalculateCentralityDiagnostics==0 || input$btnImportNetworks == 0 || input$btnAnularViz==0 ||LAYERS==0)
                return()
            
            if(btnAnularVizValue==input$btnAnularViz) return()
        
            isolate({
                progress <- shiny::Progress$new(session)
                on.exit(progress$close())
                muxFeatureDataFrame <- NULL
                monoxFeatureDataFrame <- NULL
                muxFeatureDataFrameArray <- NULL
                monoxFeatureDataFrameArray <- NULL

                if(length(listDiagnosticsSingleLayer)>0 && length(listDiagnostics)>0){
                    for( attrib in colnames(listDiagnostics[[1]]) ){
                        if( (attrib!="Node" && attrib!="Label" && attrib!="Layer") ){
                            if( all(listDiagnosticsSingleLayer[[1]][,attrib]==rep("-",Nodes)) && all(listDiagnostics[[1]][,attrib]!=rep("-",Nodes)) ){
                                diagnosticsSingleLayerOK <<- FALSE
                                break
                            }
                        }
                    }
                }

                if(!diagnosticsSingleLayerOK){
                    print("  Calculation of single-layer descriptors...")
                    listDiagnosticsSingleLayer <<- GetCentralityDataFrameArray("SingleLayer")
                    diagnosticsSingleLayerOK <<- TRUE
                }
                monoxFeatureDataFrameArray <- listDiagnosticsSingleLayer
                #print(monoxFeatureDataFrameArray)

                if(!diagnosticsMultiplexOK){
                    print("  Calculation of multiplex descriptors...")
                    listDiagnostics <<- GetCentralityDataFrameArray("Multiplex")
                    diagnosticsMultiplexOK <<- TRUE
                }
                muxFeatureDataFrameArray <- listDiagnostics
                                
                ####################################
                #Multiplex viz: each ring is a feature
                ####################################
                
                print("  New set: each ring represents a diagnostics")
                
                #we can use just the l = 1 data for the multiplex and l = LAYERS+1 for the aggregate
                
                numFeatures <- length(attributes(muxFeatureDataFrameArray[[1]])$names)
    
                progress$set(message = 'Creating tables...', value = 0.25)
                Bins <- as.numeric(input$txtANULAR_VIZ_BINS)
                
                k <- 1
                sortByFeatureID <- 1
                
                for( attrib in attributes(muxFeatureDataFrameArray[[1]])$names ){
                    if( (attrib!="Node" && attrib!="Label" && attrib!="Layer") && length(unique(muxFeatureDataFrameArray[[1]][,attrib]))>1 ){
                        print(paste("  Processing",attrib,"..."))
                        if(attrib==input$selAnularVizInputFeatureID){
                            print(paste("    Ordering by",attrib))
                            sortByFeatureID <- k
                        }
                        
                        #rescale feature
                        x <- muxFeatureDataFrameArray[[1]][,attrib]
                        x <- x + 1e-3

                        if(!input$chkANULAR_VIZ_LOG){
                            if(max(x,na.rm=T) != min(x,na.rm=T)){
                                x <- floor( 1 + Bins * (x - min(x,na.rm=T))/(max(x,na.rm=T) - min(x,na.rm=T)) )
                            }else{
                                x <- rep(1,Nodes)
                            }
                        }else{
                            if(max(x,na.rm=T) != min(x,na.rm=T)){
                                x <- floor( 1 + Bins * (log(x) - log(min(x,na.rm=T)))/(log(max(x,na.rm=T)) - log(min(x,na.rm=T))) )
                            }else{
                                x <- rep(1,Nodes)
                            }
                        }
                        
                        muxFeatureDataFrame <- rbind(
                                                                    muxFeatureDataFrame,
                                                                    data.frame(
                                                                        feature=rep(k,Nodes),  
                                                                        node=muxFeatureDataFrameArray[[1]][,"Node"],
                                                                        cluster=x,
                                                                        featurelabel=attrib
                                                                        )
                                                                    )
                        k <- k + 1
                    }
                }                            
    
                if(input$radAnularVizCorrelationMethod=="ANULAR_VIZ_CORRELATION_SPEARMAN"){
                    CorrelationMethod <- "spearman"
                }else if(input$radAnularVizCorrelationMethod=="ANULAR_VIZ_CORRELATION_PEARSON"){
                    CorrelationMethod <- "pearson"
                }else if(input$radAnularVizCorrelationMethod=="ANULAR_VIZ_CORRELATION_JSD"){                
                    CorrelationMethod <- "jsd"
                }
                
                if( input$chkANULAR_VIZ_CELL_BORDER==0 ) Border <- NA
                                
                progress$set(message = 'Creating figures...', value = 0.5)
                outfileX <- buildPath("tmp","image_annular_multiplex.png")
                
                png(outfileX, width=1200, height=1200)
    
                plotAnularViz(
                            muxFeatureDataFrame,
                            as.numeric(input$txtANULAR_VIZ_RCORE),
                            as.numeric(input$txtANULAR_VIZ_RING_DISPLACEMENT),
                            Border,
                            sortByFeatureID,
                            CorrelationMethod, 
                            input$chkANULAR_VIZ_SHOW_NODE_LABEL,
                            "Multiplex"
                            )
    
                dev.off()
    
                output$anularVizSummaryMuxImage <- renderImage({
                    list(src = outfileX,
                        contentType = 'image/png',
                        width = 550,
                        height = 550,
                        alt = "")
                        }, 
                        deleteFile = FALSE
                    )
                #if(file.exists(outfile2)) file.remove(outfile2)
    
    
                ####################################
                #Layer viz: each ring is a layer, each plot is for a single descriptor
                ####################################
                
                print("  New set: each ring represents a layer")
                
                # l = LAYERS+1 is for the aggregate
    
                progress$set(message = 'Creating tables...', value = 0.75)
                max_plots <- 0
                for( attrib in attributes(monoxFeatureDataFrameArray[[1]])$names ){
                    if( (attrib!="Node" && attrib!="Label" && attrib!="Layer") && length(unique(monoxFeatureDataFrameArray[[1]][,attrib]))>1 ){
                        print(paste("  Processing",attrib,"..."))
                        monoxFeatureDataFrame <- NULL
                        max_plots <- max_plots + 1
                        
                        #we have a feature, let's read the values for each layer
                        for(l in 1:(LAYERS+1)){
                            #rescale feature

                            x <- monoxFeatureDataFrameArray[[l]][,attrib]
                            x <- x + 1e-3

                            if(!input$chkANULAR_VIZ_LOG){
                                if(max(x,na.rm=T) != min(x,na.rm=T)){
                                    x <- floor( 1 + Bins * (x - min(x,na.rm=T))/(max(x,na.rm=T) - min(x,na.rm=T)) )
                                }else{
                                    x <- rep(1,Nodes)
                                }
                            }else{
                                if(max(x,na.rm=T) != min(x,na.rm=T)){
                                    x <- floor( 1 + Bins * (log(x) - log(min(x,na.rm=T)))/(log(max(x,na.rm=T)) - log(min(x,na.rm=T))) )
                                }else{
                                    x <- rep(1,Nodes)
                                }
                            }
                        
                            if(l<LAYERS+1){
                                thislabel <- layerLabel[[l]]#paste("Layer",l)
                            }else{
                                thislabel <- "Aggregate"
                            }
                        
                            monoxFeatureDataFrame <- rbind(
                                                                        monoxFeatureDataFrame,
                                                                        data.frame(
                                                                            feature=rep(l,Nodes),  
                                                                            node=monoxFeatureDataFrameArray[[1]][,"Node"],
                                                                            cluster=x,
                                                                            featurelabel=thislabel
                                                                            )
                                                                        )
                        }
                        
                        #add the values from the multiplex
                        #rescale feature
                        x <- muxFeatureDataFrameArray[[1]][,attrib]
                        x <- x + 1e-3
            
                        if(!input$chkANULAR_VIZ_LOG){
                            if(max(x,na.rm=T) != min(x,na.rm=T)){
                                x <- floor( 1 + Bins * (x - min(x,na.rm=T))/(max(x,na.rm=T) - min(x,na.rm=T)) )
                            }else{
                                x <- rep(1,Nodes)
                            }
                        }else{
                            if(max(x,na.rm=T) != min(x,na.rm=T)){
                                x <- floor( 1 + Bins * (log(x) - log(min(x,na.rm=T)))/(log(max(x,na.rm=T)) - log(min(x,na.rm=T))) )
                            }else{
                                x <- rep(1,Nodes)
                            }
                        }
                    
                        thislabel <- "Multiplex"
                    
                        monoxFeatureDataFrame <- rbind(
                                                                    monoxFeatureDataFrame,
                                                                    data.frame(
                                                                        feature=rep(LAYERS+2,Nodes),  
                                                                        node=monoxFeatureDataFrameArray[[1]][,"Node"],
                                                                        cluster=x,
                                                                        featurelabel=thislabel
                                                                        )
                                                                    )                    
                        
                        #print(monoxFeatureDataFrame)
                        
                        #build the plot                    
                        progress$set(message = 'Creating figures...', value = 0.9)
                        outfileY <- buildPath("tmp",paste("image_annular_",attrib,".png",sep=""))
                        
                        png(outfileY, width=1200, height=1200)
                        
                        Title <- attrib
                        if(input$selAnularVizInputLayerID=="Multiplex"){
                            sortByLayerID <- LAYERS+2
                            print(paste("    Ordering by","Multiplex"))
                        }else if(input$selAnularVizInputLayerID=="Aggregate"){
                            sortByLayerID <- LAYERS+1
                            print(paste("    Ordering by","Aggregate"))
                        }else if(input$selAnularVizInputLayerID=="Max entropy"){
                            sortByLayerID <- 0
                            print(paste("    Ordering by","Max entropy"))
                        }else{
                            #it is by one of the layers
                            sortByLayerID <- strsplit(input$selAnularVizInputLayerID," ")[[1]][2]
                            print(paste("    Ordering by layer",sortByLayerID))
                        }
                        
                        plotAnularViz(
                                    monoxFeatureDataFrame,
                                    as.numeric(input$txtANULAR_VIZ_RCORE),
                                    as.numeric(input$txtANULAR_VIZ_RING_DISPLACEMENT),
                                    Border,
                                    sortByLayerID,
                                    CorrelationMethod, 
                                    input$chkANULAR_VIZ_SHOW_NODE_LABEL,
                                    Title
                                    )
            
                        dev.off()
                    }
                } 
                
                #http://stackoverflow.com/questions/15875786/dynamically-add-plots-to-web-page-using-shiny
                output$outputAnularVizImages <- renderUI({                        
                    plot_output_list <- lapply(1:max_plots, function(i) {
                        plotname <- paste("plot", i, sep="")
                        imageOutput(plotname,width = "100%", height = "100%")
                    })
                    
                    # Convert the list to a tagList - this is necessary for the list of items
                    # to display properly.
                    do.call(tagList, plot_output_list)
                })
    
                i <- 1
                for( attrib in attributes(monoxFeatureDataFrameArray[[1]])$names ){
                    if( (attrib!="Node" && attrib!="Label" && attrib!="Layer") && length(unique(monoxFeatureDataFrameArray[[1]][,attrib]))>1 ){
                        local({
                            #print(i)
                            my_i <- i
                            outfileY <- buildPath("tmp",paste("image_annular_",attrib,".png",sep=""))
                            plotname <- paste("plot", my_i, sep="")
                            
                            output[[plotname]] <- renderImage({
                                list(src = outfileY,
                                    contentType = 'image/png',
                                    width = 550,
                                    height = 550,
                                    alt = "")
                                    }, 
                                    deleteFile = FALSE
                                )
                        })
                        i <- i + 1
                    }
                }
                
                #output$anularVizSummaryDescriptorImage <- renderImage({
                #    list(src = outfileY,
                #        contentType = 'image/png',
                #        width = 550,
                #        height = 550,
                #        alt = "")
                #        }, 
                #        deleteFile = FALSE
                #    )
                #if(file.exists(outfile2)) file.remove(outfile2)
    
                ###############################################
                    
                btnAnularVizValue <<- input$btnAnularViz
    
                progress$set(message = 'Annular Viz Completed!', value = 1)
                Sys.sleep(2)
            })  
        })
    
      	################################################
      	# Diagnostics centralty plots
      	################################################

        observe({
            if(input$btnCalculateCentralityDiagnostics == 0 || input$btnCentralityDiagnosticsAnalysis ==0 || input$btnImportNetworks == 0 ||  LAYERS==0)
                return()
            
            if(btnCentralityDiagnosticsAnalysisValue==input$btnCentralityDiagnosticsAnalysis) return()
    
            isolate({
                progress <- shiny::Progress$new(session)
                on.exit(progress$close())
    
                progress$set(message = 'Calculating...', value = 0.05)

                this.descriptor <- input$selDiagnosticsCentralityVizID

                featureDataFrameList <- NULL
                layersToInclude <- as.numeric(strsplit(input$txtDiagnosticsCentralityStructureLayer,",")[[1]])

                if(length(listDiagnosticsSingleLayer)>0 && length(listDiagnostics)>0){
                    for( attrib in colnames(listDiagnostics[[1]]) ){

                        if( (attrib!="Node" && attrib!="Label" && attrib!="Layer") ){
                            if( all(listDiagnosticsSingleLayer[[1]][,attrib]==rep("-",Nodes)) && all(listDiagnostics[[1]][,attrib]!=rep("-",Nodes)) ){
                                print(listDiagnosticsSingleLayer[[1]][,attrib]) 
                                print(listDiagnostics[[1]][,attrib])
                                diagnosticsSingleLayerOK <<- FALSE
                                break
                            }
                        }
                    }
                }

                if(input$chkCentralityAnalysisStructureMultiplex){
                    if(!diagnosticsMultiplexOK){
                        print("  Calculation of multiplex descriptors...")
                        listDiagnostics <<- GetCentralityDataFrameArray("Multiplex")
                        diagnosticsMultiplexOK <<- TRUE
                    }
                    #same for all layers, just pick the 1
                    featureDataFrameList[["Multi"]] <- listDiagnostics[[1]]
                }

                if(input$chkCentralityAnalysisStructureLayer){
                    if(!diagnosticsSingleLayerOK){
                        print("  Calculation of single-layer descriptors...")
                        listDiagnosticsSingleLayer <<- GetCentralityDataFrameArray("SingleLayer")
                        diagnosticsSingleLayerOK <<- TRUE
                    }
                    
                    for(l in layersToInclude){
                        featureDataFrameList[[ paste0("L",l) ]] <- listDiagnosticsSingleLayer[[ l ]]
                    }
                }
    
                if(input$chkCentralityAnalysisStructureAggregate){
                    if(!diagnosticsMultiplexOK){
                        print("  Calculation of multiplex descriptors...")
                        listDiagnostics <<- GetCentralityDataFrameArray("Multiplex")
                        diagnosticsMultiplexOK <<- TRUE
                    }
                    featureDataFrameList[["Aggr"]] <- listDiagnostics[[LAYERS+1]]
                }
                
                if(input$radDiagnosticsCentralityType=="DIAGNOSTICS_ANALYSIS_TOPRANKED"){
                    orderingIdx <- 1:Nodes
                    X <- data.frame()
                    Bins <- as.numeric(input$txtDiagnosticsCentralityTopRankedBins)

                    if(input$chkCentralityAnalysisStructureMultiplex){
                        v.x <- as.numeric(featureDataFrameList[["Multi"]][this.descriptor][,1])
                        if(input$centralityAnalysisTopRankedLog){
                            v.x <- log10(v.x)
                            if( any(is.infinite(v.x)) ){
                                v.x[is.infinite(v.x)] <- 0
                            }
                        }
                        
                        ordering <- sort(v.x, decreasing=T, index.return=T)
                        v.x <- ordering$x
                        orderingIdx <- ordering$ix
                        X <- rbind(X, data.frame(Node=nodesLabel[[1]][orderingIdx[1:Bins]], Var1=v.x[1:Bins], Type="Multilayer"))
                    }
                    if(input$chkCentralityAnalysisStructureAggregate){
                        v.x <- as.numeric(featureDataFrameList[["Aggr"]][this.descriptor][,1])
                        if(input$centralityAnalysisTopRankedLog){
                            v.x <- log10(v.x)
                            if( any(is.infinite(v.x)) ){
                                v.x[is.infinite(v.x)] <- 0
                            }
                        }

                        v.x <- v.x[orderingIdx]
                        X <- rbind(X, data.frame(Node=nodesLabel[[1]][orderingIdx[1:Bins]], Var1=v.x[1:Bins], Type="Aggregate"))
                    }                    
                    if(input$chkCentralityAnalysisStructureLayer){
                        for(l in layersToInclude){
                            v.x <- as.numeric(featureDataFrameList[[ paste0("L",l) ]][this.descriptor][,1])
                            if(input$centralityAnalysisTopRankedLog){
                                v.x <- log10(v.x)
                                if( any(is.infinite(v.x)) ){
                                    v.x[is.infinite(v.x)] <- 0
                                }
                            }
                            v.x <- v.x[orderingIdx]
                            X <- rbind(X, data.frame(Node=nodesLabel[[1]][orderingIdx[1:Bins]], Var1=v.x[1:Bins], Type=layerLabel[[l]]))
                        }
                    }                    
                    
                    #print(X)

                    output$centralityAnalysisPlot <- renderChart2({
                        rplot <- nPlot(Var1 ~ Node, 
                                 group = "Type",
                                 data = X, type = "multiBarHorizontalChart")

                        rplot$xAxis(axisLabel="Node")

                        if(input$centralityAnalysisTopRankedLog){
                            rplot$yAxis(axisLabel=paste0("log10 ", this.descriptor))
                        }else{
                            rplot$yAxis(axisLabel=this.descriptor)
                        }
                        
                        #rplot$chart(reduceXTicks = FALSE)
                        #rplot$xAxis(staggerLabels = TRUE)

                        return(rplot)
                    })
                }
                
                if(input$radDiagnosticsCentralityType=="DIAGNOSTICS_ANALYSIS_DISTRIBUTION"){

                    X <- data.frame()
                    Bins <- as.numeric(input$txtDiagnosticsCentralityDistributionBins)
                    
                    if(input$chkCentralityAnalysisStructureMultiplex){
                        v.x <- as.numeric(featureDataFrameList[["Multi"]][this.descriptor][,1])
                        if(input$centralityAnalysisDistributionLogx){
                            v.x <- log10(v.x)
                            if( any(is.infinite(v.x)) ){
                                v.x[is.infinite(v.x)] <- 0
                            }
                        }
                        x.min <- min(v.x,na.rm=T)
                        x.max <- max(v.x,na.rm=T)
                        x.step <- (x.max-x.min)/Bins
                        freq <- as.data.frame(table(cut(v.x, breaks=seq(x.min,x.max,x.step))))
                        if(input$centralityAnalysisDistributionLogy){
                            freq$Freq <- log10(freq$Freq)
                            if(any(is.infinite(freq$Freq))){
                                freq[is.infinite(freq$Freq),]$Freq <- 0
                            }
                        }
                        freq$Var1 <- seq(x.min,x.max,x.step)[1:Bins]
                        freq$Type <- "Multilayer"
                        X <- rbind(X, freq)
                    }
                    if(input$chkCentralityAnalysisStructureAggregate){
                        v.x <- as.numeric(featureDataFrameList[["Aggr"]][this.descriptor][,1])
                        if(input$centralityAnalysisDistributionLogx){
                            v.x <- log10(v.x)
                            if( any(is.infinite(v.x)) ){
                                v.x[is.infinite(v.x)] <- 0
                            }
                        }
                        x.min <- min(v.x,na.rm=T)
                        x.max <- max(v.x,na.rm=T)
                        x.step <- (x.max-x.min)/Bins
                        freq <- as.data.frame(table(cut(v.x, breaks=seq(x.min,x.max,x.step))))
                        if(input$centralityAnalysisDistributionLogy){
                            freq$Freq <- log10(freq$Freq)
                            if(any(is.infinite(freq$Freq))){
                                freq[is.infinite(freq$Freq),]$Freq <- 0
                            }
                        }
                        freq$Var1 <- seq(x.min,x.max,x.step)[1:Bins]
                        freq$Type <- "Aggregate"
                        X <- rbind(X, freq)
                    }                    
                    if(input$chkCentralityAnalysisStructureLayer){
                        for(l in layersToInclude){
                            v.x <- as.numeric(featureDataFrameList[[ paste0("L",l) ]][this.descriptor][,1])
                            if(input$centralityAnalysisDistributionLogx){
                                v.x <- log10(v.x)
                                if( any(is.infinite(v.x)) ){
                                    v.x[is.infinite(v.x)] <- 0
                                }
                            }
                            x.min <- min(v.x,na.rm=T)
                            x.max <- max(v.x,na.rm=T)
                            x.step <- (x.max-x.min)/Bins
                            freq <- as.data.frame(table(cut(v.x, breaks=seq(x.min,x.max,x.step))))
                            if(input$centralityAnalysisDistributionLogy){
                                freq$Freq <- log10(freq$Freq)
                                if(any(is.infinite(freq$Freq))){
                                    freq[is.infinite(freq$Freq),]$Freq <- 0
                                }
                            }
                            freq$Var1 <- seq(x.min,x.max,x.step)[1:Bins]
                            freq$Type <- layerLabel[[l]]
                            X <- rbind(X, freq)    
                        }
                    }                    
                    
                    #print(X)
            
                    output$centralityAnalysisPlot <- renderChart2({
                        rplot <- nPlot(Freq ~ Var1, 
                                 group = "Type",
                                 data = X, type = "multiBarChart")

                        if(input$centralityAnalysisDistributionLogy){
                            rplot$yAxis(axisLabel="log10 Counts")
                        }else{
                            rplot$yAxis(axisLabel="Counts")
                        }
                        if(input$centralityAnalysisDistributionLogx){
                            rplot$xAxis(axisLabel=paste0("log10 ", this.descriptor))
                        }else{
                            rplot$xAxis(axisLabel=this.descriptor)
                        }

                        return(rplot)
                    })

                }
                
                if(input$radDiagnosticsCentralityType=="DIAGNOSTICS_ANALYSIS_SCATTER"){
                    this.descriptor.x <- this.descriptor
                    this.descriptor.y <- input$selDiagnosticsCentralityVizScatterID                    
                    this.descriptor.color <- input$selDiagnosticsCentralityVizScatterColorID
                    this.descriptor.size <- input$selDiagnosticsCentralityVizScatterSizeID
                    X <- data.frame()
                    if(input$chkCentralityAnalysisStructureMultiplex){
                        v.x <- as.numeric(featureDataFrameList[["Multi"]][this.descriptor.x][,1])
                        v.y <- as.numeric(featureDataFrameList[["Multi"]][this.descriptor.y][,1])

                        if(input$centralityAnalysisScatterLogx){
                            v.x <- log10(v.x)
                            if( any(is.infinite(v.x)) ){
                                v.x[is.infinite(v.x)] <- 0
                            }
                        }
                        if(input$centralityAnalysisScatterLogy){
                            v.y <- log10(v.y)
                            if( any(is.infinite(v.y)) ){
                                v.y[is.infinite(v.y)] <- 0
                            }
                        }

                        v.radius <- 1
                        if(input$selDiagnosticsCentralityVizScatterSizeID!="Uniform"){
                            this.descriptor.radius <- input$selDiagnosticsCentralityVizScatterSizeID
                            v.radius <- as.numeric(featureDataFrameList[["Multi"]][this.descriptor.radius][,1])
    
                            if(input$centralityAnalysisScatterLogRadius){
                                v.radius <- log10(v.radius)
                                if( any(is.infinite(v.radius)) ){
                                    v.radius[is.infinite(v.radius)] <- 0
                                }
                            }
                        }
                        
                        X <- rbind(X, data.frame(Node=nodesLabel[[1]], Var1=v.x, Var2=v.y, Radius=v.radius, Type="Multilayer"))
                    }
                    if(input$chkCentralityAnalysisStructureAggregate){
                        v.x <- as.numeric(featureDataFrameList[["Aggr"]][this.descriptor.x][,1])
                        v.y <- as.numeric(featureDataFrameList[["Aggr"]][this.descriptor.y][,1])

                        if(input$centralityAnalysisScatterLogx){
                            v.x <- log10(v.x)
                            if( any(is.infinite(v.x)) ){
                                v.x[is.infinite(v.x)] <- 0
                            }
                        }
                        if(input$centralityAnalysisScatterLogy){
                            v.y <- log10(v.y)
                            if( any(is.infinite(v.y)) ){
                                v.y[is.infinite(v.y)] <- 0
                            }
                        }

                        v.radius <- 1
                        if(input$selDiagnosticsCentralityVizScatterSizeID!="Uniform"){
                            this.descriptor.radius <- input$selDiagnosticsCentralityVizScatterSizeID
                            v.radius <- as.numeric(featureDataFrameList[["Aggr"]][this.descriptor.radius][,1])
    
                            if(input$centralityAnalysisScatterLogRadius){
                                v.radius <- log10(v.radius)
                                if( any(is.infinite(v.radius)) ){
                                    v.radius[is.infinite(v.radius)] <- 0
                                }
                            }
                        }
                        
                        X <- rbind(X, data.frame(Node=nodesLabel[[1]], Var1=v.x, Var2=v.y, Radius=v.radius, Type="Aggregate"))
                    }
                    if(input$chkCentralityAnalysisStructureLayer){
                        for(l in layersToInclude){
                            v.x <- as.numeric(featureDataFrameList[[ paste0("L",l) ]][this.descriptor.x][,1])
                            v.y <- as.numeric(featureDataFrameList[[ paste0("L",l) ]][this.descriptor.y][,1])

                            if(input$centralityAnalysisScatterLogx){
                                v.x <- log10(v.x)
                                if( any(is.infinite(v.x)) ){
                                    v.x[is.infinite(v.x)] <- 0
                                }
                            }
                            if(input$centralityAnalysisScatterLogy){
                                v.y <- log10(v.y)
                                if( any(is.infinite(v.y)) ){
                                    v.y[is.infinite(v.y)] <- 0
                                }
                            }

                            v.radius <- 1
                            if(input$selDiagnosticsCentralityVizScatterSizeID!="Uniform"){
                                this.descriptor.radius <- input$selDiagnosticsCentralityVizScatterSizeID
                                v.radius <- as.numeric(featureDataFrameList[[ paste0("L",l) ]][this.descriptor.radius][,1])
        
                                if(input$centralityAnalysisScatterLogRadius){
                                    v.radius <- log10(v.radius)
                                    if( any(is.infinite(v.radius)) ){
                                        v.radius[is.infinite(v.radius)] <- 0
                                    }
                                }
                            }
                            
                            X <- rbind(X, data.frame(Node=nodesLabel[[1]], Var1=v.x, Var2=v.y, Radius=v.radius, Type=layerLabel[[l]]))
                        }
                    }

        
                    output$centralityAnalysisPlot <- renderChart2({
                        rplot <- nPlot(Var2 ~ Var1, 
                                 group = "Type", opacity=list(const=as.numeric(input$txtDiagnosticsCentralityVizScatterColorTransparency)), 
                                 data = X, type = "scatterChart")

                        if(input$centralityAnalysisScatterLogy){
                            rplot$xyAxis(axisLabel=paste0("log10 ", this.descriptor.y))
                        }else{
                            rplot$yAxis(axisLabel=this.descriptor.y)
                        }
                        if(input$centralityAnalysisScatterLogx){
                            rplot$xAxis(axisLabel=paste0("log10 ", this.descriptor.x))
                        }else{
                            rplot$xAxis(axisLabel=this.descriptor.x)
                        }

                        rplot$chart(size = '#! function(d){return d.Radius} !#')
                        rplot$chart(tooltipContent = "#! function(key, x, y, e){ return  '<h3>' + e.point.Node + '</h3> ' + '<br><b>Type:</b> ' + key } !#")
                        rplot$chart(forceY=c(0.9*floor(min(X$Var2)),1.1*floor(max(X$Var2))), 
                                          forceX=c(0.9*floor(min(X$Var1)),1.1*floor(max(X$Var1))))
                                          
                        return(rplot)
                    })

                }
                                
                btnCentralityDiagnosticsAnalysisValue <<- input$btnCentralityDiagnosticsAnalysis
    
                progress$set(message = 'Diagnostics analysis Completed!', value = 1)
                Sys.sleep(2)
            })  
        })

                
      	################################################
      	# Reducibility
      	################################################
    
        observe({
            if(input$btnCalculateReducibility ==0 || input$btnImportNetworks == 0 ||  LAYERS==0)
                return()
            
            if(btnCalculateReducibilityValue==input$btnCalculateReducibility) return()
    
            isolate({
                progress <- shiny::Progress$new(session)
                on.exit(progress$close())
    
                progress$set(message = 'Calculating Redundancy...', value = 0.05)
                    
                #create the config file for calling Octave's computation
                createOctaveConfigFile()
    
                write(input$selReducibilityClusterMethod, file="hclust_method.tmp", append=F)
    
                #call octave
                system("octave -qf octave/muxMultisliceReducibility.m",intern=T)
                    
                #read output
                resultFile <- paste(input$txtProjectName,"_reducibility_jsd.txt",sep="")
                
                progress$set(message = 'Hierarchical clustering...', value = 0.6)
                
                Layer <- NULL
                for(l in 1:LAYERS) Layer = c(Layer,as.character(layerLabel[[l]]))
    
                distanceMatrix <- matrix(scan(resultFile, n = LAYERS*LAYERS), ncol=LAYERS, nrow=LAYERS, byrow = TRUE, dimnames=list(NULL, Layer))
                #if(file.exists(resultFile)) file.remove(resultFile)
    
                outfile6 <- buildPath("tmp","image_jsd.png")
                
                png(outfile6, width=650, height=650)
                rgb.palette <- colorRampPalette(brewer.pal(brewer.pal.info$maxcolors[row.names(brewer.pal.info)==input$selReducibilityColorPalette],input$selReducibilityColorPalette))(200)

               
                heatmap.2(distanceMatrix,labRow=Layer,labCol=Layer,cexRow=as.numeric(input$txtREDUCIBILITY_HEATMAP_FONT_SIZE),cexCol=as.numeric(input$txtREDUCIBILITY_HEATMAP_FONT_SIZE),hclustfun=function(x) hclust(x,method=input$selReducibilityClusterMethod),col=rgb.palette,symm=F,dendrogram="column",trace="none",offsetCol=-0.4,offsetRow=-0.4)
                dev.off()
    
                output$jsdMatrixSummaryImage <- renderImage({
                    list(src = outfile6,
                        contentType = 'image/png',
                        width = 650,
                        height = 650,
                        alt = "")
                        }, 
                        deleteFile = FALSE
                    )
                #if(file.exists(outfile6)) file.remove(outfile6)
    
                outfile7 <- buildPath("tmp","image_dendrogram.png")
                png(outfile7, width=650, height=650)
                plot(hclust(as.dist(distanceMatrix),method=input$selReducibilityClusterMethod),labels=Layer,cex=as.numeric(input$txtREDUCIBILITY_HEATMAP_FONT_SIZE),main="Reducibility Dendrogram",sub="", xlab="")
                dev.off()
    
                output$reducibilityDendrogramSummaryImage <- renderImage({
                    list(src = outfile7,
                        contentType = 'image/png',
                        width = 650,
                        height = 650,
                        alt = "")
                        }, 
                        deleteFile = FALSE
                    )

                output$reducibilityQualityFunction <- renderChart2({
                    resultFile <- paste(input$txtProjectName,"_reducibility_quality.txt",sep="")
                    data <- read.table(resultFile, header=T, sep=" ")[,1:2]
                    colnames(data) <- c("Step", "Q")
                    linechart <- nPlot(Q ~ Step, data = data, type = 'lineChart')
                    linechart$addParams(width = 600, height = 400, title="Quality function") 
                    linechart$xAxis(axisLabel="Step")
                    linechart$yAxis(axisLabel="Q")

                    linechart$chart(forceY=c(floor(min(data$Q)),floor(max(data$Q))+1), 
                                      forceX=c(floor(min(data$Step)),floor(max(data$Step))+1))

                    return(linechart)
                })    

                #interSpearman <- data.frame(interSpearman)
                #interSpearman <- cbind(data.frame(Layer),interSpearman)
    
                #output$interSpearmanSummaryTable <- renderGvis({
                #    if(input$btnCalculateXXXDiagnostics==0 || input$btnImportNetworks == 0 || LAYERS==0)
                #        return(NULL)
                    
                #    gvisTable(interSpearman,options=googleVisInterSpearmanSummaryTableOptions())
                #})   
                
                #Sys.sleep(2)
                if(file.exists("hclust_method.tmp")) file.remove("hclust_method.tmp")
                if(file.exists("hclust_merge.txt")) file.remove("hclust_merge.txt")
                if(file.exists("jsd_distance_matrix.txt")) file.remove("jsd_distance_matrix.txt")
                if(file.exists(resultFile)) file.remove(resultFile)
                resultFile <- paste(input$txtProjectName,"_reducibility_quality.txt",sep="")
                #if(file.exists(resultFile)) file.remove(resultFile)
                
                btnCalculateReducibilityValue <<- input$btnCalculateReducibility
    
                progress$set(message = 'Reducibility analysis Completed!', value = 1)
                Sys.sleep(2)
            })  
        })
    
    
      	################################################
      	# Apply layout
      	################################################
    
        observe({
            if(input$btnApplyLayout==0 || input$btnImportNetworks == 0)
                return()
    
            if(btnApplyLayoutValue==input$btnApplyLayout) return()
    
            isolate({
                progress <- shiny::Progress$new(session)
                on.exit(progress$close())
                
                #First check if the layout is wrt Multiplex, a layer or independent
                if(input$radLayoutType == 'LAYOUT_BY_LAYER_ID' && input$selInputLayerID!='None'){
                    LAYOUT_BY_LAYER_ID <<- as.numeric(strsplit(input$selInputLayerID," ")[[1]][2])
                }else{
                    LAYOUT_BY_LAYER_ID <<- 0
                }
                
                if(input$radLayoutType == 'LAYOUT_INDEPENDENT'){
                    LAYOUT_INDEPENDENT <<- TRUE
                }else{
                    LAYOUT_INDEPENDENT <<- FALSE
                }
    
                if(input$radLayoutDimension=="LAYOUT_DIMENSION_2D"){
                    LAYOUT_DIMENSION <<- 2
                }else{
                    LAYOUT_DIMENSION <<- 3
                }
    
                layouts <<- vector("list",LAYERS+1)
                
                print(paste("Layout dimension:",LAYOUT_DIMENSION))
                
                #Check if the layouts are specified by external files, otherwise proceed with the automatic ones
                if(!LAYOUT_EXTERNAL || (GEOGRAPHIC_LAYOUT && input$chkPLOT_AS_EDGE_COLORED)){
                    if(!LAYOUT_INDEPENDENT){    
                        print("Constrained layout option.")
                        progress$set(message = 'Constrained layout...', value = 0.05)
                        Sys.sleep(0.5)
                        if(LAYOUT_BY_LAYER_ID>0){
                            #It will use the first layer to layout the others
                            if(WEIGHTED){
                                gAggr <- graph.adjacency(AdjMatrix[[LAYOUT_BY_LAYER_ID]], weighted=T)
                            }else{
                                gAggr <- graph.adjacency(AdjMatrix[[LAYOUT_BY_LAYER_ID]], weighted=NULL)    
                            }
                        }else{
                            #Aggregate graph from aggregate adjacency matrix
                            progress$set(message = 'Calculating aggregated...', value = 0.1)
                            gAggr <- graph.adjacency(AdjMatrix[[LAYERS+1]], weighted=T)
                            print("Aggregate network created. Proceeding with layout to obtain coordinates for each layer.")
                        }
                        
                        #Note that here, gAggr does not correspond to the aggregate when LAYOUT_BY_LAYER_ID is T 
                        #But this is only confusing in this piece of code. I am too lazy now to change the name of this
                        #variable, I keep this note for the future. The aggregate is in g[[LAYERS+1]]
                        
                        progress$set(message = 'Applying layout...', value = 0.2)
                        #Choose a layout and apply it to the aggregate network
                        if(input$radLayoutAlgorithm=="LAYOUT_FRUCHTERMAN_REINGOLD"){
                            lAggr <- layout.fruchterman.reingold.grid(gAggr,weights=E(gAggr)$weight,niter=as.numeric(input$txtLAYOUT_MAXITER),area=vcount(gAggr)^1.,repulserad=vcount(gAggr)^1.3,dim=LAYOUT_DIMENSION)
                        }
                        if(input$radLayoutAlgorithm=="LAYOUT_LGL"){
                            lAggr <- layout.lgl(gAggr,maxiter=as.numeric(input$txtLAYOUT_MAXITER))
                        }
                        if(input$radLayoutAlgorithm=="LAYOUT_DRL"){
                            lAggr <- layout.drl(gAggr,options=list(simmer.attraction=0,simmer.iterations=floor(as.numeric(input$txtLAYOUT_MAXITER)*0.15),crunch.iterations=floor(as.numeric(input$txtLAYOUT_MAXITER)*0.1),cooldown.iterations=floor(as.numeric(input$txtLAYOUT_MAXITER)*0.25),expansion.iterations=floor(as.numeric(input$txtLAYOUT_MAXITER)*0.25),liquid.iterations=floor(as.numeric(input$txtLAYOUT_MAXITER)*0.25)),dim=LAYOUT_DIMENSION)
                        }
                        
                        if(input$radLayoutAlgorithm=="LAYOUT_REINGOLD_TILFORD"){
                            lAggr <- layout.reingold.tilford(gAggr)
                        }
                        
                        if(input$radLayoutAlgorithm=="LAYOUT_KAMADA_KAWAI"){
                            lAggr <- layout.kamada.kawai(gAggr, niter=as.numeric(input$txtLAYOUT_MAXITER),dim=LAYOUT_DIMENSION)
                        }
                        if(input$radLayoutAlgorithm=="LAYOUT_SPRING"){
                            lAggr <- layout.spring(gAggr,repulse=T)
                        }
                        
                        if(input$radLayoutAlgorithm=="LAYOUT_COMBINED"){
                            #We try to use the DRL to scale and we use it as seed for a Kamada-Kawai with few iterations
                            ltmp <- layout.drl(gAggr,options=list(simmer.attraction=0,simmer.iterations=floor(as.numeric(input$txtLAYOUT_MAXITER)*0.15),crunch.iterations=floor(as.numeric(input$txtLAYOUT_MAXITER)*0.1),cooldown.iterations=floor(as.numeric(input$txtLAYOUT_MAXITER)*0.25),expansion.iterations=floor(as.numeric(input$txtLAYOUT_MAXITER)*0.25),liquid.iterations=floor(as.numeric(input$txtLAYOUT_MAXITER)*0.25)),dim=LAYOUT_DIMENSION)
    
                            lAggr <- layout.kamada.kawai(gAggr, niter=as.numeric(input$txtLAYOUT_MAXITER),start=ltmp,dim=LAYOUT_DIMENSION)
                        }

                        #For compatibility with the other option, we assign lAggr to each layout, aggregate included
                        for(l in 1:(LAYERS+1)){
                            layouts[[l]] <<- lAggr
                        }
                    }else{
                        progress$set(message = 'Independent layout option...', value = 0.05)
                        Sys.sleep(0.5)
                        print("Independent layout option.")
                        for(l in 1:(LAYERS+1)){
                            layouts[[l]] <<- matrix(c(1),ncol=3,nrow=Nodes)
                            
                            progress$set(message = paste('Layout for layer',l,"..."), value = 0.05 + 0.85* l / (LAYERS+1))
                            
                            print(paste("  Layout for layer",l,"..."))
                            #Each layout is calculated separately    
                            if(input$radLayoutAlgorithm=="LAYOUT_FRUCHTERMAN_REINGOLD"){
                                layouts[[l]] <<- layout.fruchterman.reingold.grid(g[[l]],weights=E(g[[l]])$weight,niter=as.numeric(input$txtLAYOUT_MAXITER),area=vcount(g[[l]])^1.,repulserad=vcount(gAggr)^1.3,dim=LAYOUT_DIMENSION)
                            }
                            if(input$radLayoutAlgorithm=="LAYOUT_LGL"){
                                layouts[[l]] <<- layout.lgl(g[[l]],maxiter=as.numeric(input$txtLAYOUT_MAXITER))
                            }
                            if(input$radLayoutAlgorithm=="LAYOUT_DRL"){
                                layouts[[l]] <<- layout.drl(g[[l]],options=list(simmer.attraction=0,simmer.iterations=floor(as.numeric(input$txtLAYOUT_MAXITER)*0.15),crunch.iterations=floor(as.numeric(input$txtLAYOUT_MAXITER)*0.1),cooldown.iterations=floor(as.numeric(input$txtLAYOUT_MAXITER)*0.25),expansion.iterations=floor(as.numeric(input$txtLAYOUT_MAXITER)*0.25),liquid.iterations=floor(as.numeric(input$txtLAYOUT_MAXITER)*0.25)),dim=LAYOUT_DIMENSION)
                            }
                            
                            if(input$radLayoutAlgorithm=="LAYOUT_REINGOLD_TILFORD"){
                                layouts[[l]] <<- layout.reingold.tilford(g[[l]])
                            }
                            
                            if(input$radLayoutAlgorithm=="LAYOUT_KAMADA_KAWAI"){
                                layouts[[l]] <<- layout.kamada.kawai(g[[l]], niter=as.numeric(input$txtLAYOUT_MAXITER),dim=LAYOUT_DIMENSION)
                            }
                            if(input$radLayoutAlgorithm=="LAYOUT_SPRING"){
                                layouts[[l]] <<- layout.spring(g[[l]],repulse=T)
                            }
                            
                            if(input$radLayoutAlgorithm=="LAYOUT_COMBINED"){
                                #We try to use the DRL to scale and we use it as seed for a Kamada-Kawai with few iterations
                                ltmp <- layout.drl(g[[l]],options=list(simmer.attraction=0,simmer.iterations=floor(as.numeric(input$txtLAYOUT_MAXITER)*0.15),crunch.iterations=floor(as.numeric(input$txtLAYOUT_MAXITER)*0.1),cooldown.iterations=floor(as.numeric(input$txtLAYOUT_MAXITER)*0.25),expansion.iterations=floor(as.numeric(input$txtLAYOUT_MAXITER)*0.25),liquid.iterations=floor(as.numeric(input$txtLAYOUT_MAXITER)*0.25)),dim=LAYOUT_DIMENSION)
                                
                                layouts[[l]] <<- layout.kamada.kawai(g[[l]], niter=as.numeric(input$txtLAYOUT_MAXITER),start=ltmp,dim=LAYOUT_DIMENSION)
                            }
                        }
                    }
                }else{
                    print("Layouting: external files.")
                    for(l in 1:LAYERS){
                        progress$set(message = paste('Layout for layer',l,"..."), value = 0.05 + 0.85* l / (LAYERS+1))
                        #print(paste("DEBUG Layer",l))
                        #print(Nodes)
                                                
                        layouts[[l]] <<- matrix(c(1),nrow=Nodes,ncol=2)
                        layouts[[l]] <<- layerLayout[[l]]
                    }
                    
                    #giving the layout of the aggregate from external file makes no sense if it is different from the other layers
                    #and it is also annoying to be constrained to specify the aggregate, if one does not want to show it.
                    #Therefore, here I prefer to assign manually the layout of the first layer to the aggregate.
                    #So far, I accept this possibility just for sake of completeness, but a correct use of muxViz should avoid
                    #situations like this..
                    progress$set(message = paste('Layout for layer',l,"..."), value = 0.05 + 0.85)
                    layouts[[LAYERS+1]] <<- layouts[[1]]
                }
    
                if(LAYOUT_DIMENSION==2 || dim(layouts[[1]])[2]==2){
                    #Make it a 3-columns object
                    for(l in 1:(LAYERS+1)){
                        layouts[[l]] <<- cbind(layouts[[l]][,1:2],0)
                    }
                }

                if(!LAYOUT_EXTERNAL && !GEOGRAPHIC_LAYOUT){
                    XMAX <<- -1e10
                    YMAX <<- -1e10
                    ZMAX <<- -1e10
                    XMIN <<- 1e10
                    YMIN <<- 1e10
                    ZMIN <<- 1e10

                    for(l in 1:(LAYERS+1)){
                        if(min(layouts[[l]][,1],na.rm=T) < XMIN) XMIN <<- min(layouts[[l]][,1],na.rm=T)
                        if(min(layouts[[l]][,2],na.rm=T) < YMIN) YMIN <<- min(layouts[[l]][,2],na.rm=T)
                        if(min(layouts[[l]][,3],na.rm=T) < ZMIN) ZMIN <<- min(layouts[[l]][,3],na.rm=T)
                        if(max(layouts[[l]][,1],na.rm=T) > XMAX) XMAX <<- max(layouts[[l]][,1],na.rm=T)
                        if(max(layouts[[l]][,2],na.rm=T) > YMAX) YMAX <<- max(layouts[[l]][,2],na.rm=T)
                        if(max(layouts[[l]][,3],na.rm=T) > ZMAX) ZMAX <<- max(layouts[[l]][,3],na.rm=T)
                    }
                }
    

                if(GEOGRAPHIC_LAYOUT && LAYOUT_EXTERNAL){
                    #we check if the boundaries are given by the user
                    if(input$txtGEOGRAPHIC_LAT_MIN != "" && input$txtGEOGRAPHIC_LAT_MAX != "" && input$txtGEOGRAPHIC_LONG_MIN != "" && input$txtGEOGRAPHIC_LONG_MAX != ""){
                        #we are sure here that each layout is specified correctly
                        for(l in 1:LAYERS){
                            layerTable <- read.table(layerLayoutFile[[l]][1], header=T, sep=as.character(input$txtEdgeListFileSep))
                            
                            if(input$chkEdgeListLabel) layerTable$nodeID <- 1:nrow(layerTable)
                            
                            layerLayout[[l]] <<- matrix(c(1),nrow=Nodes,ncol=2)
                            
                            print(paste("Layout for layer",l,"is geographic and user-defined. Converting."))
                            #Get boundaries
                            thisLATMIN <- as.numeric(input$txtGEOGRAPHIC_LAT_MIN)
                            thisLATMAX <- as.numeric(input$txtGEOGRAPHIC_LAT_MAX)
                            thisLONGMIN <- as.numeric(input$txtGEOGRAPHIC_LONG_MIN)
                            thisLONGMAX <- as.numeric(input$txtGEOGRAPHIC_LONG_MAX)
                            
                            #mapproject has problems in converting |latitudes| > 80, therefore we have to constrain
                            if(thisLATMIN < -80){
                                thisLATMIN <- -80
                                print("Warning! Min Latitude smaller than -80, changing to -80.")
                            }
                            if(thisLATMAX > 80){
                                thisLATMAX <- 80
                                print("Warning! Max Latitude larger than 80, changing to 80.") 
                            }
                            #the geographical maps give error for extremal longitudes. Constrain here too..
                            if(thisLONGMIN < -179){
                                thisLONGMIN <- -179
                                print("Warning! Min Longitude smaller than -179, changing to -179.")
                            }
                            if(thisLONGMAX>179){
                                thisLONGMAX <- 179
                                print("Warning! Max Longitude larger than 179, changing to 179.") 
                            }
                                            
                            layerTable$nodeLong[ layerTable$nodeLong < thisLONGMIN ] <- thisLONGMIN
                            layerTable$nodeLong[ layerTable$nodeLong > thisLONGMAX ] <- thisLONGMAX
                            layerTable$nodeLat[ layerTable$nodeLat < thisLATMIN ] <- thisLATMIN
                            layerTable$nodeLat[ layerTable$nodeLat > thisLATMAX ] <- thisLATMAX
        
                            longBounds = c(min(layerTable$nodeLong,na.rm=T),max(layerTable$nodeLong,na.rm=T))
                            latBounds = c(min(layerTable$nodeLat,na.rm=T),max(layerTable$nodeLat,na.rm=T))
                                         
                            print(paste("  Latitude new boundaries: ",latBounds[1],latBounds[2]))
                            print(paste("  Longitude new boundaries: ",longBounds[1],longBounds[2]))
                            
                            #The input layout is geographic, we must convert it to cartesian
                            sphCoordinates <- list()
                            sphCoordinates$x <- layerTable$nodeLong
                            sphCoordinates$y <- layerTable$nodeLat
                            cartCoordinates <- mapproject(sphCoordinates,proj="mercator")
                            
                            layerTable$nodeX <- cartCoordinates$x
                            layerTable$nodeY <- cartCoordinates$y
                            
                            layerLayout[[l]][layerTable$nodeID + offsetNode,1:2] <<- cbind(layerTable$nodeX,layerTable$nodeY)
                            layouts[[l]] <<- layerLayout[[l]]
                            layouts[[l]] <<- cbind(layouts[[l]][,1:2],0)
                
                            sphCoordinates <- list()
                            sphCoordinates$x <- c(thisLONGMIN,thisLONGMAX)
                            sphCoordinates$y <- c(thisLATMIN,thisLATMAX)
                            cartCoordinates <- mapproject(sphCoordinates,proj="mercator")
                            XMIN <<- cartCoordinates$x[1]
                            XMAX <<- cartCoordinates$x[2]
                            YMIN <<- cartCoordinates$y[1]
                            YMAX <<- cartCoordinates$y[2]  
                        }            
                        layerLayout[[LAYERS+1]] <<- layerLayout[[1]]
                        layouts[[LAYERS+1]] <<- layerLayout[[1]]
                        layouts[[LAYERS+1]] <<- cbind(layouts[[LAYERS+1]][,1:2],0)
                    }
                }

                print(paste("LIMITS:", XMIN, XMAX, YMIN, YMAX))
                            
                progress$set(message = 'Normalizing coordinates...', value = 0.95)
    
                print("  Normalizing coordinates...")    
    
                if(input$radNetworkOfLayersLayoutType=="NETWORK_LAYERS_LAYOUT_ONELINE"){
                    #rescale the layout to allow superposition with shift along z-axis
                    for(l in 1:(LAYERS+1)){
                        #not quite sure about this piece of code, waiting for empirical problems
                        #deltaX <- min(layouts[[l]][,1],na.rm=T) - XMIN
                        #if(XMIN > min(layouts[[l]][,1],na.rm=T)){
                        #    deltaX <- -deltaX
                        #}
                        #deltaY <- min(layouts[[l]][,2],na.rm=T) - YMIN
                        #if(YMIN > min(layouts[[l]][,2],na.rm=T)){
                        #    deltaY <- -deltaY
                        #}
                        deltaX <- 0
                        deltaY <- 0
                        layouts[[l]][,1] <<- as.numeric(input$txtLAYER_SCALE)*(layouts[[l]][,1] - XMIN + deltaX)/(XMAX-XMIN) - 1 + (l-1)*as.numeric(input$txtLAYER_SHIFT)
                        layouts[[l]][,2] <<- as.numeric(input$txtLAYER_SCALE)*(layouts[[l]][,2] - YMIN + deltaY)/(YMAX-YMIN) - 1
        
                        if(LAYERS>1){
                            if(input$chkPLOT_AS_EDGE_COLORED){
                                if(input$chkAGGREGATE_SHOW){
                                    layouts[[l]][,1] <<- ((layouts[[LAYERS+1]][,1]- XMIN)/(XMAX-XMIN))*runif(1,1.005,1.01)
                                    layouts[[l]][,2] <<- ((layouts[[LAYERS+1]][,2] - YMIN)/(YMAX-YMIN))*runif(1,1.005,1.01)
                                }else{
                                    layouts[[l]][,1] <<- ((layouts[[LAYERS+1]][,1]- XMIN)/(XMAX-XMIN))*runif(1,1.005,1.01)
                                    layouts[[l]][,2] <<- ((layouts[[LAYERS+1]][,2] - YMIN)/(YMAX-YMIN))*runif(1,1.005,1.01)
                                    #todo tofix this part to allow 2D plots showing the aggregate
                                }                        
        
                                if(LAYOUT_DIMENSION==3){
                                    layouts[[l]][,3] <<- ((layouts[[LAYERS+1]][,3] - ZMIN)/(ZMAX-ZMIN))*runif(1,1.005,1.01)
                                }else{
                                    layouts[[l]][,3] <<- 0
                                }
                                
                            }else{
                                if(input$chkAGGREGATE_SHOW){
                                    layouts[[l]][,3] <<- -1 + as.numeric(input$txtLAYER_SCALE)*as.numeric(input$txtLAYER_SPACE)*l/(LAYERS+1)
                                }else{
                                    layouts[[l]][,3] <<- -1 + as.numeric(input$txtLAYER_SCALE)*as.numeric(input$txtLAYER_SPACE)*l/LAYERS
                                }
                            }
                        }else{
                            #We allow this only if the layer is one
                            if(LAYOUT_DIMENSION==3 && !GEOGRAPHIC_LAYOUT){
                                layouts[[l]][,3] <<- as.numeric(input$txtLAYER_SCALE)*as.numeric(input$txtLAYER_SPACE)*(layouts[[l]][,3] - ZMIN)/(ZMAX-ZMIN) - 1 
                            }else{
                                layouts[[l]][,3] <<- 1
                            }
                        }
                    }
                }else{
                    #new network of layers layouts
                    if(input$radNetworkOfLayersLayoutType=="NETWORK_LAYERS_LAYOUT_MULTILINE"){
                        if(LAYERS>1){
                            rows <- as.numeric(input$txtNetworkLayersMultilineRows)
                            cols <- as.numeric(input$txtNetworkLayersMultilineCols)
                            
                            #estimate the number of levels in the third dimension
                            multilevels <- floor(LAYERS/(rows*cols)) + 1
                            if( LAYERS %% (rows*cols)==0 ){
                                #if the ratio is exact, we don't need one more level in the worst case
                                multilevels <- multilevels-1
                            }

                            scal <- as.numeric(input$txtLAYER_SCALE)
                            rescx <- scal/(XMAX-XMIN)
                            rescy <- scal/(YMAX-YMIN)
                            #shift <- as.numeric(input$txtLAYER_SHIFT) #useless for this layout
                            space <- as.numeric(input$txtLAYER_SPACE)

                            shiftx <- (cols*scal + cols*space)/2
                            shifty <- (rows*scal + rows*space)/2

                            rowcnt <- 1
                            colcnt <- 1
                            levelcnt <- 1
                            for(l in 1:LAYERS){
                                layouts[[l]][,1] <<- rescx*(layouts[[l]][,1] - XMIN) - shiftx + (colcnt-1)*scal + (colcnt-1)*space - 1
                                layouts[[l]][,2] <<- rescy*(layouts[[l]][,2] - YMIN) + shifty - (rowcnt-1)*scal - (rowcnt-1)*space - 1
                                
                                #change z accordinly
                                layouts[[l]][,3] <<- 1 - scal*2*space*(levelcnt-1)/LAYERS
                                
                                colcnt <- colcnt + 1
                                if(colcnt==(cols+1)){
                                    rowcnt <- rowcnt + 1
                                    
                                    if(rowcnt==(rows+1)){
                                        levelcnt <- levelcnt + 1
                                        rowcnt <- 1
                                    }
                                    
                                    colcnt <- 1
                                }
                            }
                            
                            
                        }else{
                            progress$set(message = 'This layout require more than one layer!', value = 0.9)
                        }
                    }
                    if(input$radNetworkOfLayersLayoutType=="NETWORK_LAYERS_LAYOUT_FORCEDIRECTED"){ 
                        if(LAYERS>1){
                            scal <- as.numeric(input$txtLAYER_SCALE)
                            rescx <- scal/(XMAX-XMIN)
                            rescy <- scal/(YMAX-YMIN)

                            #this is not the optimal approach.. but for the networks handled by muxViz shoudl be enough
                            #alternatively: use igraph to create the weighted network, then convert again to data.frame
                            #see http://lists.nongnu.org/archive/html/igraph-help/2013-02/msg00079.html
                            dfNoN <- data.frame()
                            
                            sub.multi <- multilayerEdges #multilayerEdges[ multilayerEdges[,2]!=multilayerEdges[,4], ]
                            sub.multi$V1 <- NULL
                            sub.multi$V3 <- NULL
                            #print(sub.multi)
                            
                            colnames(sub.multi) <- c("from", "to", "weight")
                            g.non <- graph.data.frame(sub.multi, directed=DIRECTED)
                            g.non <- simplify(g.non, edge.attr.comb="sum")

                            layout.non <<- layout.auto(g.non, dim=3)
                            layout.non[,1] <<- (layout.non[,1] - min(layout.non[,1]))/(max(layout.non[,1])-min(layout.non[,1]))
                            layout.non[,2] <<- (layout.non[,2] - min(layout.non[,2]))/(max(layout.non[,2])-min(layout.non[,2]))
                            layout.non[,3] <<- (layout.non[,3] - min(layout.non[,3]))/(max(layout.non[,3])-min(layout.non[,3]))
                            layout.non <<- scal*scal*layout.non + scal
                            #print(layout.non)
                            
                            for(l in 1:LAYERS){
                                layouts[[l]][,1] <<- rescx*(layouts[[l]][,1] - XMIN) - 1 - layout.non[l,1]
                                layouts[[l]][,2] <<- rescy*(layouts[[l]][,2] - YMIN) - 1 - layout.non[l,2]
                                layouts[[l]][,3] <<-  layout.non[l,3]
                            }
                        }else{
                            progress$set(message = 'This layout require more than one layer!', value = 0.9)
                        }
                    }
                    if(input$radNetworkOfLayersLayoutType=="NETWORK_LAYERS_LAYOUT_MATRIX"){

                        if(LAYERS>1){                            
                            rows <- as.numeric(input$txtNetworkLayersMatrixRows)
                            cols <- as.numeric(input$txtNetworkLayersMatrixCols)

                            if(rows*cols<LAYERS){
                                progress$set(message = 'ERROR! Rows x Columns < # Layers ...', value = 0.9)
                            }else{
                                scal <- as.numeric(input$txtLAYER_SCALE)
                                rescx <- scal/(XMAX-XMIN)
                                rescy <- scal/(YMAX-YMIN)
                                #shift <- as.numeric(input$txtLAYER_SHIFT) #useless for this layout
                                space <- as.numeric(input$txtLAYER_SPACE)

                                shiftx <- (cols*scal + cols*space)/2
                                shifty <- (rows*scal + rows*space)/2

                                rowcnt <- 1
                                colcnt <- 1
                                for(l in 1:LAYERS){
                                    layouts[[l]][,1] <<- rescx*(layouts[[l]][,1] - XMIN) - shiftx + (colcnt-1)*scal + (colcnt-1)*space - 1
                                    layouts[[l]][,2] <<- rescy*(layouts[[l]][,2] - YMIN) + shifty - (rowcnt-1)*scal - (rowcnt-1)*space - 1
                                    
                                    #keep the same z
                                    #layouts[[l]][,3] <<- -1 + scal*space*l/LAYERS
                                    layouts[[l]][,3] <<- 0
                                    
                                    colcnt <- colcnt + 1
                                    if(colcnt==(cols+1)){
                                        colcnt = 1
                                        rowcnt <- rowcnt + 1
                                    }
                                }
                            }
                        }else{
                            progress$set(message = 'This layout require more than one layer!', value = 0.9)
                        }                        
                    }
                }            

                if(!input$chkPLOT_WITH_RGL){
                    #rotate the view, if needed
                    thx <- as.numeric(input$txtPLOT_ROTX)
                    thy <- as.numeric(input$txtPLOT_ROTY)
                    thz <- as.numeric(input$txtPLOT_ROTZ)
                    for(l in 1:(LAYERS+1)){
                        if(thx>0.){ layouts[[l]] <<- t( Rotx(thx) %*% t(layouts[[l]]) ) }
                        if(thy>0.){ layouts[[l]] <<- t( Roty(thy) %*% t(layouts[[l]]) ) }
                        if(thz>0.){ layouts[[l]] <<- t( Rotz(thz) %*% t(layouts[[l]]) ) }
                    }            
                }

                progress$set(message = 'Layout Completed!', value = 1)
                Sys.sleep(2)
                print("Layouting finished. Proceeding with openGL plot of each layer.")
            
                btnApplyLayoutValue <<- input$btnApplyLayout
            })
        })
    
    
      	################################################
      	# Plot
      	################################################
      	makeRendering <- function(){
            progress <- shiny::Progress$new(session)
            on.exit(progress$close())

            progress$set(message = 'Start rendering...', value = 0.05)
            Sys.sleep(1)

            if(input$chkPLOT_WITH_RGL){
                #save orientation for later user, if needed
                orientationRGL <<- par3d(no.readonly=TRUE)
                
                rgl.clear()
                tryCatch(rgl.pop("lights"),error=function(e) print("Warning: no lights to pop"))
                rgl.light(theta = 0, phi = 0, viewpoint.rel = TRUE, ambient = "#FFFFFF", 
                diffuse = "#FFFFFF", specular = "#FFFFFF")
                #print(rgl.ids())
                #if ( length(rgl.ids) )
                #rgl.pop(type="lights")
            }else{
                #plot.new()
                par(mar=c(0, 0, 0, 0), xaxs='i', yaxs='i') 
                par(oma=c(0, 0, 0, 0))
                
                xmin.tmp <- 1e100
                xmax.tmp <- -1e100
                ymin.tmp <- 1e100
                ymax.tmp <- -1e100
                for(l in 1:LAYERS){
                    xmin.tmp <- min(xmin.tmp, min(layouts[[l]][,1]))
                    xmax.tmp <- max(xmax.tmp, max(layouts[[l]][,1]))
                    ymin.tmp <- min(ymin.tmp, min(layouts[[l]][,2]))
                    ymax.tmp <- max(ymax.tmp, max(layouts[[l]][,2]))
                }
                
                if(input$chkAGGREGATE_SHOW){
                    l <- LAYERS+1
                    xmin.tmp <- min(xmin.tmp, min(layouts[[l]][,1]))
                    xmax.tmp <- max(xmax.tmp, max(layouts[[l]][,1]))
                    ymin.tmp <- min(ymin.tmp, min(layouts[[l]][,2]))
                    ymax.tmp <- max(ymax.tmp, max(layouts[[l]][,2]))
                }
                
                xmin.tmp <- min(xmin.tmp*0.95, xmin.tmp*1.05)
                xmax.tmp <- max(xmax.tmp*0.95, xmax.tmp*1.05)
                ymin.tmp <- min(ymin.tmp*0.95, ymin.tmp*1.05)
                ymax.tmp <- max(ymax.tmp*0.95, ymax.tmp*1.05)
                
                plot(x=NULL, y=NULL, type="n", 
                    xlim=c(xmin.tmp,xmax.tmp), ylim=c(ymin.tmp,ymax.tmp)
                    )
                rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col =input$txtBACKGROUND_COLOR)
            }
            
            #layer color
            layer.color <- strsplit(input$txtLAYER_COLOR,",")[[1]]
            if(length(layer.color)==LAYERS){
                for(l in 1:LAYERS){
                    layerColor[[l]] <<- layer.color[l]
                }
            }else{
                for(l in 1:LAYERS){
                    layerColor[[l]] <<- input$txtLAYER_COLOR
                }                    
            }

            #layer alpha
            layer.alpha <- strsplit(input$txtLAYER_TRANSP,",")[[1]]
            if(length(layer.alpha)==LAYERS){
                for(l in 1:LAYERS){
                    layerColorAlpha[[l]] <<- as.numeric(layer.alpha[l])
                }
            }else{
                for(l in 1:LAYERS){
                    layerColorAlpha[[l]] <<- as.numeric(input$txtLAYER_TRANSP)
                }                    
            }                

            #create the vector for inactive layers 
            vecInactiveLayers <- as.numeric(strsplit(input$txtLAYERS_ACTIVE, ",")[[1]])

            for(l in 1:(LAYERS+1)){
                if( l %in% vecInactiveLayers ){
                    #skip layers set to be inactive
                    next
                }
                progress$set(message = paste('Layer',l,'...'), value = 0.05 + 0.85*l/(LAYERS+1))

                if(l==(LAYERS+1)){
                    if( (!input$chkAGGREGATE_SHOW || LAYERS==1) || 
                        (input$chkPLOT_AS_EDGE_COLORED && LAYOUT_DIMENSION==3) ||
                        (input$radNetworkOfLayersLayoutType!="NETWORK_LAYERS_LAYOUT_ONELINE")
                        ){
                        #if we don't want to show the aggregate, we must skip the rest
                        #we must skip also if the layers is just 1
                        next
                    }
                }
                if(l<LAYERS+1){
                    print(paste("Layer: ",l))
                }else{
                    print(paste("Layer: Aggregate"))    
                }
                #V(g[[l]])$vertex.label.color <- rgb(47,47,47,0,maxColorValue = 255)
                V(g[[l]])$vertex.label.color <- input$txtNODE_LABELS_FONT_COLOR

                #this set the transparency level of edges and nodes.. it can be customized
                #E(g[[l]])$alpha <- floor(as.numeric(input$txtEDGE_TRANSP)*255)
                #V(g[[l]])$alpha <- floor(as.numeric(input$txtNODE_TRANSP)*255)
            

                print("Other graphic options...")
                
                #other assignments
                E(g[[l]])$curve<- as.numeric(input$txtEDGE_BENDING)
            
                if(!input$chkNODE_LABELS_SHOW){
                    V(g[[l]])$label <- ""
                }else{
                    V(g[[l]])$label <- nodesLabel[[l]]
                }
            
                arrayDiagnostics <- 1

                if(diagnosticsOK){
                    if(input$btnCalculateCentralityDiagnostics>0){
                        #the GUI is visualizing the list of possibilities
                        attrib <- input$selVizNodeSizeID
                        if(attrib=="Uniform"){
                            arrayDiagnostics <- rep(1,Nodes)
                        }else{
                            arrayDiagnostics <- as.numeric(listDiagnostics[[l]][attrib][,1])
                        }
                    }else{
                        #the GUI shows only the UNIFORM option
                        if(input$radNodeSizeType=="NODE_SIZE_PROPORTIONAL_TO_UNIFORM"){
                            arrayDiagnostics <- rep(1,Nodes)
                        }
                    }
                }
                                
                if(input$radNodeSizeType2=="NODE_SIZE_PROPORTIONAL_TYPE_NORMAL"){
                    V(g[[l]])$size <- as.numeric(input$txtNODE_DEFAULT_SIZE)*arrayDiagnostics
                }else if(input$radNodeSizeType2=="NODE_SIZE_PROPORTIONAL_TYPE_LOG"){
                    V(g[[l]])$size <- as.numeric(input$txtNODE_DEFAULT_SIZE)*(1+2*log(1+arrayDiagnostics))
                }else if(input$radNodeSizeType2=="NODE_SIZE_PROPORTIONAL_TYPE_LOGLOG"){
                    V(g[[l]])$size <- as.numeric(input$txtNODE_DEFAULT_SIZE)*log(1+log(1+arrayDiagnostics));
                }
                                
                if(input$radEdgeSizeType=="EDGE_SIZE_PROPORTIONAL_TO_UNIFORM"){                
                    E(g[[l]])$size <- as.numeric(input$txtEDGE_DEFAULT_SIZE);
                }else if(input$radEdgeSizeType=="EDGE_SIZE_PROPORTIONAL_TO_WEIGHT"){
                    if(WEIGHTED){
                        if(input$radEdgeSizeType2=="EDGE_SIZE_PROPORTIONAL_TYPE_NORMAL"){
                            E(g[[l]])$size <- E(g[[l]])$weight
                        }else if(input$radEdgeSizeType2=="EDGE_SIZE_PROPORTIONAL_TYPE_LOG"){
                            E(g[[l]])$size <- as.numeric(input$txtEDGE_DEFAULT_SIZE)*log(1+E(g[[l]])$weight)
                        }else if(input$radEdgeSizeType2=="EDGE_SIZE_PROPORTIONAL_TYPE_LOGLOG"){
                            E(g[[l]])$size <- as.numeric(input$txtEDGE_DEFAULT_SIZE)*log(1+log(1+E(g[[l]])$weight))
                        }
                    }else{
                        E(g[[l]])$size <- as.numeric(input$txtEDGE_DEFAULT_SIZE);
                    }
                }

                #Node coloring
                if(input$radNodeColor=="NODE_COLOR_COMMUNITY"){
                    if(communityOK){
                        if(input$btnCalculateCommunityDiagnostics>0){
                            if(input$radCommunityAlgorithm!="COMMUNITY_MULTIPLEX"){
                                #color-code by community
                                tmpColor <- rainbow( max(listCommunities[[l]]$Community) + 2, alpha=as.numeric(input$txtNODE_TRANSP), start=runif(1))[ listCommunities[[l]]$Community ]
                                #setting a random start should avoid coloring nodes in the same way on different layers
                            }else{
                                #for the multiplex we want exactly the opposite behavior
                                tmpColor <- rainbow( max(listCommunities[[l]]$Community) + 2, alpha=as.numeric(input$txtNODE_TRANSP), start=commonRunif)[ listCommunities[[l]]$Community ]                                
                            }
                            
                            V(g[[l]])$color <- tmpColor
                        }
                    }
                }else if(input$radNodeColor=="NODE_COLOR_COMPONENT"){
                    if(input$chkPERFORM_COMPONENT_DETECTION && input$btnCalculateComponentDiagnostics>0){
                        #todo
                    }
                }else if(input$radNodeColor=="NODE_COLOR_RANDOM"){
                    #colorset for the multiplex                
                    if( input$selMultiplexColorPalette=="random" ){
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
                    
                        E(g[[l]])$color<-rgb(red=E(g[[l]])$red, 
                                                        green=E(g[[l]])$green, 
                                                        blue=E(g[[l]])$blue, 
                                                        maxColorValue=255)
                        V(g[[l]])$color <- rgb(red=V(g[[l]])$red, 
                                                            green=V(g[[l]])$green, 
                                                            blue=V(g[[l]])$blue, 
                                                            maxColorValue=255)
                    }else{
                        colorPalette <- colorRampPalette(brewer.pal(brewer.pal.info$maxcolors[row.names(brewer.pal.info)==input$selMultiplexColorPalette],input$selMultiplexColorPalette))(LAYERS+1)
                        E(g[[l]])$color <- colorPalette[l]
                        V(g[[l]])$color <- colorPalette[l]
                    }
                }else if(input$radNodeColor=="NODE_COLOR_UNIFORM"){
                    E(g[[l]])$color <- input$txtNODE_COLOR_UNIFORM_COLOR
                    V(g[[l]])$color <- input$txtNODE_COLOR_UNIFORM_COLOR
                }else if(input$radNodeColor=="NODE_COLOR_CENTRALITY"){
                    if(diagnosticsOK){
                        bins <- as.numeric(input$txtNODE_COLOR_CENTRALITY_BINS)
                        colorPalette <- colorRampPalette(brewer.pal(brewer.pal.info$maxcolors[row.names(brewer.pal.info)==input$selCentralityColorPalette],input$selCentralityColorPalette))(bins)

                        attrib <- input$selVizNodeColorID
                        values <- as.numeric(listDiagnostics[[l]][attrib][,1])  
                        
                        if(input$radNodeColorType2=="NODE_COLOR_PROPORTIONAL_TYPE_LOG"){
                            values <- 1+2*log(1+values)
                        }else if(input$radNodeColorType2=="NODE_COLOR_PROPORTIONAL_TYPE_LOGLOG"){
                            values <- log(1+log(1+values))
                        }

                        values <- 1 + (bins-1)*(values - min(values, na.rm=T))/(max(values, na.rm=T) - min(values, na.rm=T))
                        values <- floor(values)
                        
                        E(g[[l]])$color <- "#F2F2F2"
                        V(g[[l]])$color <- colorPalette[ values ]
                    }
                }else if(input$radNodeColor=="NODE_COLOR_TOPRANK"){
                    if(diagnosticsOK){
                        if(input$btnCalculateCentralityDiagnostics>0 && as.numeric(input$txtNODE_COLOR_TOP)>0){
                            numTop <- as.numeric(input$txtNODE_COLOR_TOP)
                            attrib <- input$selVizNodeColorTopID
                            values <- as.numeric(listDiagnostics[[l]][attrib][,1])  
                            topNodes <- head(rev(order(values)),numTop)
                            #V(g[[l]])$color <- rgb(169,169,169, V(g[[l]])$alpha, maxColorValue=255)
                            #E(g[[l]])$color <- rgb(169,169,169, V(g[[l]])$alpha, maxColorValue=255)
                            V(g[[l]])$color <- input$txtNODE_COLOR_TOP_COLOR_OTHERS
                            E(g[[l]])$color <- input$txtNODE_COLOR_TOP_COLOR_OTHERS
                            #V(g[[l]])[topNodes]$color <- rgb(255, 0, 0, V(g[[l]])[topNodes]$alpha, maxColorValue=255)
                            V(g[[l]])[topNodes]$color <- input$txtNODE_COLOR_TOP_COLOR_TOP
                            
                            if(input$chkNODE_LABELS_SHOW_ONLY_TOP){
                                V(g[[l]])$label <- ""
                                V(g[[l]])[topNodes]$label <- nodesLabel[[l]][topNodes]
                                V(g[[l]])[topNodes]$vertex.label.color <- input$txtNODE_COLOR_TOP_LABELS_FONT_COLOR
                            }
                        }
                    }
                }

                if(input$chkNODE_ISOLATED_HIDE){
                    #this piece of code must be executed after the above one, to change the size of isolated
                    #nodes to zero, and also their label to ""
                    
                    if(input$radMultiplexModel == "MULTIPLEX_IS_EDGECOLORED"){
                        arrayStrength <- graph.strength(g[[l]],mode="total")
                        V(g[[l]])[arrayStrength==0.]$size <- 0
                        V(g[[l]])[arrayStrength==0.]$label <- ""
                    }else{
                        if(input$chkNODE_ISOLATED_HIDE_INTERLINKS){
                            #account for degree in the multiplex
                            
                            arrayStrength <- graph.strength(g.multi,mode="total")
                            idxtohide <- which(arrayStrength==0.)
                            inlayers <- floor((idxtohide-1)/Nodes) + 1
                            innodes <- (idxtohide-1) %% Nodes + 1

                            idxs <- which(inlayers==l)
                            nodes2hide <- which(V(g[[l]]) %in% innodes[idxs])
                            V(g[[l]])[nodes2hide]$size <- 0
                            V(g[[l]])[nodes2hide]$label <- ""
                        }else{
                            #do not account for interlinks, just intralinks
                            arrayStrength <- graph.strength(g[[l]],mode="total")
                            V(g[[l]])[arrayStrength==0.]$size <- 0
                            V(g[[l]])[arrayStrength==0.]$label <- ""
                        }
                    }
                }

                V(g[[l]])$shape <- "circle"
                V(g[[l]])$shape[V(g[[l]])$size==0] <- "none"                    
                E(g[[l]])$color <- addalpha(E(g[[l]])$color, as.numeric(input$txtEDGE_TRANSP))
                V(g[[l]])$color <- addalpha(V(g[[l]])$color, as.numeric(input$txtNODE_TRANSP))
                V(g[[l]])$framecolor <- input$txtNODE_FRAME_COLOR

                if(input$txtNODE_FRAME_COLOR==""){ V(g[[l]])$framecolor <- V(g[[l]])$color }

                if(input$chkNODE_LABELS_SHOW_WRAP){
                    V(g[[l]])$label2 <- lapply(lapply(V(g[[l]])$label, function(x) strwrap(x,as.numeric(input$txtNODE_LABELS_WRAP))), function(x) paste(x, collapse='\n'))
                }else{
                    V(g[[l]])$label2 <- V(g[[l]])$label
                }


                #saving default values for later usage
                defaultVsize[[l]] <<- V(g[[l]])$size
                defaultVcolor[[l]] <<- V(g[[l]])$color
                defaultEsize[[l]] <<- E(g[[l]])$size
                defaultEcolor[[l]] <<- E(g[[l]])$color

                if(input$chkPLOT_WITH_RGL){
                    print("  openGL phase...")

                    #plot the graph with openGL    
                    #print(layouts[[l]])
                    rglplot(g[[l]], layout=layouts[[l]],
                                        vertex.size=V(g[[l]])$size, 
                                        vertex.color=V(g[[l]])$color,
                                        vertex.label="",#V(g[[l]])$label,
                                        vertex.label.dist=as.numeric(input$txtNODE_LABELS_DISTANCE), #,+ 0.01*V(g[[l]])$size,
                                        vertex.label.font=2,
                                        vertex.label.cex=0, 
                                        vertex.label.color=V(g[[l]])$vertex.label.color,
                                        edge.width=E(g[[l]])$size, 
                                        edge.color=E(g[[l]])$color, 
                                        edge.arrow.size=as.numeric(input$txtLAYER_ARROW_SIZE), 
                                        edge.arrow.width=as.numeric(input$txtLAYER_ARROW_WIDTH), 
                                        edge.curved=E(g[[l]])$curve,
                                        rescale=F)
                }else{
                    print("  Standard device output...")

                    #plot the graph with openGL    
                    #print(layouts[[l]])
                    plot(g[[l]], layout=layouts[[l]],
                                    vertex.size=V(g[[l]])$size, 
                                    vertex.shape=V(g[[l]])$shape,
                                    vertex.color=V(g[[l]])$color,
                                    vertex.frame.color=V(g[[l]])$framecolor,
                                    vertex.label=V(g[[l]])$label2,
                                    vertex.label.dist=as.numeric(input$txtNODE_LABELS_DISTANCE), #,+ 0.01*V(g[[l]])$size,
                                    vertex.label.font=2,
                                    vertex.label.cex=as.numeric(input$txtNODE_LABELS_FONT_SIZE), 
                                    vertex.label.color=V(g[[l]])$vertex.label.color,
                                    edge.width=E(g[[l]])$size, 
                                    edge.color=E(g[[l]])$color, 
                                    edge.arrow.size=as.numeric(input$txtLAYER_ARROW_SIZE), 
                                    edge.arrow.width=as.numeric(input$txtLAYER_ARROW_WIDTH), 
                                    edge.curved=E(g[[l]])$curve,
                                    rescale=F, add=T)
                                    
                        title(main=input$txtPLOT_TITLE, sub=input$txtPLOT_SUBTITLE)
                }
                print(paste("  Layout of layer: finished."))
            }
            
            if(input$chkINTERLINK_SHOW && LAYERS>1){
                if(input$radMultiplexModel!="MULTIPLEX_IS_EDGECOLORED"){
                    print("Adding interlayer links.")

                    #set to 0 the width of intra-layer links
                    E(g.multi)$width <- as.numeric(input$txtINTERLINK_WIDTH)*E(g.multi)$weight
                    E(g.multi)[which(multilayerEdges[,2]==multilayerEdges[,4])]$width <- 0
                    
                    #the same for interlinks from and to inactive layers 
                    for(l in vecInactiveLayers){
                        E(g.multi)[which(multilayerEdges[,2]==l | multilayerEdges[,4]==l)]$width <- 0
                    }
                    
                    #setup the layout for g.multi by merging the layout of each layer, in order
                    layout.multi <<- matrix(0, ncol=3, nrow=Nodes*LAYERS)
                    
                    for(l in 1:LAYERS){
                        layout.multi[ (1 + (l-1)*Nodes):(l*Nodes), 1] <<- layouts[[l]][, 1]
                        layout.multi[ (1 + (l-1)*Nodes):(l*Nodes), 2] <<- layouts[[l]][, 2]
                        layout.multi[ (1 + (l-1)*Nodes):(l*Nodes), 3] <<- layouts[[l]][, 3]
                    }

                    #Print the interlinks by superimposing the g.multi
                    if(input$chkPLOT_WITH_RGL){
                        rglplot(g.multi, layout=layout.multi,
                                            vertex.size=0, 
                                            vertex.label="",
                                            vertex.label.cex=0,
                                            edge.width=E(g.multi)$width, 
                                            edge.color=input$txtINTERLINK_COLOR, 
                                            edge.arrow.size=as.numeric(input$txtLAYER_ARROW_SIZE), 
                                            edge.arrow.width=as.numeric(input$txtLAYER_ARROW_WIDTH), 
                                            edge.curved=as.numeric(input$txtEDGE_BENDING),
                                            edge.lty = input$selINTERLINK_TYPE,
                                            rescale=F)
                    }else{
                        plot(g.multi, layout=layout.multi,
                                    vertex.size=0, 
                                    vertex.shape="none",
                                    vertex.label="",
                                    vertex.label.cex=0,
                                    edge.width=E(g.multi)$width, 
                                    edge.color=addalpha(input$txtINTERLINK_COLOR,as.numeric(input$txtINTERLINK_TRANSP)), 
                                    edge.arrow.size=as.numeric(input$txtLAYER_ARROW_SIZE), 
                                    edge.arrow.width=as.numeric(input$txtLAYER_ARROW_WIDTH), 
                                    edge.curved=as.numeric(input$txtEDGE_BENDING),
                                    edge.lty = input$selINTERLINK_TYPE,
                                    rescale=F, add=T)
                    }
                    #edge/node transparancy not yet supported by rglplot
                    #alpha=as.numeric(input$txtINTERLINK_TRANSP))
                }                
            }                

                                
            if(input$chkPLOT_WITH_RGL){
                #Call the visualization of other RGL graphics
                FinalizeRenderingMultiplex(progress)
            }
            
            progress$set(message = 'Rendering Completed!', value = 1)
            Sys.sleep(2)
    	 }
    	 
        observe({
            if(input$btnRenderNetworks==0 || input$btnApplyLayout==0 || input$btnImportNetworks == 0 || LAYERS<=0) return()
    
            if(btnRenderNetworksValue==input$btnRenderNetworks) return()
            
            isolate({    
                makeRendering()
            
                btnRenderNetworksValue <<- input$btnRenderNetworks
            })
        })
        
        ############################################
        ## Global functions
        ############################################
        
        FinalizeRenderingMultiplex <- function(progress){
            #Towards the end.. add layers, textures if any, etc
            #Create the hash of the properties of the map, to avoid downloading multiple times
            #the same area 
            thisLATMIN <- LATMIN
            thisLATMAX <- LATMAX
            thisLONGMIN <- LONGMIN
            thisLONGMAX <- LONGMAX
            if(input$txtGEOGRAPHIC_LAT_MIN != "" && input$txtGEOGRAPHIC_LAT_MAX != "" && input$txtGEOGRAPHIC_LONG_MIN != "" && input$txtGEOGRAPHIC_LONG_MAX != ""){
                thisLATMIN <- as.numeric(input$txtGEOGRAPHIC_LAT_MIN)
                thisLATMAX <- as.numeric(input$txtGEOGRAPHIC_LAT_MAX)
                thisLONGMIN <- as.numeric(input$txtGEOGRAPHIC_LONG_MIN)
                thisLONGMAX <- as.numeric(input$txtGEOGRAPHIC_LONG_MAX)
            }
            #print(paste(thisLATMIN,thisLATMAX,thisLONGMIN,thisLONGMAX))
            #mapproject has problems in converting |latitudes| > 80, therefore we have to constrain
            if(thisLATMIN < -80){
                thisLATMIN <- -80
                print("Warning! Min Latitude smaller than -80, changing to -80.")
            }
            if(thisLATMAX > 80){
                thisLATMAX <- 80
                print("Warning! Max Latitude larger than 80, changing to 80.") 
            }
            #the geographical maps give error for extremal longitudes. Constrain here too..
            if(thisLONGMIN < -179){
                thisLONGMIN <- -179
                print("Warning! Min Longitude smaller than -179, changing to -179.")
            }
            if(thisLONGMAX>179){
                thisLONGMAX <- 179
                print("Warning! Max Longitude larger than 179, changing to 179.") 
            }
            
            hash <- digest( c(thisLATMIN,thisLATMAX,thisLONGMIN,thisLONGMAX,input$selOSMType) , algo="md5" )
            fileNamePNG <- buildPath("tmp",paste(hash,".png",sep=""))
            #print(paste("COORDS:",LATMIN,LATMAX,LONGMIN,LONGMAX,XMIN,XMAX,YMIN,YMAX))
            if(!file.exists(fileNamePNG)){
                if(GEOGRAPHIC_LAYOUT && (input$chkGEOGRAPHIC_BOUNDARIES_SHOW || input$chkGEOGRAPHIC_BOUNDARIES_AGGREGATE_SHOW)){
                    progress$set(message = 'Downloading map...', value = 0.95)
                    Sys.sleep(1)
                    print(paste("  Downloading geographic area..."))
                    #create a map with openstreetmap and save to a file for later use
                    rescaleFactor <- (YMAX-YMIN)/(XMAX-XMIN)
                    #H0/W0 = H/W  --> H = W/W0 * H0 = W*rescaleFactor in terms of Cartesian coords
                    pngWidth = 720
                    pngHeight = pngWidth*rescaleFactor
                    png(filename=fileNamePNG,width=pngWidth,height=pngHeight)
                    map = openmap(c(lat=thisLATMAX,   lon=thisLONGMIN), c(lat=thisLATMIN,   lon=thisLONGMAX), minNumTiles=18,type=input$selOSMType)
                    plot(map)
                    dev.off()
                }
            }
                        
            progress$set(message = 'Finalizing rendering...', value = 0.95)

            #inactive layers
            vecInactiveLayers <- as.numeric(strsplit(input$txtLAYERS_ACTIVE,",")[[1]])
    
            if(input$chkLAYER_SHOW && !input$chkPLOT_AS_EDGE_COLORED){
                if(input$radNetworkOfLayersLayoutType=="NETWORK_LAYERS_LAYOUT_ONELINE"){
                    #the standard one-line visualization made my muxViz, no changes wrt previous versions
                    for(l in 1:(LAYERS+1)){
                        if(l %in% vecInactiveLayers){
                            #skip inactive layers
                            next
                        }
                        
                        if(LAYERS>1){
                            #This draws a plan to be used as layer
                            if(input$chkAGGREGATE_SHOW){
                                d <- -1 + as.numeric(input$txtLAYER_SCALE)*as.numeric(input$txtLAYER_SPACE)*l/(LAYERS+1)
                            }else{
                                d <- -1 + as.numeric(input$txtLAYER_SCALE)*as.numeric(input$txtLAYER_SPACE)*l/LAYERS
                            }
                        }else{
                            #to allow drawing single-layer networks
                            d <- 1
                        }
                
                        x <- c(-1,-1,-1+as.numeric(input$txtLAYER_SCALE),-1+as.numeric(input$txtLAYER_SCALE)) + (l-1)*as.numeric(input$txtLAYER_SHIFT)
                        y <- c(-1+as.numeric(input$txtLAYER_SCALE),-1,-1,-1+as.numeric(input$txtLAYER_SCALE))
                        z <- c(d,d,d,d)

                        if(LAYOUT_DIMENSION==2){
                            if(l<LAYERS+1){
                                #planes3d(0,0,1, -d , alpha=LAYER_TRANSP, col=LAYER_COLOR)
                                if(GEOGRAPHIC_LAYOUT && input$chkGEOGRAPHIC_BOUNDARIES_SHOW){
                                    quads3d(x,y,z, alpha=layerColorAlpha[[l]], col=layerColor[[l]],texcoords=cbind(c(0,0,1,1), -c(0,1,1,0)), texture=fileNamePNG)
                                }else{
                                    quads3d(x,y,z, alpha=layerColorAlpha[[l]], col=layerColor[[l]])
                                }
                            }else{
                                if(input$chkAGGREGATE_SHOW && LAYERS>1){
                                    #planes3d(0,0,1, -d , alpha=LAYER_AGGREGATE_TRANSP, col=LAYER_AGGREGATE_COLOR)
                                    if(GEOGRAPHIC_LAYOUT && input$chkGEOGRAPHIC_BOUNDARIES_AGGREGATE_SHOW){
                                        quads3d(x,y,z, alpha=as.numeric(input$txtLAYER_AGGREGATE_TRANSP), col=input$txtLAYER_AGGREGATE_COLOR,texcoords=cbind(c(0,0,1,1), -c(0,1,1,0)), texture=fileNamePNG)
                                    }else{
                                        quads3d(x,y,z, alpha=as.numeric(input$txtLAYER_AGGREGATE_TRANSP), col=input$txtLAYER_AGGREGATE_COLOR)                    
                                    }
                                }else{
                                    next
                                }
                            }
                                            
                            if(input$chkLAYER_ID_SHOW_BOTTOMLEFT){
                                text3d(-1+(l-1)*as.numeric(input$txtLAYER_SHIFT), -1, d+0.1,text=layerLabel[[l]][1],adj = 0.2, color="black", family="sans", cex=as.numeric(input$txtLAYER_ID_FONTSIZE))
                            }
                            if(input$chkLAYER_ID_SHOW_TOPLEFT){
                                text3d(-1+(l-1)*as.numeric(input$txtLAYER_SHIFT), -1 + as.numeric(input$txtLAYER_SCALE), d+0.1,text=layerLabel[[l]][1],adj = 0.2, color="black", family="sans", cex=as.numeric(input$txtLAYER_ID_FONTSIZE))
                            }
                            if(input$chkLAYER_ID_SHOW_BOTTOMRIGHT){
                                text3d(-1+(l-1)*as.numeric(input$txtLAYER_SHIFT)+as.numeric(input$txtLAYER_SCALE), -1, d+0.1,text=layerLabel[[l]][1],adj = 0.2, color="black", family="sans", cex=as.numeric(input$txtLAYER_ID_FONTSIZE))
                            }
                            if(input$chkLAYER_ID_SHOW_TOPRIGHT){
                                text3d(-1+(l-1)*as.numeric(input$txtLAYER_SHIFT)+as.numeric(input$txtLAYER_SCALE), -1 + as.numeric(input$txtLAYER_SCALE), d+0.1,text=layerLabel[[l]][1],adj = 0.2, color="black", family="sans", cex=as.numeric(input$txtLAYER_ID_FONTSIZE))
                            }
                        }
                    }
                }else{
                    #new network of layers layouts
                    if(input$radNetworkOfLayersLayoutType=="NETWORK_LAYERS_LAYOUT_MULTILINE"){
                        if(LAYERS>1){                            
                            rows <- as.numeric(input$txtNetworkLayersMultilineRows)
                            cols <- as.numeric(input$txtNetworkLayersMultilineCols)

                            #estimate the number of levels in the third dimension
                            multilevels <- floor(LAYERS/(rows*cols)) + 1
                            if( LAYERS %% (rows*cols)==0 ){
                                #if the ratio is exact, we don't need one more level in the worst case
                                multilevels <- multilevels-1
                            }


                            scal <- as.numeric(input$txtLAYER_SCALE)
                            rescx <- scal/(XMAX-XMIN)
                            rescy <- scal/(YMAX-YMIN)
                            #shift <- as.numeric(input$txtLAYER_SHIFT) #useless for this layout
                            space <- as.numeric(input$txtLAYER_SPACE)

                            shiftx <- cols*(scal + space)/2
                            shifty <- rows*(scal + space)/2

                            rowcnt <- 1
                            colcnt <- 1
                            levelcnt <- 1
                            for(l in 1:LAYERS){
                                d <- 1 - scal*2*space*(levelcnt-1)/LAYERS
                                #when scal=1 x ranges in [-1,0]   y [-1,0]
                                #try: rgl.clear();axes3d();quads3d(c(-1,-1,0,0),c(0,-1,-1,0),c(0,0,0,0),col='green')
                                x <- c(-1,-1,-1+scal,-1+scal) - shiftx + (colcnt-1)*(scal + space)
                                y <- c(-1+scal,-1,-1,-1+scal) + shifty - (rowcnt-1)*(scal + space)
                                z <- c(d,d,d,d)

                                if(!l %in% vecInactiveLayers){
                                    #skip inactive layers
                                    if(GEOGRAPHIC_LAYOUT && input$chkGEOGRAPHIC_BOUNDARIES_SHOW){
                                        quads3d(x,y,z, alpha=layerColorAlpha[[l]], col=layerColor[[l]], 
                                                        texcoords=cbind(c(0,0,1,1), -c(0,1,1,0)), texture=fileNamePNG)
                                    }else{
                                        quads3d(x,y,z, alpha=layerColorAlpha[[l]], col=layerColor[[l]])
                                    }
                                                                        
                                    if(input$chkLAYER_ID_SHOW_TOPLEFT){
                                        text3d(-1- shiftx + (colcnt-1)*(scal + space), 
                                                    -1 + scal + shifty - (rowcnt-1)*(scal + space), 
                                                    d, 
                                                    text=layerLabel[[l]][1], 
                                                    adj = 0.2, 
                                                    color="black", family="sans", cex=as.numeric(input$txtLAYER_ID_FONTSIZE))
                                    }
                                    if(input$chkLAYER_ID_SHOW_BOTTOMLEFT){
                                        text3d(-1- shiftx + (colcnt-1)*(scal + space), 
                                                    -1 + shifty - (rowcnt-1)*(scal + space), 
                                                    d, 
                                                    text=layerLabel[[l]][1], 
                                                    adj = 0.2, 
                                                    color="black", family="sans", cex=as.numeric(input$txtLAYER_ID_FONTSIZE))
                                    }
                                    if(input$chkLAYER_ID_SHOW_BOTTOMRIGHT){
                                        text3d(-1 + scal - shiftx + (colcnt-1)*(scal + space), 
                                                    -1 + shifty - (rowcnt-1)*(scal + space), 
                                                    d, 
                                                    text=layerLabel[[l]][1], 
                                                    adj = 0.2, 
                                                    color="black", family="sans", cex=as.numeric(input$txtLAYER_ID_FONTSIZE))
                                    }
                                    if(input$chkLAYER_ID_SHOW_TOPRIGHT){
                                        text3d(-1 + scal - shiftx + (colcnt-1)*(scal + space), 
                                                    -1 + scal + shifty - (rowcnt-1)*(scal + space), 
                                                    d, 
                                                    text=layerLabel[[l]][1], 
                                                    adj = 0.2, 
                                                    color="black", family="sans", cex=as.numeric(input$txtLAYER_ID_FONTSIZE))
                                    }
                                }

                                colcnt <- colcnt + 1
                                if(colcnt==(cols+1)){
                                    rowcnt <- rowcnt + 1
                                    
                                    if(rowcnt==(rows+1)){
                                        levelcnt <- levelcnt + 1
                                        rowcnt <- 1
                                    }
                                    
                                    colcnt <- 1
                                }
                            }
                        }else{
                            progress$set(message = 'This layout require more than one layer!', value = 0.9)
                        }                        
                    }
                    if(input$radNetworkOfLayersLayoutType=="NETWORK_LAYERS_LAYOUT_FORCEDIRECTED"){
                        if(LAYERS>1){                            

                            scal <- as.numeric(input$txtLAYER_SCALE)
                            #shift <- as.numeric(input$txtLAYER_SHIFT) #useless for this layout
                            #space <- as.numeric(input$txtLAYER_SPACE)
                              
                            #this should have been already calculated and stored..  
                            #layout.non <<- layout.auto(g.non, dim=3)
                            #layout.non[,1] <<- (layout.non[,1] - min(layout.non[,1]))/(max(layout.non[,1])-min(layout.non[,1]))
                            #layout.non[,2] <<- (layout.non[,2] - min(layout.non[,2]))/(max(layout.non[,2])-min(layout.non[,2]))
                            #layout.non[,3] <<- (layout.non[,3] - min(layout.non[,3]))/(max(layout.non[,3])-min(layout.non[,3]))
                            #layout.non <<- scal*layout.non + scal
                            #print(layout.non)
                            

                            for(l in 1:LAYERS){
                                d <- layout.non[l,3]
                                shiftx <- layout.non[l,1]
                                shifty <- layout.non[l,2]
                                #when scal=1 x ranges in [-1,0]   y [-1,0]
                                #try: rgl.clear();axes3d();quads3d(c(-1,-1,0,0),c(0,-1,-1,0),c(0,0,0,0),col='green')
                                x <- c(-1,-1,-1+scal,-1+scal) - shiftx
                                y <- c(-1+scal,-1,-1,-1+scal) - shifty
                                z <- c(d,d,d,d)

                                if(!l %in% vecInactiveLayers){
                                    #skip inactive layers
                                    if(GEOGRAPHIC_LAYOUT && input$chkGEOGRAPHIC_BOUNDARIES_SHOW){
                                        quads3d(x,y,z, alpha=layerColorAlpha[[l]], col=layerColor[[l]], 
                                                        texcoords=cbind(c(0,0,1,1), -c(0,1,1,0)), texture=fileNamePNG)
                                    }else{
                                        quads3d(x,y,z, alpha=layerColorAlpha[[l]], col=layerColor[[l]])
                                    }
                                                                        
                                    if(input$chkLAYER_ID_SHOW_TOPLEFT){
                                        text3d(-1 - shiftx, 
                                                    -1 + scal - shifty, 
                                                    d, 
                                                    text=layerLabel[[l]][1], 
                                                    adj = 0.2, 
                                                    color="black", family="sans", cex=as.numeric(input$txtLAYER_ID_FONTSIZE))
                                    }
                                    if(input$chkLAYER_ID_SHOW_BOTTOMLEFT){
                                        text3d(-1 - shiftx, 
                                                    -1 - shifty, 
                                                    d, 
                                                    text=layerLabel[[l]][1], 
                                                    adj = 0.2, 
                                                    color="black", family="sans", cex=as.numeric(input$txtLAYER_ID_FONTSIZE))
                                    }
                                    if(input$chkLAYER_ID_SHOW_BOTTOMRIGHT){
                                        text3d(-1 + scal  - shiftx, 
                                                    -1  - shifty, 
                                                    d, 
                                                    text=layerLabel[[l]][1], 
                                                    adj = 0.2, 
                                                    color="black", family="sans", cex=as.numeric(input$txtLAYER_ID_FONTSIZE))
                                    }
                                    if(input$chkLAYER_ID_SHOW_TOPRIGHT){
                                        text3d(-1 + scal  - shiftx, 
                                                    -1 + scal - shifty, 
                                                    d, 
                                                    text=layerLabel[[l]][1], 
                                                    adj = 0.2, 
                                                    color="black", family="sans", cex=as.numeric(input$txtLAYER_ID_FONTSIZE))
                                    }
                                }
                            }
                        }else{
                            progress$set(message = 'This layout require more than one layer!', value = 0.9)
                        }                        
                    }
                    if(input$radNetworkOfLayersLayoutType=="NETWORK_LAYERS_LAYOUT_MATRIX"){
                        if(LAYERS>1){                            
                            rows <- as.numeric(input$txtNetworkLayersMatrixRows)
                            cols <- as.numeric(input$txtNetworkLayersMatrixCols)

                            if(rows*cols<LAYERS){
                                progress$set(message = 'ERROR! Rows x Columns < # Layers ...', value = 0.9)
                            }else{
                                scal <- as.numeric(input$txtLAYER_SCALE)
                                rescx <- scal/(XMAX-XMIN)
                                rescy <- scal/(YMAX-YMIN)
                                #shift <- as.numeric(input$txtLAYER_SHIFT) #useless for this layout
                                space <- as.numeric(input$txtLAYER_SPACE)

                                shiftx <- cols*(scal + space)/2
                                shifty <- rows*(scal + space)/2

                                rowcnt <- 1
                                colcnt <- 1
                                for(l in 1:LAYERS){
                                    d <- 0
                                    #when scal=1 x ranges in [-1,0]   y [-1,0]
                                    #try: rgl.clear();axes3d();quads3d(c(-1,-1,0,0),c(0,-1,-1,0),c(0,0,0,0),col='green')
                                    x <- c(-1,-1,-1+scal,-1+scal) - shiftx + (colcnt-1)*(scal + space)
                                    y <- c(-1+scal,-1,-1,-1+scal) + shifty - (rowcnt-1)*(scal + space)
                                    z <- c(d,d,d,d)

                                    if(!l %in% vecInactiveLayers){
                                        #skip inactive layers
                                        if(GEOGRAPHIC_LAYOUT && input$chkGEOGRAPHIC_BOUNDARIES_SHOW){
                                            quads3d(x,y,z, alpha=layerColorAlpha[[l]], col=layerColor[[l]], 
                                                          texcoords=cbind(c(0,0,1,1), -c(0,1,1,0)), texture=fileNamePNG)
                                        }else{
                                            quads3d(x,y,z, alpha=layerColorAlpha[[l]], col=layerColor[[l]])
                                        }
                                                                            
                                        if(input$chkLAYER_ID_SHOW_TOPLEFT){
                                            text3d(-1- shiftx + (colcnt-1)*(scal + space), 
                                                       -1 + scal + shifty - (rowcnt-1)*(scal + space), 
                                                       d, 
                                                       text=layerLabel[[l]][1], 
                                                       adj = 0.2, 
                                                       color="black", family="sans", cex=as.numeric(input$txtLAYER_ID_FONTSIZE))
                                        }
                                        if(input$chkLAYER_ID_SHOW_BOTTOMLEFT){
                                            text3d(-1- shiftx + (colcnt-1)*(scal + space), 
                                                       -1 + shifty - (rowcnt-1)*(scal + space), 
                                                       d, 
                                                       text=layerLabel[[l]][1], 
                                                       adj = 0.2, 
                                                       color="black", family="sans", cex=as.numeric(input$txtLAYER_ID_FONTSIZE))
                                        }
                                        if(input$chkLAYER_ID_SHOW_BOTTOMRIGHT){
                                            text3d(-1 + scal - shiftx + (colcnt-1)*(scal + space), 
                                                       -1 + shifty - (rowcnt-1)*(scal + space), 
                                                       d, 
                                                       text=layerLabel[[l]][1], 
                                                       adj = 0.2, 
                                                       color="black", family="sans", cex=as.numeric(input$txtLAYER_ID_FONTSIZE))
                                        }
                                        if(input$chkLAYER_ID_SHOW_TOPRIGHT){
                                            text3d(-1 + scal - shiftx + (colcnt-1)*(scal + space), 
                                                       -1 + scal + shifty - (rowcnt-1)*(scal + space), 
                                                       d, 
                                                       text=layerLabel[[l]][1], 
                                                       adj = 0.2, 
                                                       color="black", family="sans", cex=as.numeric(input$txtLAYER_ID_FONTSIZE))
                                        }
                                    }
                                    
                                    colcnt <- colcnt + 1
                                    if(colcnt==(cols+1)){
                                        colcnt = 1
                                        rowcnt <- rowcnt + 1
                                    }

                                }
                            }
                            
                            flag <- F
                            while(!flag){
                                tryCatch({
                                    print("Popping lights...")
                                    rgl.pop("lights")},
                                    error=function(e){
                                        print("Warning: no more lights to pop")
                                        },
                                    finally={flag=T}
                                    )
                            }
                            rgl.light(theta = 30, phi = 20, viewpoint.rel = TRUE, ambient = "#FFFFFF", 
                                        diffuse = "#FFFFFF", specular = "#FFFFFF")
                        }else{
                            progress$set(message = 'This layout require more than one layer!', value = 0.9)
                        }                        

                    }
                }            
            }
            
            #add labels as text3d because the vertex.label attribute of rgl.igraph does not work
            if(input$chkNODE_LABELS_SHOW){
                for(l in 1:LAYERS){    
                    if(!l %in% vecInactiveLayers){
                        this.labels <- nodesLabel[[l]]
                        if(input$chkNODE_ISOLATED_HIDE){
                            #this is to account for nodes isolated wrt intra-layer links, but not wrt inter-layer links
                            if(input$radMultiplexModel=="MULTIPLEX_IS_EDGECOLORED"){
                                arrayStrength <- graph.strength(g[[l]],mode="total")
                                this.labels[arrayStrength==0.] <- ""    
                            }else{
                                nodesOK <- union(multilayerEdges[ multilayerEdges$V2==l, ]$V1, multilayerEdges[ multilayerEdges$V4==l, ]$V3)                                
                                this.labels[-nodesOK] <- ""                                    
                            }
                        }
                        this.labels[ is.na(this.labels) ] <- ""

                        if(input$chkNODE_LABELS_SHOW_WRAP){
                            if(as.numeric(input$txtNODE_LABELS_WRAP)>0){
                                text3dwrap(layouts[[l]],
                                           this.labels,
                                           as.numeric(input$txtNODE_LABELS_WRAP),
                                           as.numeric(input$txtNODE_LABELS_WRAP_OFFSET),
                                           as.numeric(input$txtLAYER_SCALE),
                                           as.numeric(input$txtNODE_LABELS_DISTANCE), 
                                           input$txtNODE_LABELS_FONT_COLOR, 
                                           "sans", 
                                           as.numeric(input$txtNODE_LABELS_FONT_SIZE)
                                           )
                            }
                        }else{
                            text3d(layouts[[l]],
                                       text=this.labels,
                                       adj = as.numeric(input$txtNODE_LABELS_DISTANCE), 
                                       color=input$txtNODE_LABELS_FONT_COLOR, 
                                       family="sans", 
                                       cex=as.numeric(input$txtNODE_LABELS_FONT_SIZE)
                                       )
                        }
                    }
                }
            }
                        
#            if(!LAYOUT_INDEPENDENT){
#                if(input$chkINTERLINK_SHOW && as.numeric(input$txtINTERLINK_SHOW_FRACTION)>0 && LAYERS>1){
#                    print("Adding interlayer links.")
#                    #to be generalized to allow cross-interlink and absence of interlinks for some nodes
#                    for( l in 1:(LAYERS-1) ){
#                        layerLinesX <- matrix(c(0),nrow=Nodes,ncol=2)
#                        layerLinesY <- matrix(c(0),nrow=Nodes,ncol=2)
#                        layerLinesZ <- matrix(c(0),nrow=Nodes,ncol=2)
#            
#                        layerLinesX <- cbind(layouts[[l]][,1] + (l-1)*as.numeric(input$txtLAYER_SHIFT),layouts[[l+1]][,1] + l*as.numeric(input$txtLAYER_SHIFT))
#                        layerLinesY <- cbind(layouts[[l]][,2],layouts[[l+1]][,2])
#                        layerLinesZ <- cbind(layouts[[l]][,3],layouts[[l+1]][,3])
#            
#                        for(i in 1:Nodes){
#                            if(runif(1)>1-as.numeric(input$txtINTERLINK_SHOW_FRACTION)){ 
#                                segments3d(
#                                    layerLinesX[i,],
#                                    layerLinesY[i,],
#                                    layerLinesZ[i,],
#                                    lwd=as.numeric(input$txtINTERLINK_WIDTH), 
#                                    col=input$txtINTERLINK_COLOR, 
#                                    lty=input$selINTERLINK_TYPE,
#                                    alpha=as.numeric(input$txtINTERLINK_TRANSP))
#                            }
#                        }
#                    }
#                }
#            }
            
            if(!input$chkPLOT_AS_EDGE_COLORED){
                if(!input$chkPLOT_REMEMBER_ORIENTATION){
                    #forget current orientation and use the default one
                    M <- matrix(0, ncol=4,nrow=4)
                    M[1,] <- c(0.54,0,0.84,0)
                    M[2,] <- c(0.33,0.92,-0.22,0)
                    M[3,] <- c(-0.77,0.39,0.5,0)
                    M[4,] <- c(0,0,0,1)
                    
                    par3d(FOV=as.numeric(input$txtPLOT_FOV), userMatrix=M)
                }else{
                    #if orientation must be remembered but it's the first rendering:
                    if(btnRenderNetworksValue==0){
                        M <- matrix(0, ncol=4,nrow=4)
                        M[1,] <- c(0.54,0,0.84,0)
                        M[2,] <- c(0.33,0.92,-0.22,0)
                        M[3,] <- c(-0.77,0.39,0.5,0)
                        M[4,] <- c(0,0,0,1)
                        
                        par3d(FOV=as.numeric(input$txtPLOT_FOV), userMatrix=M)
                    }
                }
                
                #not really needed
                #par3D(orientationRGL)
            }
                        
            bg3d(input$txtBACKGROUND_COLOR)
            title3d(input$txtPLOT_TITLE, input$txtPLOT_SUBTITLE,'','','')
            
            if(input$chkPLOT_LIGHT){
                #add a light
                rgl.light(phi=as.numeric(input$txtPLOT_LIGHT_PHI),theta=as.numeric(input$txtPLOT_LIGHT_THETA))
            }

            if(input$chkPLOT_AXES3D){
                #add a light
                axes3d()
            }

            print("Finalizing rendering...")    
        }
        
    
        GetCentralityDataFrameArray <- function(Type){
            tmplistDiagnostics <- NULL
            
            if(Type=="Multiplex"){
                #calculation in the multiplex. For the moment the output is obtained calling octave.
                #the output will be stored in [[l]] for the multiplex and [[LAYERS+1]] for the aggregated.
                
                for(l in 1:LAYERS){
                    tmplistDiagnostics[[l]] <- data.frame(Layer = rep(paste(l,"Multi",sep="-"),Nodes))
                    tmplistDiagnostics[[l]] <- cbind(tmplistDiagnostics[[l]],data.frame(Node = 1:Nodes))
                    tmplistDiagnostics[[l]] <- cbind(tmplistDiagnostics[[l]],data.frame(Label = nodesLabel[[l]]))
                }
                l <- (LAYERS+1)
                tmplistDiagnostics[[l]] <- data.frame(Layer = rep("Aggr",Nodes))
                tmplistDiagnostics[[l]] <- cbind(tmplistDiagnostics[[l]],data.frame(Node = 1:Nodes))
                tmplistDiagnostics[[l]] <- cbind(tmplistDiagnostics[[l]],data.frame(Label = nodesLabel[[l]]))

                if(input$chkNODE_CENTRALITY_STRENGTH){
                    progress2 <- shiny::Progress$new(session)
                    on.exit(progress2$close())
                    progress2$set(message = paste('Current: Strength...'), value = 0.5)
                    
                    if(input$radMultiplexModel!="MULTIPLEX_IS_INTERDEPENDENT"){
                        createOctaveConfigFile()
                        #call octave
                        system("octave -qf octave/muxMultisliceCentralityDegree.m",intern=T)
                        #read output.
                        resultFile <- paste(input$txtProjectName,"_centrality_degree.txt",sep="")
                        centralityVector <- matrix(scan(resultFile, n = Nodes), ncol=1, nrow=Nodes, byrow = TRUE)
                        for(l in 1:LAYERS){
                            tmplistDiagnostics[[l]] <- cbind(tmplistDiagnostics[[l]],data.frame(Strength = centralityVector))
                        }
                        if(file.exists(resultFile)) file.remove(resultFile)
        
                        resultFile <- paste(input$txtProjectName,"_centrality_degree_aggregate.txt",sep="")
                        centralityVector <- matrix(scan(resultFile, n = Nodes), ncol=1, nrow=Nodes, byrow = TRUE)
                        l <- LAYERS+1
                        tmplistDiagnostics[[l]] <- cbind(tmplistDiagnostics[[l]],data.frame(Strength = centralityVector))
                        if(file.exists(resultFile)) file.remove(resultFile)
                    }else{
                        #for an interdependent network, it is enough to calculate centrality in the aggregate
                        #http://igraph.sourceforge.net/doc/R/graph.strength.html
                        centralityVector <- graph.strength(g[[LAYERS+1]],mode="total")
                        for(l in 1:(LAYERS+1)){
                            tmplistDiagnostics[[l]] <- cbind(tmplistDiagnostics[[l]],data.frame(Strength = centralityVector))
                        }
                    }
                    
                    progress2$set(message = paste('Current: Strength... Done!'), value = 1)
                    Sys.sleep(1)
                    progress2$close()
                }else{
                    for(l in 1:LAYERS){
                        tmplistDiagnostics[[l]] <- cbind(tmplistDiagnostics[[l]],data.frame(Strength = rep("-",Nodes)))
                    }
                    l <- (LAYERS+1)
                    tmplistDiagnostics[[l]] <- cbind(tmplistDiagnostics[[l]],data.frame(Strength = rep("-",Nodes)))
                }
    
                if(input$chkNODE_CENTRALITY_STRENGTH){
                    progress2 <- shiny::Progress$new(session)
                    on.exit(progress2$close())
                    progress2$set(message = paste('Current: In-Strength...'), value = 0.5)
                    
                    if(DIRECTED){
                        if(input$radMultiplexModel!="MULTIPLEX_IS_INTERDEPENDENT"){
                            createOctaveConfigFile()
                            #call octave
                            system("octave -qf octave/muxMultisliceCentralityInDegree.m",intern=T)
                            #read output.
                            resultFile <- paste(input$txtProjectName,"_centrality_indegree.txt",sep="")
                            centralityVector <- matrix(scan(resultFile, n = Nodes), ncol=1, nrow=Nodes, byrow = TRUE)
                            for(l in 1:LAYERS){
                                tmplistDiagnostics[[l]] <- cbind(tmplistDiagnostics[[l]],data.frame(StrengthIn = centralityVector))
                            }
                            if(file.exists(resultFile)) file.remove(resultFile)
            
                            resultFile <- paste(input$txtProjectName,"_centrality_indegree_aggregate.txt",sep="")
                            centralityVector <- matrix(scan(resultFile, n = Nodes), ncol=1, nrow=Nodes, byrow = TRUE)
                            l <- LAYERS+1
                            tmplistDiagnostics[[l]] <- cbind(tmplistDiagnostics[[l]],data.frame(StrengthIn = centralityVector))
                            if(file.exists(resultFile)) file.remove(resultFile)
                        }else{
                            #for an interdependent network, it is enough to calculate centrality in the aggregate
                            #http://igraph.sourceforge.net/doc/R/graph.strength.html
                            centralityVector <- graph.strength(g[[LAYERS+1]],mode="in")
                            for(l in 1:(LAYERS+1)){
                                tmplistDiagnostics[[l]] <- cbind(tmplistDiagnostics[[l]],data.frame(StrengthIn = centralityVector))
                            }

                        }
                    }else{
                        for(l in 1:(LAYERS+1)){
                            tmplistDiagnostics[[l]] <- cbind(tmplistDiagnostics[[l]],data.frame(StrengthIn = tmplistDiagnostics[[l]]$Strength))
                        }
                    }

                    progress2$set(message = paste('Current: In-Strength... Done!'), value = 1)
                    Sys.sleep(1)
                    progress2$close()

                }else{
                    for(l in 1:LAYERS){
                        tmplistDiagnostics[[l]] <- cbind(tmplistDiagnostics[[l]],data.frame(StrengthIn = rep("-",Nodes)))
                    }
                    l <- (LAYERS+1)
                    tmplistDiagnostics[[l]] <- cbind(tmplistDiagnostics[[l]],data.frame(StrengthIn = rep("-",Nodes)))
                }
    
                if(input$chkNODE_CENTRALITY_STRENGTH){
                    progress2 <- shiny::Progress$new(session)
                    on.exit(progress2$close())
                    progress2$set(message = paste('Current: Out-Strength...'), value = 0.5)

                    if(DIRECTED){
                        if(input$radMultiplexModel!="MULTIPLEX_IS_INTERDEPENDENT"){
                            createOctaveConfigFile()
                            #call octave
                            system("octave -qf octave/muxMultisliceCentralityOutDegree.m",intern=T)
                            #read output.
                            resultFile <- paste(input$txtProjectName,"_centrality_outdegree.txt",sep="")
                            centralityVector <- matrix(scan(resultFile, n = Nodes), ncol=1, nrow=Nodes, byrow = TRUE)
                            for(l in 1:LAYERS){
                                tmplistDiagnostics[[l]] <- cbind(tmplistDiagnostics[[l]],data.frame(StrengthOut = centralityVector))
                            }
                            if(file.exists(resultFile)) file.remove(resultFile)
            
                            resultFile <- paste(input$txtProjectName,"_centrality_outdegree_aggregate.txt",sep="")
                            centralityVector <- matrix(scan(resultFile, n = Nodes), ncol=1, nrow=Nodes, byrow = TRUE)
                            l <- LAYERS+1
                            tmplistDiagnostics[[l]] <- cbind(tmplistDiagnostics[[l]],data.frame(StrengthOut = centralityVector))
                            if(file.exists(resultFile)) file.remove(resultFile)
                        }else{
                            #for an interdependent network, it is enough to calculate centrality in the aggregate
                            #http://igraph.sourceforge.net/doc/R/graph.strength.html
                            centralityVector <- graph.strength(g[[LAYERS+1]],mode="out")
                            for(l in 1:(LAYERS+1)){
                                tmplistDiagnostics[[l]] <- cbind(tmplistDiagnostics[[l]],data.frame(StrengthOut = centralityVector))
                            }
                        }
                    }else{
                        for(l in 1:(LAYERS+1)){
                            tmplistDiagnostics[[l]] <- cbind(tmplistDiagnostics[[l]],data.frame(StrengthOut = tmplistDiagnostics[[l]]$Strength))
                        }
                    }

                    progress2$set(message = paste('Current: Out-Strength... Done!'), value = 1)
                    Sys.sleep(1)
                    progress2$close()
                }else{
                    for(l in 1:LAYERS){
                        tmplistDiagnostics[[l]] <- cbind(tmplistDiagnostics[[l]],data.frame(StrengthOut = rep("-",Nodes)))
                    }
                    l <- (LAYERS+1)
                    tmplistDiagnostics[[l]] <- cbind(tmplistDiagnostics[[l]],data.frame(StrengthOut = rep("-",Nodes)))
                }
                
                if(input$chkNODE_CENTRALITY_PAGERANK){
                    progress <- shiny::Progress$new(session)
                    on.exit(progress$close())
                    progress$set(message = paste('Current: PageRank...'), value = 0.5)

                    if(input$radMultiplexModel!="MULTIPLEX_IS_INTERDEPENDENT"){
                        createOctaveConfigFile()
                        #call octave
                        system("octave -qf octave/muxMultisliceCentralityPageRank.m",intern=T)
                        #read output.
                        resultFile <- paste(input$txtProjectName,"_centrality_pagerank.txt",sep="")
                        centralityVector <- matrix(scan(resultFile, n = Nodes), ncol=1, nrow=Nodes, byrow = TRUE)
                        centralityVector <- centralityVector/max(centralityVector)
                        for(l in 1:LAYERS){
                            tmplistDiagnostics[[l]] <- cbind(tmplistDiagnostics[[l]],data.frame(PageRank = centralityVector))
                        }
                        if(file.exists(resultFile)) file.remove(resultFile)
        
                        resultFile <- paste(input$txtProjectName,"_centrality_pagerank_aggregate.txt",sep="")
                        centralityVector <- matrix(scan(resultFile, n = Nodes), ncol=1, nrow=Nodes, byrow = TRUE)
                        centralityVector <- centralityVector/max(centralityVector)
                        l <- (LAYERS+1)
                        tmplistDiagnostics[[l]] <- cbind(tmplistDiagnostics[[l]],data.frame(PageRank = centralityVector))
                        if(file.exists(resultFile)) file.remove(resultFile)
                    }else{
                        #http://igraph.sourceforge.net/doc/R/page.rank.html
                        #for an interdependent network, it is enough to calculate centrality in the aggregate                        
                        centralityVector <- page.rank(g[[LAYERS+1]],directed=DIRECTED)$vector
                        centralityVector <- centralityVector/max(centralityVector)
                        for(l in 1:(LAYERS+1)){
                            tmplistDiagnostics[[l]] <- cbind(tmplistDiagnostics[[l]],data.frame(PageRank = centralityVector))
                        }
                    }                    
                    
                    progress$set(message = paste('Current: PageRank... Done!'), value = 1)
                    Sys.sleep(1)
                    progress$close()

                }else{
                    for(l in 1:LAYERS){
                        tmplistDiagnostics[[l]] <- cbind(tmplistDiagnostics[[l]],data.frame(PageRank = rep("-",Nodes)))
                    }
                    l <- (LAYERS+1)
                    tmplistDiagnostics[[l]] <- cbind(tmplistDiagnostics[[l]],data.frame(PageRank = rep("-",Nodes)))
                }
    
                if(input$chkNODE_CENTRALITY_EIGENVECTOR){
                    progress <- shiny::Progress$new(session)
                    on.exit(progress$close())
                    progress$set(message = paste('Current: Eigenvector...'), value = 0.5)

                    if(input$radMultiplexModel!="MULTIPLEX_IS_INTERDEPENDENT"){
                        #call octave
                        system("octave -qf octave/muxMultisliceCentralityEigenvector.m",intern=T)
                        #read output.
                        resultFile <- paste(input$txtProjectName,"_centrality_eigenvector.txt",sep="")
                        centralityVector <- matrix(scan(resultFile, n = Nodes), ncol=1, nrow=Nodes, byrow = TRUE)
                        centralityVector <- centralityVector/max(centralityVector)
                        if(any(centralityVector<0)){
                            print(paste("WARNING! Eigenvector centralities cannot be calculated. Assigning the same centrality to all nodes."))
                            progress$set(message = paste('Graph directed and acyclic or other problems...'), value = 0.5)
                            Sys.sleep(5)
                            centralityVector <- rep(1,Nodes)
                        }
                        for(l in 1:LAYERS){
                            tmplistDiagnostics[[l]] <- cbind(tmplistDiagnostics[[l]],data.frame(Eigenvector = centralityVector))
                        }
                        if(file.exists(resultFile)) file.remove(resultFile)
                        
                        resultFile <- paste(input$txtProjectName,"_centrality_eigenvector_aggregate.txt",sep="")
                        centralityVector <- matrix(scan(resultFile, n = Nodes), ncol=1, nrow=Nodes, byrow = TRUE)
                        centralityVector <- centralityVector/max(centralityVector)
                        if(any(centralityVector<0)){
                            print(paste("WARNING! Eigenvector centralities cannot be calculated. Assigning the same centrality to all nodes."))
                            progress$set(message = paste('Graph directed and acyclic or other problems...'), value = 0.5)
                            Sys.sleep(5)
                            centralityVector <- rep(1,Nodes)
                        }
    
                        l <- (LAYERS+1)
                        tmplistDiagnostics[[l]] <- cbind(tmplistDiagnostics[[l]],data.frame(Eigenvector = centralityVector))
                        if(file.exists(resultFile)) file.remove(resultFile)
                    }else{
                        #http://igraph.sourceforge.net/doc/R/evcent.html
                        #for an interdependent network, it is enough to calculate centrality in the aggregate
                        centralityVector <- evcent(g[[LAYERS+1]],directed=DIRECTED)$vector
                        centralityVector <- centralityVector/max(centralityVector)
                        if(any(is.null(centralityVector)) || any(is.nan(centralityVector)) || length(centralityVector)==0){
                            centralityVector <- rep(0,Nodes)
                        } 
                        for(l in 1:(LAYERS+1)){
                            tmplistDiagnostics[[l]] <- cbind(tmplistDiagnostics[[l]],data.frame(Eigenvector = centralityVector))
                        }
                    }
                                        
                    progress$set(message = paste('Current: Eigenvector... Done!'), value = 1)
                    Sys.sleep(1)
                    progress$close()
                }else{
                    for(l in 1:LAYERS){
                        tmplistDiagnostics[[l]] <- cbind(tmplistDiagnostics[[l]],data.frame(Eigenvector = rep("-",Nodes)))
                    }
                    l <- (LAYERS+1)
                    tmplistDiagnostics[[l]] <- cbind(tmplistDiagnostics[[l]],data.frame(Eigenvector = rep("-",Nodes)))
                }
    
                if(input$chkNODE_CENTRALITY_HUB){
                    progress <- shiny::Progress$new(session)
                    on.exit(progress$close())
                    progress$set(message = paste('Current: Hub...'), value = 0.5)

                    if(input$radMultiplexModel!="MULTIPLEX_IS_INTERDEPENDENT"){
                        #call octave
                        system("octave -qf octave/muxMultisliceCentralityHub.m",intern=T)
                        #read output.
                        resultFile <- paste(input$txtProjectName,"_centrality_hub.txt",sep="")
                        centralityVector <- matrix(scan(resultFile, n = Nodes), ncol=1, nrow=Nodes, byrow = TRUE)
                        centralityVector <- centralityVector/max(centralityVector)
                        for(l in 1:LAYERS){
                            tmplistDiagnostics[[l]] <- cbind(tmplistDiagnostics[[l]],data.frame(Hub = centralityVector))
                        }
                        if(file.exists(resultFile)) file.remove(resultFile)
        
                        resultFile <- paste(input$txtProjectName,"_centrality_hub_aggregate.txt",sep="")
                        centralityVector <- matrix(scan(resultFile, n = Nodes), ncol=1, nrow=Nodes, byrow = TRUE)
                        centralityVector <- centralityVector/max(centralityVector)
                        l <- (LAYERS+1)
                        tmplistDiagnostics[[l]] <- cbind(tmplistDiagnostics[[l]],data.frame(Hub = centralityVector))
                        if(file.exists(resultFile)) file.remove(resultFile)
                    }else{
                        #http://igraph.sourceforge.net/doc/R/kleinberg.html
                        #for an interdependent network, it is enough to calculate centrality in the aggregate
                        centralityVector <- hub.score(g[[LAYERS+1]],scale = TRUE)$vector
                        centralityVector <- centralityVector/max(centralityVector)
                        for(l in 1:(LAYERS+1)){
                            tmplistDiagnostics[[l]] <- cbind(tmplistDiagnostics[[l]],data.frame(Hub = centralityVector))
                        }
                    }                    
                    
                    progress$set(message = paste('Current: Hub... Done!'), value = 1)
                    Sys.sleep(1)
                    progress$close()
                }else{
                    for(l in 1:LAYERS){
                        tmplistDiagnostics[[l]] <- cbind(tmplistDiagnostics[[l]],data.frame(Hub = rep("-",Nodes)))
                    }
                    l <- LAYERS+1
                    tmplistDiagnostics[[l]] <- cbind(tmplistDiagnostics[[l]],data.frame(Hub = rep("-",Nodes)))
                }
    
                if(input$chkNODE_CENTRALITY_AUTHORITY){
                    progress <- shiny::Progress$new(session)
                    on.exit(progress$close())
                    progress$set(message = paste('Current: Authority...'), value = 0.5)

                    if(input$radMultiplexModel!="MULTIPLEX_IS_INTERDEPENDENT"){
                        #call octave
                        system("octave -qf octave/muxMultisliceCentralityAuthority.m",intern=T)
                        #read output.
                        resultFile <- paste(input$txtProjectName,"_centrality_authority.txt",sep="")
                        centralityVector <- matrix(scan(resultFile, n = Nodes), ncol=1, nrow=Nodes, byrow = TRUE)
                        centralityVector <- centralityVector/max(centralityVector)
                        for(l in 1:LAYERS){
                            tmplistDiagnostics[[l]] <- cbind(tmplistDiagnostics[[l]],data.frame(Authority = centralityVector))
                        }
                        if(file.exists(resultFile)) file.remove(resultFile)
        
                        resultFile <- paste(input$txtProjectName,"_centrality_authority_aggregate.txt",sep="")
                        centralityVector <- matrix(scan(resultFile, n = Nodes), ncol=1, nrow=Nodes, byrow = TRUE)
                        centralityVector <- centralityVector/max(centralityVector)
                        l <- (LAYERS+1)
                        tmplistDiagnostics[[l]] <- cbind(tmplistDiagnostics[[l]],data.frame(Authority = centralityVector))
                        if(file.exists(resultFile)) file.remove(resultFile)
                    }else{
                        #http://igraph.sourceforge.net/doc/R/kleinberg.html
                        #for an interdependent network, it is enough to calculate centrality in the aggregate
                        centralityVector <- authority.score(g[[LAYERS+1]],scale = TRUE)$vector
                        centralityVector <- centralityVector/max(centralityVector)
                        for(l in 1:(LAYERS+1)){
                            tmplistDiagnostics[[l]] <- cbind(tmplistDiagnostics[[l]],data.frame(Authority = centralityVector))
                        }
                    }                    
                    
                    progress$set(message = paste('Current: Authority... Done!'), value = 1)
                    Sys.sleep(1)
                    progress$close()
                }else{
                    for(l in 1:LAYERS){
                        tmplistDiagnostics[[l]] <- cbind(tmplistDiagnostics[[l]],data.frame(Authority = rep("-",Nodes)))
                    }
                    l <- (LAYERS+1)
                    tmplistDiagnostics[[l]] <- cbind(tmplistDiagnostics[[l]],data.frame(Authority = rep("-",Nodes)))
                }
    
                if(input$chkNODE_CENTRALITY_KATZ){
                    progress <- shiny::Progress$new(session)
                    on.exit(progress$close())
                    progress$set(message = paste('Current: Katz...'), value = 0.5)

                    if(input$radMultiplexModel!="MULTIPLEX_IS_INTERDEPENDENT"){
                        #call octave
                        system("octave -qf octave/muxMultisliceCentralityKatz.m",intern=T)
                        #read output.
                        resultFile <- paste(input$txtProjectName,"_centrality_katz.txt",sep="")
                        centralityVector <- matrix(scan(resultFile, n = Nodes), ncol=1, nrow=Nodes, byrow = TRUE)
                        centralityVector <- centralityVector/max(centralityVector)
                        for(l in 1:LAYERS){
                            tmplistDiagnostics[[l]] <- cbind(tmplistDiagnostics[[l]],data.frame(Katz = centralityVector))
                        }
                        if(file.exists(resultFile)) file.remove(resultFile)
        
                        resultFile <- paste(input$txtProjectName,"_centrality_katz_aggregate.txt",sep="")
                        centralityVector <- matrix(scan(resultFile, n = Nodes), ncol=1, nrow=Nodes, byrow = TRUE)
                        centralityVector <- centralityVector/max(centralityVector)
                        l <- (LAYERS+1)
                        tmplistDiagnostics[[l]] <- cbind(tmplistDiagnostics[[l]],data.frame(Katz = centralityVector))
                        if(file.exists(resultFile)) file.remove(resultFile)
                    }else{
                        #http://igraph.sourceforge.net/doc/R/alpha.centrality.html
                        #for an interdependent network, it is enough to calculate centrality in the aggregate
                        #It is easy to show that Katz centrality can be obtained from Bonacich centrality:
                        # v(katz) = v(bonacich) - vec(1)
                        #calculate the eigenvector centrality to obtain the leading eigenvalue

                        lambda <- evcent(g[[LAYERS+1]],directed=DIRECTED)$value
                        if(is.null(lambda) || is.nan(lambda) || is.infinite(lambda) || abs(lambda)<1e-8){
                            #use a lower bound:
                            #http://files.ele-math.com/articles/jmi-04-36.pdf
                            lambda <- sqrt(max(graph.strength(g[[LAYERS+1]],mode="total")))
                        }

                        centralityVector <- alpha.centrality(g[[LAYERS+1]], exo=1, alpha=0.99999/lambda) - 1
                        centralityVector <- centralityVector/max(centralityVector)
                        for(l in 1:(LAYERS+1)){
                            tmplistDiagnostics[[l]] <- cbind(tmplistDiagnostics[[l]],data.frame(Katz = centralityVector))
                        }
                    }                    
                    progress$set(message = paste('Current: Katz... Done!'), value = 1)
                    Sys.sleep(1)
                    progress$close()
                }else{
                    for(l in 1:LAYERS){
                        tmplistDiagnostics[[l]] <- cbind(tmplistDiagnostics[[l]],data.frame(Katz = rep("-",Nodes)))
                    }
                    l <- (LAYERS+1)
                    tmplistDiagnostics[[l]] <- cbind(tmplistDiagnostics[[l]],data.frame(Katz = rep("-",Nodes)))
                }

                if(input$chkNODE_CENTRALITY_MULTIPLEXITY){
                    progress <- shiny::Progress$new(session)
                    on.exit(progress$close())
                    progress$set(message = paste('Current: Multiplexity...'), value = 0.5)

                    if(input$radMultiplexModel!="MULTIPLEX_IS_INTERDEPENDENT"){
                        #call octave
                        system("octave -qf octave/muxMultisliceCentralityMultiplexity.m",intern=T)
                        #read output.
                        resultFile <- paste(input$txtProjectName,"_centrality_multiplexity.txt",sep="")
                        centralityVector <- matrix(scan(resultFile, n = Nodes), ncol=1, nrow=Nodes, byrow = TRUE)
                        #centralityVector <- centralityVector/max(centralityVector)
                        for(l in 1:LAYERS){
                            tmplistDiagnostics[[l]] <- cbind(tmplistDiagnostics[[l]],data.frame(Multiplexity = centralityVector))
                        }
                        if(file.exists(resultFile)) file.remove(resultFile)
        
                        #resultFile <- paste(input$txtProjectName,"_centrality_multiplexity_aggregate.txt",sep="")
                        #centralityVector <- matrix(scan(resultFile, n = Nodes), ncol=1, nrow=Nodes, byrow = TRUE)
                        #centralityVector <- centralityVector/max(centralityVector)
                        l <- (LAYERS+1)
                        centralityVector <- rep(0, Nodes)
                        tmplistDiagnostics[[l]] <- cbind(tmplistDiagnostics[[l]],data.frame(Multiplexity = centralityVector))
                        #if(file.exists(resultFile)) file.remove(resultFile)
                    }else{
                        centralityVector <- as.numeric(rowSums(table(  rbind(multilayerEdges[,1],multilayerEdges[,3]), rbind(multilayerEdges[,2],multilayerEdges[,4])  )>0))
                        
                        #for an interdependent network, node multiplexity should be 1
                        if(!all( centralityVector==1 )){
                            progress <- shiny::Progress$new(session)
                            on.exit(progress$close())
                            progress$set(message = 'Error! Multiplexity expected to be 1/# Layers for all nodes, but different values have been found. Are you sure your network is interdependent?', value = 0.5)
                            Sys.sleep(10)
                        }
                        centralityVector <- centralityVector/LAYERS

                        for(l in 1:(LAYERS+1)){
                            tmplistDiagnostics[[l]] <- cbind(tmplistDiagnostics[[l]],data.frame(Multiplexity = centralityVector))
                        }
                    }
                                        
                    progress$set(message = paste('Current: Multiplexity... Done!'), value = 1)
                    Sys.sleep(1)
                    progress$close()
                }else{
                    for(l in 1:LAYERS){
                        tmplistDiagnostics[[l]] <- cbind(tmplistDiagnostics[[l]],data.frame(Multiplexity = rep("-",Nodes)))
                    }
                    l <- (LAYERS+1)
                    tmplistDiagnostics[[l]] <- cbind(tmplistDiagnostics[[l]],data.frame(Multiplexity = rep("-",Nodes)))
                }
                
                if(input$chkNODE_CENTRALITY_KCORE){
                    progress <- shiny::Progress$new(session)
                    on.exit(progress$close())
                    progress$set(message = paste('Current: K-core...'), value = 0.5)

                    if(input$radMultiplexModel!="MULTIPLEX_IS_INTERDEPENDENT"){
                        #we can calculate this within R, no octave

                        #calculate centrality in each layer separately and then get the max per node
                        kcore.table <- matrix(0, nrow=Nodes, ncol=LAYERS)

                        for(l in 1:LAYERS){
                            kcore.table[,l] <- graph.coreness(g[[l]], mode="all")
                        }
                        
                        centralityVector <- apply(kcore.table, 1, max)
                        #centralityVector <- centralityVector/max(centralityVector)
                        for(l in 1:LAYERS){
                            tmplistDiagnostics[[l]] <- cbind(tmplistDiagnostics[[l]],data.frame(Kcore = centralityVector))
                        }
            
                        l <- (LAYERS+1)
                        centralityVector <- graph.coreness(g[[l]], mode="all")
                        #centralityVector <- centralityVector/max(centralityVector)
                        tmplistDiagnostics[[l]] <- cbind(tmplistDiagnostics[[l]],data.frame(Kcore = centralityVector))
                    }else{
                        #http://www.inside-r.org/packages/cran/igraph/docs/graph.coreness
                        #for an interdependent network, it is enough to calculate centrality in the aggregate

                        centralityVector <- graph.coreness(g[[LAYERS+1]], mode="all")
                        #centralityVector <- centralityVector/max(centralityVector)
                        for(l in 1:(LAYERS+1)){
                            tmplistDiagnostics[[l]] <- cbind(tmplistDiagnostics[[l]],data.frame(Kcore = centralityVector))
                        }
                    }                    
                    progress$set(message = paste('Current: K-core... Done!'), value = 1)
                    Sys.sleep(1)
                    progress$close()
                }else{
                    for(l in 1:LAYERS){
                        tmplistDiagnostics[[l]] <- cbind(tmplistDiagnostics[[l]],data.frame(Kcore = rep("-",Nodes)))
                    }
                    l <- (LAYERS+1)
                    tmplistDiagnostics[[l]] <- cbind(tmplistDiagnostics[[l]],data.frame(Kcore = rep("-",Nodes)))
                }

            }else{
                #calculation per layer. No needs to specify the weight attribute because the g objects
                #are built assuming weighted input (where weight is 1 for binary networks), and each measure
                #assume by default the weight attribute of E(g)
                for(l in 1:(LAYERS)){
                    tmplistDiagnostics[[l]] <- data.frame(Layer = rep(l,Nodes))
                    tmplistDiagnostics[[l]] <- cbind(tmplistDiagnostics[[l]],data.frame(Node = 1:Nodes))
                    tmplistDiagnostics[[l]] <- cbind(tmplistDiagnostics[[l]],data.frame(Label = nodesLabel[[l]]))
                }
    
                tmplistDiagnostics[[LAYERS+1]] <- data.frame(Layer = rep("Aggr",Nodes))
                tmplistDiagnostics[[LAYERS+1]] <- cbind(tmplistDiagnostics[[LAYERS+1]],data.frame(Node = 1:Nodes))
                tmplistDiagnostics[[LAYERS+1]] <- cbind(tmplistDiagnostics[[LAYERS+1]],data.frame(Label = nodesLabel[[LAYERS+1]]))

                                                        
                for(l in 1:(LAYERS+1)){
                    if(input$chkNODE_CENTRALITY_STRENGTH){
                        progress <- shiny::Progress$new(session)
                        on.exit(progress$close())
                        progress$set(message = paste('Current: Strength  for layer',l,'...'), value = 0.5)

                        #http://igraph.sourceforge.net/doc/R/graph.strength.html
                        tmplistDiagnostics[[l]] <- cbind(tmplistDiagnostics[[l]],data.frame(Strength = graph.strength(g[[l]],mode="total")))
                        if(DIRECTED){
                            tmplistDiagnostics[[l]] <- cbind(tmplistDiagnostics[[l]],data.frame(StrengthIn = graph.strength(g[[l]],mode="in")))
                            tmplistDiagnostics[[l]] <- cbind(tmplistDiagnostics[[l]],data.frame(StrengthOut = graph.strength(g[[l]],mode="out")))
                        }else{
                            tmplistDiagnostics[[l]] <- cbind(tmplistDiagnostics[[l]],data.frame(StrengthIn = tmplistDiagnostics[[l]]$Strength))
                            tmplistDiagnostics[[l]] <- cbind(tmplistDiagnostics[[l]],data.frame(StrengthOut = tmplistDiagnostics[[l]]$Strength))   
                        }
                        progress$set(message = paste('Current: Strength  for layer',l,'... Done!'), value = 1)
                        Sys.sleep(1)
                        progress$close()
                    }else{
                        tmplistDiagnostics[[l]] <- cbind(tmplistDiagnostics[[l]],data.frame(Strength = rep("-",Nodes)))
                        tmplistDiagnostics[[l]] <- cbind(tmplistDiagnostics[[l]],data.frame(StrengthIn = rep("-",Nodes)))
                        tmplistDiagnostics[[l]] <- cbind(tmplistDiagnostics[[l]],data.frame(StrengthOut = rep("-",Nodes)))
                    }
    
                    if(input$chkNODE_CENTRALITY_PAGERANK){
                        progress <- shiny::Progress$new(session)
                        on.exit(progress$close())
                        progress$set(message = paste('Current: PageRank  for layer',l,'...'), value = 0.5)

                        #http://igraph.sourceforge.net/doc/R/page.rank.html
                        tmplistDiagnostics[[l]] <- cbind(tmplistDiagnostics[[l]],data.frame(PageRank = page.rank(g[[l]],directed=DIRECTED)$vector))
                        tmplistDiagnostics[[l]]$PageRank <- tmplistDiagnostics[[l]]$PageRank/max(tmplistDiagnostics[[l]]$PageRank)
                        
                        progress$set(message = paste('Current: PageRank  for layer',l,'... Done!'), value = 1)
                        Sys.sleep(1)
                        progress$close()
                    }else{
                        tmplistDiagnostics[[l]] <- cbind(tmplistDiagnostics[[l]],data.frame(PageRank = rep("-",Nodes)))
                    }
    
                    if(input$chkNODE_CENTRALITY_EIGENVECTOR){
                        progress <- shiny::Progress$new(session)
                        on.exit(progress$close())
                        progress$set(message = paste('Current: Eigenvector  for layer',l,'...'), value = 0.5)

                        #http://igraph.sourceforge.net/doc/R/evcent.html
                        tmplistDiagnostics[[l]] <- cbind(tmplistDiagnostics[[l]],data.frame(Eigenvector = evcent(g[[l]],directed=DIRECTED)$vector))
                        tmplistDiagnostics[[l]]$Eigenvector <- tmplistDiagnostics[[l]]$Eigenvector/max(tmplistDiagnostics[[l]]$Eigenvector)
                        if(any(is.null(tmplistDiagnostics[[l]]$Eigenvector)) || any(is.nan(tmplistDiagnostics[[l]]$Eigenvector)) || length(tmplistDiagnostics[[l]]$Eigenvector)==0){
                            tmplistDiagnostics[[l]]$Eigenvector <- rep(0,Nodes)
                        } 
                        
                        progress$set(message = paste('Current: Eigenvector  for layer',l,'... Done!'), value = 1)
                        Sys.sleep(1)
                        progress$close()
                    }else{
                        tmplistDiagnostics[[l]] <- cbind(tmplistDiagnostics[[l]],data.frame(Eigenvector = rep("-",Nodes)))
                    }
    
                    if(input$chkNODE_CENTRALITY_HUB){
                        progress <- shiny::Progress$new(session)
                        on.exit(progress$close())
                        progress$set(message = paste('Current: Hub  for layer',l,'...'), value = 0.5)

                        #http://igraph.sourceforge.net/doc/R/kleinberg.html
                        tmplistDiagnostics[[l]] <- cbind(tmplistDiagnostics[[l]],data.frame(Hub = hub.score(g[[l]],scale = TRUE)$vector))
                        tmplistDiagnostics[[l]]$Hub <- tmplistDiagnostics[[l]]$Hub/max(tmplistDiagnostics[[l]]$Hub)
                        
                        progress$set(message = paste('Current: Hub  for layer',l,'... Done!'), value = 1)
                        Sys.sleep(1)
                        progress$close()
                    }else{
                        tmplistDiagnostics[[l]] <- cbind(tmplistDiagnostics[[l]],data.frame(Hub = rep("-",Nodes)))
                    }
    
                    if(input$chkNODE_CENTRALITY_AUTHORITY){
                        progress <- shiny::Progress$new(session)
                        on.exit(progress$close())
                        progress$set(message = paste('Current: Authority  for layer',l,'...'), value = 0.5)

                        #http://igraph.sourceforge.net/doc/R/kleinberg.html
                        tmplistDiagnostics[[l]] <- cbind(tmplistDiagnostics[[l]],data.frame(Authority = authority.score(g[[l]],scale = TRUE)$vector))
                        tmplistDiagnostics[[l]]$Authority <- tmplistDiagnostics[[l]]$Authority/max(tmplistDiagnostics[[l]]$Authority)
                        
                        progress$set(message = paste('Current: Authority  for layer',l,'... Done!'), value = 1)
                        Sys.sleep(1)
                        progress$close()
                    }else{
                        tmplistDiagnostics[[l]] <- cbind(tmplistDiagnostics[[l]],data.frame(Authority = rep("-",Nodes)))
                    }
    
                    if(input$chkNODE_CENTRALITY_KATZ){
                        progress <- shiny::Progress$new(session)
                        on.exit(progress$close())
                        progress$set(message = paste('Current: Katz  for layer',l,'...'), value = 0.5)

                        #http://igraph.sourceforge.net/doc/R/alpha.centrality.html
                        #It is easy to show that Katz centrality can be obtained from Bonacich centrality:
                        # v(katz) = v(bonacich) - vec(1)
                        #calculate the eigenvector centrality to obtain the leading eigenvalue
                        lambda <- evcent(g[[l]],directed=DIRECTED)$value
                        
                        if(is.null(lambda) || is.nan(lambda) || is.infinite(lambda) || abs(lambda)<1e-8){
                            #use a lower bound:
                            #http://files.ele-math.com/articles/jmi-04-36.pdf
                            lambda <- sqrt(max(graph.strength(g[[l]],mode="total")))
                        }
                        
                        tmplistDiagnostics[[l]] <- cbind(tmplistDiagnostics[[l]],data.frame(Katz = alpha.centrality(g[[l]],exo=1,alpha=0.99999/lambda) - 1))
                        tmplistDiagnostics[[l]]$Katz <- tmplistDiagnostics[[l]]$Katz/max(tmplistDiagnostics[[l]]$Katz)
                        
                        progress$set(message = paste('Current: Katz  for layer',l,'... Done!'), value = 1)
                        Sys.sleep(1)
                        progress$close()
                    }else{
                        tmplistDiagnostics[[l]] <- cbind(tmplistDiagnostics[[l]],data.frame(Katz = rep("-",Nodes)))
                    }

                    if(input$chkNODE_CENTRALITY_MULTIPLEXITY){
                        progress <- shiny::Progress$new(session)
                        on.exit(progress$close())
                        progress$set(message = paste('Current: Multiplexity  for layer',l,'...'), value = 0.5)

                        tmplistDiagnostics[[l]] <- cbind(tmplistDiagnostics[[l]],data.frame(Multiplexity = rep(0,Nodes)))
                        
                        progress$set(message = paste('Current: Multiplexity  for layer',l,'... Done!'), value = 1)
                        Sys.sleep(1)
                        progress$close()
                    }else{
                        tmplistDiagnostics[[l]] <- cbind(tmplistDiagnostics[[l]],data.frame(Multiplexity = rep("-",Nodes)))
                    }

                    if(input$chkNODE_CENTRALITY_KCORE){
                        progress <- shiny::Progress$new(session)
                        on.exit(progress$close())
                        progress$set(message = paste('Current: Kcore  for layer',l,'...'), value = 0.5)

                        centralityVector <- graph.coreness(g[[l]], mode="all")
                        #centralityVector <- centralityVector/max(centralityVector)
                        tmplistDiagnostics[[l]] <- cbind(tmplistDiagnostics[[l]],data.frame(Kcore = centralityVector))
                        
                        progress$set(message = paste('Current: Kcore  for layer',l,'... Done!'), value = 1)
                        Sys.sleep(1)
                        progress$close()
                    }else{
                        tmplistDiagnostics[[l]] <- cbind(tmplistDiagnostics[[l]],data.frame(Kcore = rep("-",Nodes)))
                    }

                }
                #progress$close()
            }
            
            return(tmplistDiagnostics)
        }
        
        #######################################
        ## Export
        #######################################
        
        #observe({
            #if(input$btnImportNetworks == 0 || LAYERS<=0) return()
            
            #if(btnSaveSessionValue==input$btnSaveSession) return()
            
            #isolate({
                #progress <- shiny::Progress$new(session)
                #on.exit(progress$close())
                #progress$set(message = 'Saving current session...', value = 0.05)
            #    print(ls(all.names = TRUE))
            #    save(list = ls(all.names = TRUE), file = "~/Desktop/test.RData", envir = .GlobalEnv)
                #progress$set(message = 'Session correctly saved!', value = 1)
                #Sys.sleep(5)
            #    btnSaveSessionValue <<- input$btnSaveSession
            #})

        #})
        
        observe({
            if(input$btnExportRendering==0 || input$btnRenderNetworks==0 || input$btnApplyLayout==0 || input$btnImportNetworks == 0 || LAYERS<=0) return()
    
            if(btnExportRenderingValue==input$btnExportRendering) return()
    
            isolate({
                progress <- shiny::Progress$new(session)
                on.exit(progress$close())
    
                progress$set(message = 'Start exporting...', value = 0.05)
                Sys.sleep(1)
    
                FILE_RGL_SNAPSHOT <- buildPath("export",paste(input$txtProjectName,"_",as.character(format(Sys.time(), "%d-%m-%Y_%H%M%S")),".png",sep=""))
                rgl.snapshot(FILE_RGL_SNAPSHOT) 
                                
                progress$set(message = paste('Image exported to',FILE_RGL_SNAPSHOT), value = 1)
                Sys.sleep(5)
    
                btnExportRenderingValue <<- input$btnExportRendering
            })
        })
        
        observe({
            if(input$btnExportRenderingPDF==0 || input$btnRenderNetworks==0 || input$btnApplyLayout==0 || input$btnImportNetworks == 0 || LAYERS<=0) return()
    
            if(btnExportRenderingPDFValue==input$btnExportRenderingPDF) return()
    
            isolate({
                progress <- shiny::Progress$new(session)
                on.exit(progress$close())
    
                progress$set(message = 'Start exporting...', value = 0.05)
                Sys.sleep(1)
    
                FILE_RGL_SNAPSHOT <- buildPath("export",paste(input$txtProjectName,"_",as.character(format(Sys.time(), "%d-%m-%Y_%H%M%S")),".pdf",sep=""))
                rgl.postscript(FILE_RGL_SNAPSHOT,"pdf",drawText=TRUE)

                progress$set(message = paste('Image exported to',FILE_RGL_SNAPSHOT), value = 1)
                Sys.sleep(5)
    
                btnExportRenderingPDFValue <<- input$btnExportRenderingPDF
            })
        })

        observe({
            if(input$btnExportRenderingSVG==0 || input$btnRenderNetworks==0 || input$btnApplyLayout==0 || input$btnImportNetworks == 0 || LAYERS<=0) return()
    
            if(btnExportRenderingSVGValue==input$btnExportRenderingSVG) return()
    
            isolate({
                progress <- shiny::Progress$new(session)
                on.exit(progress$close())
    
                progress$set(message = 'Start exporting...', value = 0.05)
                Sys.sleep(1)
    
                FILE_RGL_SNAPSHOT <- buildPath("export",paste(input$txtProjectName,"_",as.character(format(Sys.time(), "%d-%m-%Y_%H%M%S")),".svg",sep=""))
                rgl.postscript(FILE_RGL_SNAPSHOT,"svg",drawText=TRUE)

                progress$set(message = paste('Image exported to',FILE_RGL_SNAPSHOT), value = 1)
                Sys.sleep(5)
    
                btnExportRenderingSVGValue <<- input$btnExportRenderingSVG
            })
        })

    
        observe({
            if(input$btnExportRenderingWeb==0 || input$btnRenderNetworks==0 || input$btnApplyLayout==0 || input$btnImportNetworks == 0 || LAYERS<=0) return()
    
            if(btnExportRenderingWebValue==input$btnExportRenderingWeb) return()
    
            isolate({
                progress <- shiny::Progress$new(session)
                on.exit(progress$close())
    
                progress$set(message = 'Start exporting webGL...', value = 0.05)
                Sys.sleep(1)
    
                #browseURL(paste("file://", writeWebGL(dir=file.path("/path/", "webGL"), width=700), sep=""))
                writeWebGL(dir=buildPath("export", paste("webGL_",input$txtProjectName,sep="")), width=945)
                progress$set(message = 'webGL exported. See export folder.', value = 1)
                Sys.sleep(5)

                btnExportRenderingWebValue <<- input$btnExportRenderingWeb
            })
        })




        observe({
            if(input$btnExportRenderingClassicPDF==0 || input$btnRenderNetworks==0 || input$btnApplyLayout==0 || input$btnImportNetworks == 0 || LAYERS<=0) return()
    
            if(btnExportRenderingClassicPDFValue==input$btnExportRenderingClassicPDF) return()
    
            isolate({
                progress <- shiny::Progress$new(session)
                on.exit(progress$close())
    
                progress$set(message = 'Start exporting...', value = 0.05)
                Sys.sleep(1)
    
                FILE_OUTPUT <- buildPath("export",paste(input$txtProjectName,"_",as.character(format(Sys.time(), "%d-%m-%Y_%H%M%S")),".pdf",sep=""))

                width <- as.numeric(input$txtExportRenderingClassicPNGWidth)
                height <- as.numeric(input$txtExportRenderingClassicPNGHeight)

                pdf(file=FILE_OUTPUT, width=width, height=height)
                makeRendering()
                dev.off()
                
                progress$set(message = paste('Image exported to',FILE_OUTPUT), value = 1)
                Sys.sleep(5)
    
                btnExportRenderingClassicPDFValue <<- input$btnExportRenderingClassicPDF
            })
        })

        observe({
            if(input$btnExportRenderingClassicPNG==0 || input$btnRenderNetworks==0 || input$btnApplyLayout==0 || input$btnImportNetworks == 0 || LAYERS<=0) return()
    
            if(btnExportRenderingClassicPNGValue==input$btnExportRenderingClassicPNG) return()
    
            isolate({
                progress <- shiny::Progress$new(session)
                on.exit(progress$close())
    
                progress$set(message = 'Start exporting...', value = 0.05)
                Sys.sleep(1)
    
                FILE_OUTPUT <- buildPath("export",paste(input$txtProjectName,"_",as.character(format(Sys.time(), "%d-%m-%Y_%H%M%S")),".png",sep=""))

                width <- as.numeric(input$txtExportRenderingClassicPNGWidth)
                height <- as.numeric(input$txtExportRenderingClassicPNGHeight)
                dpi <- as.numeric(input$txtExportRenderingClassicPNGResolution)

                png(filename=FILE_OUTPUT, width=width, height=height, res=dpi)
                makeRendering()
                dev.off()

                progress$set(message = paste('Image exported to',FILE_OUTPUT), value = 1)
                Sys.sleep(5)
    
                btnExportRenderingClassicPNGValue <<- input$btnExportRenderingClassicPNG
            })
        })







        #this is to setup the pageable table output
        googleVisMotifsSummaryTableOptions <- reactive({
            #other options here:
            #http://www.inside-r.org/packages/cran/googleVis/docs/gvisTable
            if(input$btnCalculateMotifs==0 || input$btnImportNetworks == 0 || LAYERS<=1)
                return()
                
            list(
                page='enable',
                width=750,
                alternatingRowStyle = FALSE
            )
        })

        googleVisOverlapMatrixSummaryTableOptions <- reactive({
            #other options here:
            #http://www.inside-r.org/packages/cran/googleVis/docs/gvisTable
            if(input$btnCalculateCorrelationDiagnostics==0 || input$btnImportNetworks == 0 || LAYERS==0 || !input$chkMULTIPLEX_OVERLAPPING)
                return()
                
            list(
                page='disable',
                width=550,
                height=150
            )
        })
        
        #this is to setup the pageable table output
        googleVisInterPearsonSummaryTableOptions <- reactive({
            #other options here:
            #http://www.inside-r.org/packages/cran/googleVis/docs/gvisTable
            if(input$btnCalculateCorrelationDiagnostics==0 || input$btnImportNetworks == 0 || LAYERS==0 || !input$chkMULTIPLEX_INTERASSORTATIVITY_PEARSON)
                return()
                
            list(
                page='disable',
                width=550,
                height=150
            )
        })
    
        #this is to setup the pageable table output
        googleVisInterSpearmanSummaryTableOptions <- reactive({
            #other options here:
            #http://www.inside-r.org/packages/cran/googleVis/docs/gvisTable
            if(input$btnCalculateCorrelationDiagnostics==0 || input$btnImportNetworks == 0 || LAYERS==0 || !input$chkMULTIPLEX_INTERASSORTATIVITY_SPEARMAN)
                return()
                
            list(
                page='disable',
                width=550,
                height=150
            )
        })
        
        #this is to setup the pageable table output
        googleVisCommunitySummaryTableOptions <- reactive({
            #other options here:
            #http://www.inside-r.org/packages/cran/googleVis/docs/gvisTable
            if(input$btnCalculateCommunityDiagnostics==0 || input$btnImportNetworks == 0 || LAYERS==0)
                return()
                
            list(
                page='disable',
                width=550,
                height=150
    #            pageSize=10
            )
        })
        
        #this is to setup the pageable table output
        output$numOutputCommunityTableNodesPerPage <- renderUI({
            numericInput(inputId = "communityTablePageSize",label = "Nodes per page",Nodes)
        })

        googleVisCommunityTableOptions <- reactive({
            #other options here:
            #http://www.inside-r.org/packages/cran/googleVis/docs/gvisTable
            if(input$btnCalculateCommunityDiagnostics==0 || input$btnImportNetworks == 0 || LAYERS==0)
                return()
                
            if(is.null(input$communityTablePageSize)){
                list(
                    page=ifelse(input$communityTablePageable==TRUE,'enable','disable'),
                    pageSize=Nodes,
                    width=550
                )
            }else{
                list(
                    page=ifelse(input$communityTablePageable==TRUE,'enable','disable'),
                    pageSize=as.numeric(input$communityTablePageSize),
                    width=550
                )
            }
        })
    
        #this is to setup the pageable table output
        output$numOutputCentralityTableNodesPerPage <- renderUI({
            numericInput(inputId = "centralityTablePageSize",label = "Nodes per page",Nodes)
        })

        googleVisCentralityTableOptions <- reactive({
            #other options here:
            #http://www.inside-r.org/packages/cran/googleVis/docs/gvisTable
            if(input$btnCalculateCentralityDiagnostics==0 || input$btnImportNetworks == 0 || LAYERS==0)
                return()
            
            if(is.null(input$centralityTablePageSize)){
                list(
                    #page=ifelse(input$centralityTablePageable==TRUE,'enable','disable'),
                    pageSize=Nodes,
                    width=550
                )
            }else{
                list(
                    #page=ifelse(input$centralityTablePageable==TRUE,'enable','disable'),
                    pageSize=as.numeric(input$centralityTablePageSize),
                    width=550
                )                
            }
        })
    
        #this is to setup the pageable table output
        googleVisEdgelistTableOptions <- reactive({
            #other options here:
            #http://www.inside-r.org/packages/cran/googleVis/docs/gvisTable
            if(input$btnImportNetworks==0 || LAYERS==0)
                return()
                
            list(
                page=ifelse(input$edgelistTablePageable==TRUE,'enable','disable'),
                pageSize=input$edgelistTablePageSize,
                width=550
            )
        })

        #######################################
        ## Download handlers
        #######################################

        # downloadHandler() takes two arguments, both functions.
        #output$downSaveAs <- downloadHandler(    
        #    filename = function() { paste(input$txtProjectName, "csv", sep = ".") },
        #    content = function(file) { write.table(myData, file, sep = ";", row.names = FALSE) }
        #)

        output$downMotifsTable <- downloadHandler(    
            filename = function() { paste0(input$txtProjectName, "_motifs_table.csv") },
            content = function(file) { 

                if(input$btnCalculateMotifs==0 || input$btnImportNetworks == 0 ||  LAYERS<=1){
                    return(NULL)
                }else{
                    write.table(listMotifs, file, sep = ";", row.names = FALSE) 
                }
            }
        )
        
        output$downCentralityTable <- downloadHandler(    
            filename = function() { paste0(input$txtProjectName, "_centrality_table.csv") },
            content = function(file) { 

                if(input$btnCalculateCentralityDiagnostics==0 || input$btnImportNetworks == 0 || LAYERS==0){
                    return(NULL)
                }else{
                    write.table(listDiagnosticsMerge, file, sep = ";", row.names = FALSE) 
                }
            }
        )

        output$downCommunitySummaryTable <- downloadHandler(    
            filename = function() { paste0(input$txtProjectName, "_community_summary_table.csv") },
            content = function(file) { 

                if(input$btnCalculateCommunityDiagnostics==0 || input$btnImportNetworks == 0 || LAYERS==0){
                    return(NULL)
                }else{
                    write.table(sumCommunitiesMerge, file, sep = ";", row.names = FALSE) 
                }
            }
        )

        output$downCommunityTable <- downloadHandler(    
            filename = function() { paste0(input$txtProjectName, "_community_table.csv") },
            content = function(file) { 

                if(input$btnCalculateCommunityDiagnostics==0 || input$btnImportNetworks == 0 || LAYERS==0){
                    return(NULL)
                }else{
                    write.table(listCommunitiesMerge, file, sep = ";", row.names = FALSE) 
                }
            }
        )
        
        output$downOverlappingSummaryTable <- downloadHandler(    
            filename = function() { paste0(input$txtProjectName, "_overlap_table.csv") },
            content = function(file) { 

                if(input$btnCalculateCorrelationDiagnostics==0 || input$btnImportNetworks == 0 || LAYERS==0){
                    return(NULL)
                }else{
                    write.table(listOverlap, file, sep = ";", row.names = FALSE) 
                }
            }
        )

        output$downInterPearsonSummaryTable <- downloadHandler(    
            filename = function() { paste0(input$txtProjectName, "_interpearson_table.csv") },
            content = function(file) { 

                if(input$btnCalculateCorrelationDiagnostics==0 || input$btnImportNetworks == 0 || LAYERS==0){
                    return(NULL)
                }else{
                    write.table(listInterPearson, file, sep = ";", row.names = FALSE) 
                }
            }
        )

        output$downInterSpearmanSummaryTable <- downloadHandler(    
            filename = function() { paste0(input$txtProjectName, "_interspearman_table.csv") },
            content = function(file) { 

                if(input$btnCalculateCorrelationDiagnostics==0 || input$btnImportNetworks == 0 || LAYERS==0){
                    return(NULL)
                }else{
                    write.table(listInterSpearman, file, sep = ";", row.names = FALSE) 
                }
            }
        )


                    
        #######################################
        ## Simple interface with octave
        #######################################
    
        createOctaveConfigFile <- function(){
            if(LAYERS==0) return(NULL)
            
            octaveConfigFile <- "octave/muxOctaveConfig.m"
            
            write(paste("AnalysisName = \"",input$txtProjectName,"\";",sep=""),
                file=octaveConfigFile,append=F)
    
            if(input$radMultiplexModel=="MULTIPLEX_IS_EDGECOLORED"){
                write(paste0("isExtendedEdgesList = 0;"), file=octaveConfigFile,append=T)
                for(l in 1:LAYERS){
                    if(input$chkEdgeListLabel){
                        write(paste0("LayersList{",l,"}=\"",normalizePath(paste0(fileName[[l]][1],".rel")),"\";"),
                            file=octaveConfigFile,append=T)                    
                    }else{
                        write(paste0("LayersList{",l,"}=\"",normalizePath(fileName[[l]][1]),"\";"),
                            file=octaveConfigFile,append=T)
                    }
                }
            }else{
                write(paste0("isExtendedEdgesList = 1;"), file=octaveConfigFile,append=T) 
                if(input$chkEdgeListLabel){
                    write(paste0("MultiLayerEdgesListFile = \"",normalizePath(paste0(fileName[[1]][1],".rel")),"\";"),
                            file=octaveConfigFile,append=T)
                }else{
                    write(paste0("MultiLayerEdgesListFile = \"",normalizePath(fileName[[1]][1]),"\";"),
                            file=octaveConfigFile,append=T)
                }
            }
                
            if(!DIRECTED){
                flag <- "U"
                if(!input$chkEdgeListUndirectedBoth) flag <- "UD"
            }else{
                flag <- "D"
            }
            if(WEIGHTED) flag <- paste(flag,"W",sep="")
            write(paste("Flags = \"",flag,"\";",sep=""),
                file=octaveConfigFile,append=T)
    
            write(paste("MaxNodes = ",Nodes,";"),
                file=octaveConfigFile,append=T)
    
            write(paste("FirstNodeLabel = ",minNodeID,";"),
                file=octaveConfigFile,append=T)

            if(!is.null(input$txtGamma) && input$txtGamma!=""){
                #trick necessary because this value is hidden at the beginning and it would recognize its value
                #only if you pass from multislice community detection panel.. 
                write(paste("GammaParameter = ",input$txtGamma,";"),
                    file=octaveConfigFile,append=T)
            }else{
                write(paste("GammaParameter = ",1,";"),
                    file=octaveConfigFile,append=T)                
            }
                
            write(paste("OmegaParameter = ",input$txtOmega,";"),
                file=octaveConfigFile,append=T)
    
            write(paste("InterAssortativityType = \"",input$selAssortativityType,"\";",sep=""),
                file=octaveConfigFile,append=T)
    
            if(input$radMultiplexType=="MULTIPLEX_IS_ORDERED"){
                write(paste("MultisliceType = \"ordered\";"),
                    file=octaveConfigFile,append=T)
            }else if(input$radMultiplexType=="MULTIPLEX_IS_CATEGORICAL"){
                write(paste("MultisliceType = \"categorical\";"),
                    file=octaveConfigFile,append=T)                    
            }    
            
            return(TRUE)
        }
    
        #######################################
        ## External function for plots
        #######################################
        
        #to plot a matrix, as for the interassortativity, I use a piece of code apparently available for free from
        #http://www.phaget4.org/R/image_matrix.html
        #Author: Chris Seidel
        # ----- Define a function for plotting a matrix ----- #
        myImagePlot <- function(x, ...){
             min <- min(x,na.rm=T)
             max <- max(x,na.rm=T)
             yLabels <- rownames(x)
             xLabels <- colnames(x)
             title <-c()
             ColorRamp <- c()
          # check for additional function arguments
          if( length(list(...)) ){
            Lst <- list(...)
            if( !is.null(Lst$zlim) ){
               min <- Lst$zlim[1]
               max <- Lst$zlim[2]
            }
            if( !is.null(Lst$yLabels) ) yLabels <- c(Lst$yLabels)
            if( !is.null(Lst$xLabels) ) xLabels <- c(Lst$xLabels)
            if( !is.null(Lst$title) ) title <- Lst$title
            if( !is.null(Lst$ColorRamp) ) ColorRamp <- Lst$ColorRamp
          }
        # check for null values
        if( is.null(xLabels) ) xLabels <- c(1:ncol(x))
        if( is.null(yLabels) ) yLabels <- c(1:nrow(x))
        
        layout(matrix(data=c(1,2), nrow=1, ncol=2), widths=c(4,1), heights=c(1,1))
    
        if( is.null(ColorRamp) ){    
             # Red and green range from 0 to 1 while Blue ranges from 1 to 0
             ColorRamp <- rgb( seq(0,1,length=256),  # Red
                               seq(0,1,length=256),  # Green
                               seq(1,0,length=256))  # Blue
         }
         ColorLevels <- seq(min, max, length=length(ColorRamp))
        
         # Reverse Y axis
         reverse <- nrow(x) : 1
         yLabels <- yLabels[reverse]
         x <- x[reverse,]
        
         # Data Map
         par(mar = c(3,5,2.5,2))
         image(1:length(xLabels), 1:length(yLabels), t(x), col=ColorRamp, xlab="",
         ylab="", axes=FALSE, zlim=c(min,max))
         if( !is.null(title) ){
            title(main=title)
         }
         axis(BELOW<-1, at=1:length(xLabels), labels=xLabels, cex.axis=1)
         axis(LEFT <-2, at=1:length(yLabels), labels=yLabels, las= HORIZONTAL<-1,
         cex.axis=1)
        
         # Color Scale
         par(mar = c(3,2.5,2.5,2))
         image(1, ColorLevels,
              matrix(data=ColorLevels, ncol=length(ColorLevels),nrow=1),
              col=ColorRamp,
              xlab="",ylab="",
              xaxt="n")
    
         layout(1)
        }
        # ----- END plot function ----- #
    
    
        NormalizedShannonEntropy <- function(featureArray){
            #this assume as the number of bins the max value in featureArray
            B <- max(featureArray)
            N <- length(featureArray)
            H <- 0
            for(i in 1:B){
                p <- length(featureArray[featureArray==i])/N
                if(p>0) H <- H - p*log(p)
            }
            
            return(H/log(N))
        }
        
        KullbackLeiblerDivergence <- function(x,y){
            #this code is valid for this application, because I am assuming that the number of bins is
            #given by the max value in the arrays, which are integers and binned during pre-processing
            #note that the function is not symmetric!
            if(length(x)!=length(y)){
                stop("KLD: mismatching length of the two arrays")
            }
            B <- max(max(x,na.rm=T),max(y))
            N <- length(x)
            KLD <- 0
        
            for(i in 1:B){
                px <- length(x[x==i])/N
                py <- length(y[y==i])/N
                if(px>0 && py>0) KLD <- KLD + px*log(px/py)
            }
            
            return(KLD)
        }
        
        JensenShannonDivergence <- function(x,y){
            if(length(x)!=length(y)){
                stop("JSD: mismatching length of the two arrays")
            }
            B <- max(max(x,na.rm=T),max(y))
            N <- length(x)
            JSD <- 0
        
            for(i in 1:B){
                px <- length(x[x==i])/N
                py <- length(y[y==i])/N
                m <- 0.5*(px+py)
                if(px>0 && m>0) JSD <- JSD + 0.5*px*log(px/m)
                if(py>0 && m>0) JSD <- JSD + 0.5*py*log(py/m)
            }
        
            return( JSD )
        }
        
        GetCorrelationMatrix <- function(FeaturesDataFrame,method="spearman"){
            plotFeatures <- max(FeaturesDataFrame$feature)
        
            #build the correlation matrix for the measures
            correlationMatrix <- matrix(0,nrow=plotFeatures,ncol=plotFeatures)
                
            for(l in 1:plotFeatures){
                thisCluster <- FeaturesDataFrame$cluster[FeaturesDataFrame$feature==l]
        
                for(m in l:plotFeatures){
                    if(method=="pearson"){
                        correlationMatrix[l,m] <- cor(thisCluster,FeaturesDataFrame$cluster[FeaturesDataFrame$feature==m],use="all.obs", method="pearson")
                    }else if(method=="spearman"){
                        correlationMatrix[l,m] <- cor(thisCluster,FeaturesDataFrame$cluster[FeaturesDataFrame$feature==m],use="all.obs", method="spearman")                
                    }else if(method=="jsd"){
                        correlationMatrix[l,m] <- JensenShannonDivergence(thisCluster,FeaturesDataFrame$cluster[FeaturesDataFrame$feature==m]) 
                    }
                    correlationMatrix[m,l] <- correlationMatrix[l,m]
                }
        
                if(method=="pearson" || method=="spearman"){
                    correlationMatrix[l,l] <- 1
                }else if (method=="jsd"){
                    correlationMatrix[l,l] <- 0
                }
            }
            
            correlationMatrix[is.na(correlationMatrix)] <- 0
            
            return(correlationMatrix)
        }
        
        GetDistanceMatrix <- function(FeaturesDataFrame,method="spearman"){
            correlationMatrix <- GetCorrelationMatrix(FeaturesDataFrame,method)
            
            if(method=="pearson" || method=="spearman"){
                return( 1-correlationMatrix )
            }else if(method=="jsd"){
                return( sqrt(correlationMatrix) )
            }
        }
                
        annular_chart_ <- function (Nodes,Clusters, r1=1, r2=2, Border = NA, colorPalette) {
           stopifnot(Nodes>=0, r1 >= 0, r2 > 0, r1 < r2)
           #nodeOffset <- floor(max(Nodes)*0.05) + 1
           #x <- Nodes / (max(Nodes)+nodeOffset)
           #x <- c(0,x,rep(0,nodeOffset))
           #Clusters <- c(0,Clusters,rep(0,nodeOffset))
           x <- Nodes / (max(Nodes))
           x <- c(0,x)
           Clusters <- c(0,Clusters)
        
           for (i in 2:length(x)) {
             theta <- 2*pi*seq(x[i-1], x[i], length=100)
        
             polygon( c(r1 * cos(theta), r2 * cos(rev(theta))),
                      c(r1 * sin(theta), r2 * sin(rev(theta))),
                      col = colorPalette[Clusters[i]], border = Border )
           }
        }
        
        
        plotAnularViz <- function (FeaturesDataFrame, rCore=0.4,DisplacementFactor=0.05, Border = NA, sortbyFeatureID = 1, correlationMethod = "spearman", ShowLabels = T, Title = "") {
            myFontSize <- as.numeric(input$txtANULAR_VIZ_FONT_SIZE)
            
            print("### Annular Viz")
            
            #print(FeaturesDataFrame)
            
            #Expected format for data.frame: feature node cluster
            plotFeatures <- max(FeaturesDataFrame$feature)
            plotNodes <- max(FeaturesDataFrame$node)
            plotClusters <- max(FeaturesDataFrame$cluster)
            Displacement <- DisplacementFactor/plotFeatures
            
            plot.new()
            plot.window(xlim = c(-1,1), ylim = c(-1,1))


            #Set the palette
            #colorPalette <- rainbow(plotClusters+1,start=0.,end=0.8,alpha=0.9)[1:plotClusters]
            colorPalette <- colorRampPalette(brewer.pal(brewer.pal.info$maxcolors[row.names(brewer.pal.info)==input$selAnularColorPalette],input$selAnularColorPalette))(plotClusters)
               
            #plot the core
            #annular_chart_(1:plotNodes, AggregateCommunityDataFrame$cluster, rCore - 5*Displacement - ((1-rCore)/plotFeatures), rCore - 5*Displacement, Border, colorPalette)

            #plot the single-features
            if(nrow(FeaturesDataFrame)>0){
                image.plot(legend.only=TRUE, 1, 1:length(colorPalette),
                    z=matrix(data=1:length(colorPalette), ncol=length(colorPalette),nrow=1),
                    col=colorPalette,
                    xlab="",ylab="", xaxt="n", horizontal=T, legend.mar=4.1, cex=2*myFontSize)   

                entropyFactor <- vector()
                featureLabels <- vector()
                        
                for(l in 1:plotFeatures){
                    #calculate also the entropy of the distribution in the meanwhile
                    entropyFactor[l] <- 1-NormalizedShannonEntropy( FeaturesDataFrame$cluster[FeaturesDataFrame$feature==l] )

                    #assign the label
                    featureLabels[l] <- as.character(unique(FeaturesDataFrame$featurelabel[FeaturesDataFrame$feature==l]))
                }

                if(correlationMethod!="none"){
                    distanceMatrix <- GetDistanceMatrix(FeaturesDataFrame,correlationMethod)
                    myClustering <- hclust(as.dist(distanceMatrix),method="centroid")
                    featureOrdering <- myClustering$order
                }else{
                    featureOrdering <- 1:plotFeatures
                }

                #print(featureOrdering)
                if(sortbyFeatureID>0){
                    for(l in 1:plotFeatures){
                        if(l==sortbyFeatureID){
                            print(paste(" = Order by feature",l," ( ring",which(featureOrdering==l),")"))
                            idx <- sort.int(FeaturesDataFrame$cluster[FeaturesDataFrame$feature==l],index.return=T)$ix

                        }
                    }
                }else{
                    #we order by the feature with largest entropy, that is the min entropy factor
                    featureIndex <- which.min(entropyFactor)
                    print(paste(" = Order by maximum entropy feature",featureIndex," ( ring",which(featureOrdering==featureIndex),")"))
                    
                    idx <- sort.int(FeaturesDataFrame$cluster[FeaturesDataFrame$feature==featureIndex],index.return=T)$ix
                }
             
                for(l in 1:plotFeatures){
                    l2 <- featureOrdering[l]
                    thisCluster <- FeaturesDataFrame$cluster[FeaturesDataFrame$feature==l2]
                    #we reduce the thickness of the ring according to its information entropy: 
                    #smaller the entropy, smaller the thickness
                    print(paste(" == Feature",l2,": entropy =",1-entropyFactor[l2]))

                    annular_chart_(1:plotNodes, thisCluster[idx], rCore + ((1-rCore)/plotFeatures)*(l-1), rCore + ((1-rCore)/plotFeatures)*(l -0.5*entropyFactor[l2]) - Displacement, Border, colorPalette)
                    
                    if(l==plotFeatures && ShowLabels==T){
                        #show nodes label
                        x <- (1:plotNodes)/max(plotNodes)
                        x <- c(0,x)
                        for (i in 2:length(x)) {
                           theta <- 2*pi*(x[i-1] + x[i])*0.5
                           r <- (rCore + ((1-rCore)/plotFeatures)*(l -0.5*entropyFactor[l2])- Displacement)*1.04
                           text( r*cos(theta), r*sin(theta), label=as.character(idx[i-1]),cex = myFontSize, col="black" )
                        }
                    }
                }
        
                for(l in 1:plotFeatures){
                    l2 <- featureOrdering[l]
                    #print(paste(featureLabels[l],"-->",featureLabels[l2]))
                    mtext(paste(featureLabels[l2],l,""), side=3, line=2.2 - (l-1)*1.5*myFontSize, adj=1.0, cex=2*myFontSize, col="black")
                }
                mtext(Title, side=3, line=1., cex=3*myFontSize, col="black")       
            }    
        }
    }, warning = function(war) {
      # warning handler picks up where error was generated
      print(paste("Warning:  ",war))
        progress <- shiny::Progress$new(session)
        on.exit(progress$close())
        progress$set(message = paste('Warning! ',war), value = 0.5)
        Sys.sleep(10)
    }, error = function(err) {
      # error handler picks up where error was generated
      print(paste("Error:  ",err))
        progress <- shiny::Progress$new(session)
        on.exit(progress$close())
        progress$set(message = paste('Error! ',err), value = 0.5)
        Sys.sleep(10)
    }, finally = {
    }) #end tryCatch

})
