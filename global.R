##################################################
# Global variables
##################################################

#This is to avoid pushing a button and starting all the other ones..
btnQueryValue <- 0
btnRunConsoleValue <- 0
btnCalculateComponentsDiagnosticsValue <- 0
btnComponentsStatisticsValue <- 0
btnDiameterStatisticsValue <- 0
btnMeanPathLengthStatisticsValue <- 0
btnDensityStatisticsValue <- 0
btnNodeStatisticsValue <- 0
btnEdgeStatisticsValue <- 0
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
btnImportNodeColorValue <- 0

#Other variables
fileInput <- NULL
LAYERS <- 0
multilayerEdges <- NULL
isolatedNodes <- vector("list", LAYERS)
layerEdges <- vector("list",LAYERS+1)
fileName <- vector("list",LAYERS)
layerLabel <- vector("list",LAYERS+1)
layerColor <- vector("list",LAYERS)
layerColorAlpha <- vector("list",LAYERS)
layerLayoutFile <- vector("list",LAYERS)
layerLayout <- vector("list",LAYERS+1)
nodesLabel <- vector("list",LAYERS+1)
nodesLabel2 <- vector("list",LAYERS+1)
layout.non <- NULL
layerTable <- NULL
g <- vector("list",LAYERS+1)
g.multi <- NULL
layout.multi <- NULL
AdjMatrix.multi <- NULL
layouts <- vector("list",LAYERS+1)
AdjMatrix <- vector("list",LAYERS+1)

listDiagnostics <- data.frame()
listDiagnosticsSingleLayer <- data.frame()
listDiagnosticsMerge <- data.frame()
listDiagnosticsMergeSingleLayer <- data.frame()

listCommunities <- data.frame()
listCommunitiesSingleLayer <- data.frame()
listCommunitiesMerge <- data.frame()
listCommunitiesMergeSingleLayer <- data.frame()
sumCommunitiesMerge <- data.frame()
sumCommunitiesMergeSingleLayer <- data.frame()

listComponents <- data.frame()
listComponentsSingleLayer <- data.frame()
listComponentsMerge <- data.frame()
listComponentsMergeSingleLayer <- data.frame()
sumComponentsMerge <- data.frame()
sumComponentsMergeSingleLayer <- data.frame()

listDistanceSimilarity <- data.frame()
listInterPearson <- data.frame()
listInterSpearman <- data.frame()
listOverlap <- data.frame()
listNodeOverlap <- data.frame()
listMotifs <- data.frame()

listQueryResult <- data.frame()

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

externalNodeSizeFlag <- FALSE
externalNodeColorFlag <- FALSE
externalNodeColorTable <- NULL
nodeLabelSeqIdConvTable <- NULL

#==== Network type
DIRECTED <- F
WEIGHTED <- F

diagnosticsMultiplexOK <- F
diagnosticsSingleLayerOK <- F
diagnosticsOK <- F
communityOK <- F
componentsOK <- F
communityMultiplexOK <- F
communitySingleLayerOK <- F
componentsMultiplexOK <- F
componentsSingleLayerOK <- F

welcomeFunction <- function(){

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

octave.call <- function(arg){
    if( Sys.info()["sysname"]=="Windows" ){
        system(paste("octave-cli -qf", arg), intern=T)
    }else{
        system(paste("octave -qf", arg), intern=T)
    }
}


buildPath <- function(folder,objname){
    if( Sys.info()["sysname"]=="Windows" ){
        return( paste(getwd(),folder,objname,sep="\\") )
    }else{
        return( paste(getwd(),folder,objname,sep="/") )        
    }
}

buildTmpPath <- function(objname){
    return(  buildPath("tmp",objname) )
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

mdebug <- function(message){
    cat(paste("DEBUG:",message,"\n"))    
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
    th <- th*pi/180.
    if(th<0) th <- 2*pi + th
    Rx <- matrix(nrow=3,ncol=3,0)
    Rx[1,1] <- 1
    Rx[2,2] <- cos(th)
    Rx[3,3] <- cos(th)
    Rx[2,3] <- -sin(th)
    Rx[3,2] <- sin(th)
    return(Rx) 
}

Roty <- function(th){ 
    th <- th*pi/180.
    if(th<0) th <- 2*pi + th
    Ry <- matrix(nrow=3,ncol=3,0)
    Ry[1,1] <- cos(th)
    Ry[2,2] <- 1
    Ry[3,3] <- cos(th)
    Ry[1,3] <- sin(th)
    Ry[3,1] <- -sin(th)
    return(Ry) 
}

Rotz <- function(th){ 
    th <- th*pi/180.
    if(th<0) th <- 2*pi + th
    Rz<-matrix(nrow=3,ncol=3,0)
    Rz[1,1] <- cos(th)
    Rz[2,2] <- cos(th)
    Rz[3,3] <- 1
    Rz[1,2] <- -sin(th)
    Rz[2,1] <- sin(th)
    return(Rz) 
}

