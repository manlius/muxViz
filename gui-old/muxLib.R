library(Matrix)
library(RSpectra)
library(tidyverse)

####################################################
# MuxNetLib: Library for Multilayer Network Analysis in muxViz
#
# Version: 0.1
# Last update: Dec 2017
# Authors: Manlio De Domenico
#
# History:
#
# Mar 2017: From Matlab to R!
# May 2014: First release, including part of muxNet
####################################################

#Good refs, to check in general:
#https://cran.r-project.org/doc/contrib/Hiebeler-matlabR.pdf
#http://mathesaurus.sourceforge.net/octave-r.html
#https://cran.r-project.org/web/packages/Matrix/Matrix.pdf


###################################################################
## MATLAB-like function
###################################################################

inv <- function(A){
    return( solve(A) )
}

kron <- function(A, B){
    #return the kronecker product between two matrices
    return( Matrix::kronecker(A, B) )
}

speye <- function(n){
    #return the identity matrix in sparse format
    return( Matrix::Diagonal(n,1) )
}

zeros <- function(n,m){
    #return a matrix full of zeros
    #return( Matrix::Matrix(Matrix::kronecker( cbind(rep(0,n)), rbind(rep(1,m)), sparse=T), sparse=T) )
    return(Matrix::Matrix(0, n, m, sparse=T))
}

ones <- function(n,m){
    #return a matrix full of ones
    #return( Matrix::Matrix(Matrix::kronecker( cbind(rep(1,n)), rbind(rep(1,m)) )) )
    return(Matrix::Matrix(1, n, m, sparse=T))
}

spcan <- function(n, i, j){
    #return a canonical matrix
    A <- zeros(n,n)
    A[i,j] <- 1
    return(A)
}

rand <- function(n,m){
    #return a matrix full of random numbers (uniformly distributed in [0,1])
    return( Matrix::Matrix( runif(n*m), n, m, sparse=T ) )
}

blkdiag <- function(...){
    #return a list of matrices in block diagonal form
    return( Matrix::bdiag(...) )
}

reshapeR <- function(A, n, m){
    #cannot call this function just "reshape", because it's an R builtin command
    #return a matrix reshaped according to dimension n and m
    dim(A) <- c(n,m)
    return( A )
}

diagR <- function(x, n, offset=0){ 
    #cannot call this function just "diag", because it's an R builtin command
    #return a matrix with elements on a diagonal specified by offset (0=main diagonal)
    M <- zeros(n,n)
    M[which(row(M)+offset == col(M))] <- x
    return(M)
}

traceR <- function(A){
    #return the trace of a matrix
    return(sum(diag(A)))
}

modR <- function(x,y){
    #return the reminder between x and y
    return( unlist(lapply(x, function(z) z %% y)) )
}

sumR <- function(A, n){
    #return the sum across rows (n=1) or columns (n=2) of a matrix
    if(n==1){
        return( rbind(colSums(A)) )
    }else if(n==2){
        return( cbind(rowSums(A)) )
    }else{
        stop("ERROR! Not a valid dimension.")
    }
}

###################################################################
## BASIC OPERATIONS
###################################################################


BuildLayersTensor <- function(Layers, OmegaParameter, MultisliceType){
    #Layers: scalar, number of layers
    #OmegaParameter: scalar, weight of links (future inter-layer links)
    #MultisliceType: "ordered" or "categorical"

    M <- Matrix(0, Layers, Layers, sparse=T)
    if(Layers>1){
        if(MultisliceType=="ordered"){
            M <- (diagR(ones(1,Layers-1),Layers,1) + diagR(ones(1,Layers-1),Layers,-1) )*OmegaParameter 
        }else if(MultisliceType=="categorical"){
            M <- (ones(Layers,Layers) - speye(Layers))*OmegaParameter
        }
    }else{
        M <- 0
        cat("--> Algorithms for one layer will be used\n")
    }

    return( M )
}

BuildSupraAdjacencyMatrixFromExtendedEdgelist <- function(mEdges, Layers, Nodes, isDirected){
    #mEdges: data frame with extended edge list
    #Layers and Nodes are scalars
    
    if( max(max(mEdges[,2]), max(mEdges[,4]))!=Layers ){
        stop("Error: expected number of layers does not match the data. Aborting process.")
    }
    
    edges <- data.frame(from=mEdges[,1] + Nodes*(mEdges[,2]-1),
                                      to=mEdges[,3] + Nodes*(mEdges[,4]-1),
                                      weight=mEdges[,5]
                                      )
    
    M <- Matrix::sparseMatrix(i=edges$from, j=edges$to, x = edges$weight, dims=c(Nodes*Layers,Nodes*Layers))
    
    return( M )
}

BuildSupraAdjacencyMatrixFromEdgeColoredMatrices <- function(NodesTensor, LayerTensor, Layers, Nodes){
    #NodesTensor: list of adjacency matrices, expected to be aligned (a node has same index in any layer)
    #LayerTensor: matrix
    #Layers and Nodes are scalars

    # Remind that AdjMatrix (that here would play the role of NodesTensor) is vector list with Layers+1 entries. 
    #Use AdjMatrix[1:LAYERS] to obtain the expected result

    Identity <- speye(Nodes)
    M <- blkdiag(NodesTensor) + kron(LayerTensor, Identity)

    return( M )
}

SupraAdjacencyToNodesTensor <- function(SupraAdjacencyMatrix, Layers, Nodes){
    #SupraAdjacencyMatrix: sparse matrix
    #Layers and Nodes are scalars
    
    # return the diagonal blocks from a supradajcency matrix

    return( lapply(1:Layers, function(x) SupraAdjacencyMatrix[(1+ (x-1)*Nodes):(x*Nodes), 
                                                                                                 (1+ (x-1)*Nodes):(x*Nodes)]) 
            )
}

SupraAdjacencyToBlockTensor <- function(SupraAdjacencyMatrix, Layers, Nodes){
    #SupraAdjacencyMatrix: sparse matrix
    #Layers and Nodes are scalars

    # return the the blocks from a supradajcency matrix

    #this is equivalent to Matlab's   BlockTensor = {}
    BlockTensor <- matrix(list(), Layers, Layers)

#    for(i in 1:Layers){
#        for(j in 1:Layers){
#            BlockTensor[[i,j]] <- SupraAdjacencyMatrix[(1+ (i-1)*Nodes):(i*Nodes),
#                                                                                 (1+ (j-1)*Nodes):(j*Nodes)]
#        }
#    }

    #this should be faster :-D
    lapply(1:Layers, function(i){
                                                lapply(1:Layers, function(j){
                                                    BlockTensor[[i,j]] <<- SupraAdjacencyMatrix[(1+ (i-1)*Nodes):(i*Nodes),
                                                                                                                         (1+ (j-1)*Nodes):(j*Nodes)]
                                                        })
                                                })
    
    return( BlockTensor )
}

GetAggregateMatrix <- function(NodesTensor, Layers, Nodes){
    #NodesTensor: list of adjacency matrices, expected to be aligned (a node has same index in any layer)
    #LayerTensor: matrix
    #Layers and Nodes are scalars

    Aggregate <- zeros(Nodes,Nodes)

    for(i in 1:Layers){
        Aggregate <- Aggregate + NodesTensor[[i]]
    }
    
    return(Aggregate)
}

GetAggregateMatrixFromNetworkList <- function(g.list){
    #g.list is a list of igraph objects
    
    W <- sparseMatrix( length(V(g.list[[1]])), length(V(g.list[[1]])))
    
    for(g in g.list){
        W <- W + igraph::get.adjacency(g)
    }
    
    return( W )
}

GetAggregateNetworkFromNetworkList <- function(g.list){
    #g.list is a list of igraph objects
    
    W <- sparseMatrix( length(V(g.list[[1]])), length(V(g.list[[1]])))
    
    for(g in g.list){
        W <- W + igraph::get.adjacency(g)
    }
    
    return( igraph::graph.adjacency(W, weighted=T) )
}


GetSampleMultiplex <- function(Layers, Nodes, p){
    NodeTensor <- lapply(1:Layers, function(x){
                                                                            A <- rand(Nodes,Nodes)
                                                                            A[which(A < p)] <- 1
                                                                            A[which(row(A)==col(A))] <- 0
                                                                            A[which(A < 1)] <- 0
                                                                            A[lower.tri(A)] <- 0
                                                                            A <- A + t(A)
                                                                            return(A)
                                })

    LayerTensor <- BuildLayersTensor(Layers, 1, "categorical")
    
    M <- BuildSupraAdjacencyMatrixFromEdgeColoredMatrices(NodeTensor, LayerTensor, Layers, Nodes)
    diag(M) <- 0

    return(Matrix::drop0(M))
}

SolveEigenvalueProblem <- function(Matrix){
    #return the matrix of eigenvectors (Q) and the diagonal matrix of eigenvalues (L)
    #as well as a vector with ordered eigenvalues (E)
    #Note that it is assumed that we work with real values (up to now, no applications
    #required to use complex numbers)
    #A warning is raised if complex numbers emerge...

    tmp <- eigen(Matrix)
    
    if(is.complex(tmp$vectors)){
        cat("  Warning! Complex eigenvectors. Using the real part.\n")
    }

    if(is.complex(tmp$values)){
        cat("  Warning! Complex eigenvalues. Using the real part.\n")
    }
    
    return( list( QMatrix=Re(tmp$vectors), 
                       LMatrix=Diagonal(Re(tmp$values), n=length(tmp$values)), 
                       Eigenvalues=cbind(sort(tmp$values))
                    ) )
}


GetLargestEigenv <- function(Matrix){
    #we must distinguish between symmetric and nonsymmetric matrices to have correct results
    #still to do for further improvements
    tmp <- RSpectra::eigs(Matrix, 1, which="LM")

    #The result of some computation might return complex numbers with 0 imaginary part
    #If this is the case, we fix it, otherwise a warning is rised
    if( all(Im(tmp$vectors)==0) ){
        tmp$vectors <- Re(tmp$vectors)
    }else{
        cat("Warning! Complex numbers in the leading eigenvector.\n")
    }

    if( all(Im(tmp$values)==0) ){
        tmp$values <- Re(tmp$values)
    }else{
        cat("Warning! Complex numbers in the leading eigenvalue.\n")
    }

    #check if the eigenvector has all negative components.. in that case we change the sign
    #first, set to zero everything that is so small that can create problems even if it compatible with zero
    tmp$vectors[which(tmp$vectors>-1e-12 & tmp$vectors<1e-12)] <- 0
    #now verify that all components are negative and change sign
    if( all( tmp$vectors[which(tmp$vectors!=0)] < 0 ) ){
        tmp$vectors <- -tmp$vectors
    }
    

    return( list(QMatrix=tmp$vectors, LMatrix=tmp$values) )

    ##remind to return a column vector result.. check always that returned result is compatible with original octave result
}

#GetLargestEigenv <- function(Matrix){
#    #we must distinguish between symmetric and nonsymmetric matrices to have correct results
#    tmp <- eigen(Matrix)
#    QMatrix <- tmp$vectors
#    LMatrix <- tmp$values
#
#    k <- which.max(abs(LMatrix))
#    LMatrix <- LMatrix[k]
#    QMatrix <- cbind(QMatrix[,k])   #return column vector
#
#    #The result of some computation might return complex numbers with 0 imaginary part
#    #If this is the case, we fix it, otherwise a warning is rised
#    if( all(Im(QMatrix)==0) ){
#        QMatrix <- Re(QMatrix)
#    }else{
#        cat("Warning! Complex numbers in the leading eigenvector.\n")
#    }
#
#    if( all(Im(LMatrix)==0) ){
#        LMatrix <- Re(LMatrix)
#    }else{
#        cat("Warning! Complex numbers in the leading eigenvalue.\n")
#    }
#
#    #check if the eigenvector has all negative components.. in that case we change the sign
#    #first, set to zero everything that is so small that can create problems even if it compatible with zero
#    QMatrix[which(QMatrix>-1e-12 & QMatrix<1e-12)] <- 0
#    #now verify that all components are negative and change sign
#    if( all( QMatrix[QMatrix!=0] < 0 ) ){
#        QMatrix <- -QMatrix
#    }
#    
#
#    return( list(QMatrix=QMatrix, LMatrix=LMatrix) )
#
#    ##remind to return a column vector result.. check always that returned result is compatible with original octave result
#}

binarizeMatrix <- function(A){
    #A is assumed to be sparse
    A[which(A != 0)] <- 1
    return( A )
}

#binarizeMatrix <- function(A){
#    return( Matrix::Matrix(as.numeric(A>0), dim(A)[1], dim(A)[2], sparse=T) )
#}


CanonicalVector <- function(N, i){
    vec <- zeros(1,N)
    vec[i] <- 1
    return(vec)
}

###################################################################
## REDUCIBILITY OF MULTILAYER NETWORKS
###################################################################

GetLaplacianMatrix <- function(AdjacencyMatrix){
    #Calculate the laplacian matrix from an adjacency matrix
    
    N <- dim(AdjacencyMatrix)[1]
    u <- ones(N,1)

    #laplacian
    LaplacianMatrix <- diagR(AdjacencyMatrix %*% u, N, 0) - AdjacencyMatrix
    
    #always check
    if(sum(LaplacianMatrix %*% u) > 1.e-8){
        stop("ERROR! The Laplacian matrix has rows that don't sum to 0. Aborting process.\n")
    }
    
    return(Matrix::drop0(LaplacianMatrix))
}


BuildDensityMatrixBGS <- function(AdjacencyMatrix){
    #Calculate the density matrix from an adjacency matrix
    #   References: 
    #   S. L. Braunstein, S. Ghosh, S. Severini, Annals of Combinatorics 10, No 3, (2006)
    #   M. De Domenico et al, Phys. Rev. X 3, 041022 (2013)
    
    DensityMatrix <- GetLaplacianMatrix(AdjacencyMatrix)

    #normalize to degree sum
    return( DensityMatrix/(traceR(DensityMatrix)) )
}

GetEigenvaluesOfDensityMatrix <- function(DensityMatrix){
    #Calculate the eigenvalues of a density matrix
    #   References: 
    #   M. De Domenico et al, Phys. Rev. X 3, 041022 (2013)

    Eigenvalues <- cbind(eigen(DensityMatrix)$values)

    #check that eigenvalues sum to 1
    if(abs(sum(Eigenvalues)-1)>1e-8){
        stop("ERROR! Eigenvalues dont sum to 1! Aborting process.")
    }
    
    return(Eigenvalues)
}

GetEigenvaluesOfDensityMatrixFromAdjacencyMatrix <- function(AdjacencyMatrix){
    DensityMatrix <- BuildDensityMatrixBGS(AdjacencyMatrix)
    return(GetEigenvaluesOfDensityMatrix(DensityMatrix))
}

GetRenyiEntropyFromAdjacencyMatrix <- function(AdjacencyMatrix, Q=1){
    #Calculate the quantum Renyi entropy of a network
    #   References: 
    #   M. De Domenico et al, Phys. Rev. X 3, 041022 (2013)
    #   M. De Domenico, V. Nicosia, A. Arenas, V. Latora, Nature Communications 6, 6864 (2015)

    Eigenvalues <- GetEigenvaluesOfDensityMatrixFromAdjacencyMatrix(AdjacencyMatrix)

    if (Q==1.){
        #Von Neuman quantum entropy
        RenyiEntropy <- -sum(Eigenvalues[Eigenvalues>0]*log(Eigenvalues[Eigenvalues>0]))
    }else{
        #Renyi quantum entropy
        RenyiEntropy <- (1 - sum(Eigenvalues[Eigenvalues>0]^Q))/(Q-1)
    }
    
    return(RenyiEntropy)
}

GetJensenShannonDivergence <- function(AdjacencyMatrix1,AdjacencyMatrix2,VNEntropy1,VNEntropy2){
    #Calculate the Jensen-Shannon Divergence of two networks
    #   References: 
    #   M. De Domenico, V. Nicosia, A. Arenas, V. Latora, Nature Communications 6, 6864 (2015)

#    %M = 0.5 * (RHO + SIGMA)
#    %JSD: 0.5 * DKL( RHO || M ) + 0.5 * DKL( SIGMA || M )
#    %DKL( A || B ) = tr[ A log A - A log B ] = -entropy(A) - tr[ A log B ]
#    %
#    %JSD: 0.5 * ( -entropy(RHO) - entropy(SIGMA) - tr[ RHO log M ] - tr[ SIGMA log M ] )
#    %         -0.5 * [ entropy(RHO) + entropy(SIGMA) ] - tr[ M log M ] )
#    %         -0.5 * [ entropy(RHO) + entropy(SIGMA) ] + entropy(M) 

    DensityMatrix1 <- BuildDensityMatrixBGS(AdjacencyMatrix1)
    DensityMatrix2 <- BuildDensityMatrixBGS(AdjacencyMatrix2)
    DensityMatrixM <- (DensityMatrix1 +DensityMatrix2)/2.
        
    EigenvaluesM <- eigen(DensityMatrixM)$values
    CrossEntropyM = -sum(EigenvaluesM[EigenvaluesM>0]*log(EigenvaluesM[EigenvaluesM>0]))
            
    JSD <- CrossEntropyM - 0.5*(VNEntropy1 + VNEntropy2)
    
    return(JSD)
}

###################################################################
## TOPOLOGICAL DESCRIPTORS OF MULTILAYER NETWORKS
###################################################################

GetGlobalNumberTriangles <- function(SupraAdjacencyMatrix,Layers,Nodes){
    #   References: 
    #   M. De Domenico et al, Phys. Rev. X 3, 041022 (2013)

    num <- traceR( (SupraAdjacencyMatrix %*% SupraAdjacencyMatrix) %*% SupraAdjacencyMatrix )
    
    return(num)
}

GetAverageGlobalClustering <- function(SupraAdjacencyMatrix,Layers,Nodes){
    #   References: 
    #   M. De Domenico et al, Phys. Rev. X 3, 041022 (2013)

    FMatrix <- ones(Nodes*Layers, Nodes*Layers) - speye(Nodes*Layers)
    num <- traceR( (SupraAdjacencyMatrix %*% SupraAdjacencyMatrix) %*% SupraAdjacencyMatrix )
    den <- traceR( (SupraAdjacencyMatrix %*% FMatrix) %*% SupraAdjacencyMatrix )
    
    return(num/(max(SupraAdjacencyMatrix)*den))
}

#Note: can be optimized
GetLocalClustering <- function(SupraAdjacencyMatrix,Layers,Nodes){
    #   References: 
    #   M. De Domenico et al, Phys. Rev. X 3, 041022 (2013)

    FMatrix <- ones(Nodes*Layers, Nodes*Layers) - speye(Nodes*Layers)
    M3 <- (SupraAdjacencyMatrix %*% SupraAdjacencyMatrix) %*% SupraAdjacencyMatrix
    F3 <- (SupraAdjacencyMatrix %*% FMatrix) %*% SupraAdjacencyMatrix
    
    
    blocks.num <- SupraAdjacencyToBlockTensor(M3, Layers, Nodes)
    blocks.den <- SupraAdjacencyToBlockTensor(F3, Layers, Nodes)
    
    B.num <- zeros(Nodes,Nodes)
    B.den <- zeros(Nodes,Nodes)
    for(i in 1:Layers){
        for(j in 1:Layers){ 
            B.num <- B.num + blocks.num[[i,j]] 
            B.den <- B.den + blocks.den[[i,j]] 
        }    
    }
    
    clus <- cbind(diag(B.num)/diag(B.den))
    
    if(any(clus>1 | clus <0)){
        stop("GetLocalClustering:ERROR! Impossible clustering coefficients. Aborting process.")
    }
    
    return( clus )
}

#Note: can be optimized
GetAverageGlobalOverlapping <- function(SupraAdjacencyMatrix,Layers,Nodes){
    #   References: 
    #   M. De Domenico, V. Nicosia, A. Arenas, V. Latora, Nature Communications 6, 6864 (2015)

    if(Layers==1){
        stop("GetAverageGlobalOverlapping:ERROR! At least two layers required. Aborting process.")
    }

    NodesTensor <- SupraAdjacencyToNodesTensor(SupraAdjacencyMatrix, Layers, Nodes)

    O <- pmin(NodesTensor[[1]],NodesTensor[[2]])
    NormTotal <- sum(sum(NodesTensor[[1]]))
    
    if(Layers > 2){
        #assuming that LayerTensor is an undirected clique
        for(l in 2:Layers){
            O <- pmin(O,NodesTensor[[l]])
            NormTotal <- NormTotal + sum(sum(NodesTensor[[l]]))
        }
    }
    
    AvGlobOverl <- Layers*sum(sum(O))/NormTotal
    
    if(sum(SupraAdjacencyMatrix-t(SupraAdjacencyMatrix))==0){
        AvGlobOverl <- AvGlobOverl/2
    }
    
    return(AvGlobOverl)
}

#Note: can be optimized
GetAverageGlobalNodeOverlappingMatrix <- function(SupraAdjacencyMatrix,Layers,Nodes){
    #   References: 
    #   M. De Domenico, V. Nicosia, A. Arenas, V. Latora, Nature Communications 6, 6864 (2015)

    if(Layers==1){
        stop("GetAverageGlobalNodeOverlappingMatrix:ERROR! At least two layers required. Aborting process.\n")
    }
    
    NodesTensor <- SupraAdjacencyToNodesTensor(SupraAdjacencyMatrix,Layers,Nodes)

    existingNodes <- vector("list", Layers)
    
    for(l in 1:Layers){
        #find cols and rows where sum > zero to identify connected nodes
        cols <- which(sumR(NodesTensor[[l]],2)!=0)
        rows <- which(sumR(NodesTensor[[l]],1)!=0)
        
        #merge the two (this approach is necessary to deal also with directed networks)
        existingNodes[[l]] <- union(cols, rows)
    }

    AvGlobOverlMatrix <- Matrix::Matrix(0, Layers, Layers, sparse=T)
    diag(AvGlobOverlMatrix) <- 1
    for(l1 in 1:(Layers-1)){
        for(l2 in (l1+1):Layers){
            AvGlobOverlMatrix[l1,l2] <- length(intersect( existingNodes[[l1]], existingNodes[[l2]] ))/Nodes;
            AvGlobOverlMatrix[l2,l1] <- AvGlobOverlMatrix[l1,l2]
        }
    }
    
    return(AvGlobOverlMatrix)
}

#Note: can be optimized
GetAverageGlobalOverlappingMatrix <- function(SupraAdjacencyMatrix,Layers,Nodes){
    #   References: 
    #   M. De Domenico, V. Nicosia, A. Arenas, V. Latora, Nature Communications 6, 6864 (2015)

    if(Layers==1){
        stop("GetAverageGlobalOverlappingMatrix:ERROR! At least two layers required. Aborting process.\n")
    }

    NodesTensor <- SupraAdjacencyToNodesTensor(SupraAdjacencyMatrix,Layers,Nodes)

    AvGlobOverlMatrix <- Matrix::Matrix(0, Layers, Layers, sparse=T)
    diag(AvGlobOverlMatrix) <- 1
    for(l1 in 1:(Layers-1)){
        Norm1 <- sum(sum(NodesTensor[[l1]]))
        for(l2 in (l1+1):Layers){
            O <- pmin(NodesTensor[[l1]], NodesTensor[[l2]])
            AvGlobOverlMatrix[l1,l2] <- 2*sum(sum(O))/(Norm1 + sum(sum(NodesTensor[[l2]])))
            AvGlobOverlMatrix[l2,l1] <- AvGlobOverlMatrix[l1,l2]
        }
    }
    
    return(AvGlobOverlMatrix)
}

GetMultiOutDegree <- function(SupraAdjacencyMatrix,Layers,Nodes,isDirected){
    #   References: 
    #   M. De Domenico et al, Phys. Rev. X 3, 041022 (2013)

    # Return the multi-out-degree, not accounting for interlinks
    NodesTensor <- SupraAdjacencyToNodesTensor(binarizeMatrix(SupraAdjacencyMatrix),Layers,Nodes)
    AggrMatrix <- GetAggregateMatrix(NodesTensor, Layers, Nodes)
    MultiOutDegreeVector <- sumR(AggrMatrix, 2)

    return(MultiOutDegreeVector)
}

GetMultiInDegree <- function(SupraAdjacencyMatrix,Layers,Nodes,isDirected){
    #   References: 
    #   M. De Domenico et al, Phys. Rev. X 3, 041022 (2013)

    # Return the multi-out-degree, not accounting for interlinks
    NodesTensor <- SupraAdjacencyToNodesTensor(binarizeMatrix(SupraAdjacencyMatrix),Layers,Nodes)
    AggrMatrix <- GetAggregateMatrix(NodesTensor, Layers, Nodes)
    MultiInDegreeVector <- sumR(AggrMatrix, 1)

    #return in column format
    return(t(MultiInDegreeVector))
}


GetMultiDegree <- function(SupraAdjacencyMatrix, Layers, Nodes, isDirected){
    #   References: 
    #   M. De Domenico et al, Phys. Rev. X 3, 041022 (2013)
    
    MultiInDegreeVector <- GetMultiInDegree(SupraAdjacencyMatrix, Layers, Nodes, isDirected)
    MultiOutDegreeVector <- GetMultiOutDegree(SupraAdjacencyMatrix, Layers, Nodes, isDirected)
    
    if(!isDirected){
        MultiDegreeVector <- (MultiInDegreeVector + MultiOutDegreeVector)/2
    }else{
        MultiDegreeVector <- MultiInDegreeVector + MultiOutDegreeVector
    }
    
    return(MultiDegreeVector)
}

#Note: can be optimized
GetInterAssortativityTensor <- function(SupraAdjacencyMatrix,Layers,Nodes,isDirected,Type){

    if(Layers==1){
        stop("GetInterAssortativityTensor: ERROR! At least two layers required. Aborting process.\n")
    }
    
    NodesTensor <- SupraAdjacencyToNodesTensor(SupraAdjacencyMatrix,Layers,Nodes)

    InterPearson <- speye(Layers)
    InterSpearman <- speye(Layers)

    if (Type=="IO" || Type=="OI"){
        InDegree <- vector("list", Layers)
        OutDegree <- vector("list", Layers)
    
        for(l in 1:Layers){
            InDegree[[l]] <- GetMultiInDegree(NodesTensor[[l]],1,Nodes,isDirected)
            OutDegree[[l]] <- GetMultiOutDegree(NodesTensor[[l]],1,Nodes,isDirected)
        }

        for(l1 in 1:Layers){
            for(l2 in 1:Layers){
                InterPearson[l1,l2] <- cor(x=InDegree[[l1]],OutDegree[[l2]], method="pearson")
                InterSpearman[l1,l2] <- cor(x=InDegree[[l1]],OutDegree[[l2]], method="spearman")
            }
        }

        if (Type=="OI"){
            InterPearson <- t(InterPearson)
            InterSpearman <- t(InterSpearman)
        }
    }else{
        Degree <- vector("list", Layers)
        
        if (Type=="OO"){
            for(l in 1:Layers){
                Degree[[l]] <- GetMultiOutDegree(NodesTensor[[l]],1,Nodes,isDirected)
            }
        }
    
        if (Type=="II"){
            for(l in 1:Layers){
                Degree[[l]] <- GetMultiInDegree(NodesTensor[[l]],1,Nodes,isDirected)
            }
        }
        
        if (Type=="TT"){
            for(l in 1:Layers){
                Degree[[l]] <- GetMultiDegree(NodesTensor[[l]],1,Nodes,isDirected)
            }
        }
        
        
        for(l1 in 1:(Layers-1)){
            for(l2 in (l1+1):Layers){
                InterPearson[l1,l2] <- cor(x=Degree[[l1]], y=Degree[[l2]], method="pearson")
                InterSpearman[l1,l2] <- cor(x=Degree[[l1]], y=Degree[[l2]], method="spearman")
                
                InterPearson[l2,l1] <- InterPearson[l1,l2]
                InterSpearman[l2,l1] <- InterSpearman[l1,l2]
            }
        }
    }

    return( list(InterPearson=InterPearson, InterSpearman=InterSpearman) )
}

GetMultiOutDegreeSum <- function(SupraAdjacencyMatrix,Layers,Nodes,isDirected){
    #this degree include multiple times the interlinks
    #   References: 
    #   M. De Domenico et al, Phys. Rev. X 3, 041022 (2013)

    SupraDegree <- sumR(binarizeMatrix(SupraAdjacencyMatrix),2)
    MultiOutDegreeVector <- sumR(reshapeR(SupraDegree,Nodes,Layers),2)
    
    return(MultiOutDegreeVector)
}

GetMultiInDegreeSum <- function(SupraAdjacencyMatrix,Layers,Nodes,isDirected){
    #this degree include multiple times the interlinks
    #   References: 
    #   M. De Domenico et al, Phys. Rev. X 3, 041022 (2013)
    
    SupraDegree <- t(sumR(binarizeMatrix(SupraAdjacencyMatrix),1))
    MultiInDegreeVector <- sumR(reshapeR(SupraDegree,Nodes,Layers),2)
    
    return(MultiInDegreeVector)
}

GetMultiDegreeSum <- function(SupraAdjacencyMatrix,Layers,Nodes,isDirected){
    #   References: 
    #   M. De Domenico et al, Phys. Rev. X 3, 041022 (2013)

    MultiInDegreeVector <- GetMultiInDegreeSum(SupraAdjacencyMatrix,Layers,Nodes,isDirected)
    MultiOutDegreeVector <- GetMultiOutDegreeSum(SupraAdjacencyMatrix,Layers,Nodes,isDirected)
    
    if(!isDirected){
        MultiDegreeVector <- (MultiInDegreeVector + MultiOutDegreeVector)/2
    }else{
        MultiDegreeVector <- MultiInDegreeVector + MultiOutDegreeVector
    }
    
    return(MultiDegreeVector)
}

GetMultiOutStrength <- function(SupraAdjacencyMatrix,Layers,Nodes,isDirected){
    #   References: 
    #   M. De Domenico et al, Phys. Rev. X 3, 041022 (2013)

    # Return the multi-out-Strength, not accounting for interlinks
    NodesTensor <- SupraAdjacencyToNodesTensor(SupraAdjacencyMatrix,Layers,Nodes)
    AggrMatrix <- GetAggregateMatrix(NodesTensor, Layers, Nodes)
    MultiOutStrengthVector <- sumR(AggrMatrix, 2)

    return(MultiOutStrengthVector)
}

GetMultiInStrength <- function(SupraAdjacencyMatrix,Layers,Nodes,isDirected){
    #   References: 
    #   M. De Domenico et al, Phys. Rev. X 3, 041022 (2013)

    # Return the multi-out-Strength, not accounting for interlinks
    NodesTensor <- SupraAdjacencyToNodesTensor(SupraAdjacencyMatrix,Layers,Nodes)
    AggrMatrix <- GetAggregateMatrix(NodesTensor, Layers, Nodes)
    MultiInStrengthVector <- sumR(AggrMatrix, 1)

    #return in column format
    return(t(MultiInStrengthVector))
}

GetMultiStrength <- function(SupraAdjacencyMatrix, Layers, Nodes, isDirected){
    #   References: 
    #   M. De Domenico et al, Phys. Rev. X 3, 041022 (2013)
    
    MultiInStrengthVector <- GetMultiInStrength(SupraAdjacencyMatrix, Layers, Nodes, isDirected)
    MultiOutStrengthVector <- GetMultiOutStrength(SupraAdjacencyMatrix, Layers, Nodes, isDirected)
    
    if(!isDirected){
        MultiStrengthVector <- (MultiInStrengthVector + MultiOutStrengthVector)/2
    }else{
        MultiStrengthVector <- MultiInStrengthVector + MultiOutStrengthVector
    }
    
    return(MultiStrengthVector)
}

GetMultiOutStrengthSum <- function(SupraAdjacencyMatrix,Layers,Nodes,isDirected){
    #this Strength include multiple times the interlinks
    #   References: 
    #   M. De Domenico et al, Phys. Rev. X 3, 041022 (2013)

    SupraStrength <- sumR(SupraAdjacencyMatrix,2)
    MultiOutStrengthVector <- sumR(reshapeR(SupraStrength,Nodes,Layers),2)
    
    return(MultiOutStrengthVector)
}

GetMultiInStrengthSum <- function(SupraAdjacencyMatrix,Layers,Nodes,isDirected){
    #this Strength include multiple times the interlinks
    #   References: 
    #   M. De Domenico et al, Phys. Rev. X 3, 041022 (2013)
    
    SupraStrength <- t(sumR(SupraAdjacencyMatrix,1))
    MultiInStrengthVector <- sumR(reshapeR(SupraStrength,Nodes,Layers),2)
    
    return(MultiInStrengthVector)
}

GetMultiStrengthSum <- function(SupraAdjacencyMatrix,Layers,Nodes,isDirected){
    #   References: 
    #   M. De Domenico et al, Phys. Rev. X 3, 041022 (2013)

    MultiInStrengthVector <- GetMultiInStrengthSum(SupraAdjacencyMatrix,Layers,Nodes,isDirected)
    MultiOutStrengthVector <- GetMultiOutStrengthSum(SupraAdjacencyMatrix,Layers,Nodes,isDirected)
    
    if(!isDirected){
        MultiStrengthVector <- (MultiInStrengthVector + MultiOutStrengthVector)/2
    }else{
        MultiStrengthVector <- MultiInStrengthVector + MultiOutStrengthVector
    }
    
    return(MultiStrengthVector)
}


GetMultiKatzCentrality <- function(SupraAdjacencyMatrix,Layers,Nodes){
    #   References: 
    #   M. De Domenico, A. Sole-Ribalta, E. Omodei, S. Gomez, A. Arenas, Nature Communications 6, 6868  (2015)

    # we pass the transpose of the transition matrix to get the left eigenvectors
    tmp <- GetLargestEigenv(t(SupraAdjacencyMatrix))
    LeadingEigenvalue <- tmp$LMatrix
   
    #Katz kernel tensor
    deltaTensor <- kron(speye(Nodes), speye(Layers))
    
    #this ensures convergence of the Katz kernel tensor
    a <- 0.99999/abs(LeadingEigenvalue)

    KatzKernelTensor <- inv(deltaTensor - a*SupraAdjacencyMatrix)

    KatzCentralitySupraVector <- KatzKernelTensor %*% ones(Nodes*Layers,1)
    CentralityVector <- sumR(reshapeR(KatzCentralitySupraVector,Nodes,Layers),2)
    CentralityVector <- CentralityVector/max(CentralityVector)

    return(CentralityVector)
}

GetMultiEigenvectorCentrality <- function(SupraAdjacencyMatrix,Layers,Nodes){
    # References: 
    #   M. De Domenico et al, Phys. Rev. X 3, 041022 (2013)
    #   M. De Domenico, A. Sole-Ribalta, E. Omodei, S. Gomez, A. Arenas, Nature Communications 6, 6868  (2015)
    
    #we pass the transpose of the transition matrix to get the left eigenvectors
    tmp <- GetLargestEigenv(t(SupraAdjacencyMatrix))
    LeadingEigenvector <- tmp$QMatrix

    CentralityVector <- sumR(reshapeR(LeadingEigenvector,Nodes,Layers),2)
    CentralityVector <- CentralityVector/max(CentralityVector)

    return(CentralityVector)
}

GetMultiHubCentrality <- function(SupraAdjacencyMatrix,Layers,Nodes){
    #see review http://arxiv.org/pdf/0805.3322v2.pdf
    #References: 
    #   M. De Domenico, A. Sole-Ribalta, E. Omodei, S. Gomez, A. Arenas, Nature Communications 6, 6868  (2015)
    
    #build the A A'
    SupraMatrix <- SupraAdjacencyMatrix %*% t(SupraAdjacencyMatrix)

    #we pass the matrix to get the right eigenvectors
    #to deal with the possible degeneracy of the leading eigenvalue, we add an eps to the matrix
    #this ensures that we can apply the Perron-Frobenius theorem to say that there is a unique
    #leading eigenvector. Here we add eps, a very very small number (<1e-8, generally)
    tmp <- GetLargestEigenv(SupraMatrix + 1e-16)
    LeadingEigenvector <- tmp$QMatrix
    
    CentralityVector <- sumR(reshapeR(LeadingEigenvector,Nodes,Layers),2)
    CentralityVector <- CentralityVector/max(CentralityVector)

    return(CentralityVector)
}

GetMultiAuthCentrality <- function(SupraAdjacencyMatrix,Layers,Nodes){
    #see review http://arxiv.org/pdf/0805.3322v2.pdf
    #   References: 
    #   M. De Domenico, A. Sole-Ribalta, E. Omodei, S. Gomez, A. Arenas, Nature Communications 6, 6868  (2015)

    #build the A' A
    SupraMatrix <- t(SupraAdjacencyMatrix) %*% SupraAdjacencyMatrix

    #we pass the matrix to get the right eigenvectors
    #to deal with the possible degeneracy of the leading eigenvalue, we add an eps to the matrix
    #this ensures that we can apply the Perron-Frobenius theorem to say that there is a unique
    #leading eigenvector. Here we add eps, a very very small number (<1e-8, generally)
    tmp <- GetLargestEigenv(SupraMatrix + 1e-16)
    LeadingEigenvector <- tmp$QMatrix
    
    CentralityVector <- sumR(reshapeR(LeadingEigenvector,Nodes,Layers),2)
    CentralityVector <- CentralityVector/max(CentralityVector)
    
    return(CentralityVector)
}

GetMultiKCoreCentrality <- function(SupraAdjacencyMatrix,Layers,Nodes){
    library(igraph)

    #calculate centrality in each layer separately and then get the max per node
    kcore.table <- matrix(0, nrow=Nodes, ncol=Layers)
    NodesTensor <- SupraAdjacencyToNodesTensor(SupraAdjacencyMatrix,Layers,Nodes)
    
    for(l in 1:Layers){
        g.tmp <- graph.adjacency(NodesTensor[[l]], weighted=T)
        kcore.table[,l] <- graph.coreness(g.tmp, mode="all")
    }
    
    CentralityVector <- apply(kcore.table, 1, max)
    return(CentralityVector)
}

GetMultiplexityCentrality <- function(SupraAdjacencyMatrix,Layers,Nodes){

    NodesTensor <- SupraAdjacencyToNodesTensor(SupraAdjacencyMatrix,Layers,Nodes)

    existingNodes <- vector("list", Layers)
    nodeMultiplexity <- rep(0, Nodes)

    for(l in 1:Layers){
        #find cols and rows where sum > zero to identify connected nodes
        cols <- which(sumR(NodesTensor[[l]],2)!=0)
        rows <- which(sumR(NodesTensor[[l]],1)!=0)
        
        #merge the two (this approach is necessary to deal also with directed networks)
        existingNodes[[l]] <- union(cols, rows)
        nodeMultiplexity[ existingNodes[[l]] ] <- nodeMultiplexity[ existingNodes[[l]] ] + 1
    }
    
    CentralityVector <- cbind(c(nodeMultiplexity))/Layers
    return(CentralityVector)
}

###################################################################
## RANDOM WALKS IN MULTILAYER NETWORKS
###################################################################

BuildSupraTransitionMatrixFromSupraAdjacencyMatrix <- function(SupraAdjacencyMatrix,Layers,Nodes){
    #   References: 
    #   M. De Domenico et al, Phys. Rev. X 3, 041022 (2013)
    #   M. De Domenico, A. Sole-Ribalta, S. Gomez, A. Arenas, PNAS 11, 8351 (2014) 

    Order <- Layers*Nodes
    #SupraUnitVector = ones(Order,1);

    SupraStrengthMatrix <- sumR(SupraAdjacencyMatrix,2)
    DisconnectedNodes <- length(SupraStrengthMatrix[SupraStrengthMatrix==0])
    
    if(DisconnectedNodes>0){
        cat(paste0(" #Trapping nodes (no outgoing-links): ", DisconnectedNodes, "\n"))
    }

    SupraStrengthMatrix[ SupraStrengthMatrix>0 ] <- 1./SupraStrengthMatrix[SupraStrengthMatrix>0]
    SupraStrengthMatrix <- diagR(SupraStrengthMatrix, Nodes*Layers, 0)    

    SupraTransitionMatrix <- SupraStrengthMatrix %*% SupraAdjacencyMatrix

    alpha <- 0.85
    
    if(DisconnectedNodes>0){
        #to normalize correctly in the case of nodes with no outgoing links:
        SupraTransitionMatrix[which(sumR(SupraTransitionMatrix,2)==0),] <- 1/Order
        SupraTransitionMatrix[which(sumR(SupraTransitionMatrix,2)!=0),] <- alpha * SupraTransitionMatrix[which(sumR(SupraTransitionMatrix,2)!=0),] + (1-alpha)/Order
    }else{
        SupraTransitionMatrix <- alpha * SupraTransitionMatrix + (1-alpha)/Order
    }
    
    #no more disconnected nodes
    DisconnectedNodes = 0;
        
    #check
    if(abs(sumR(sumR(SupraTransitionMatrix,2),1)-Order+DisconnectedNodes) > 1e-6){
        stop(paste("  BuildSupraTransitionMatrixFromSupraAdjacencyMatrix: ERROR! Problems in building the supra-transition matrix -> ",abs(sumR(sumR(SupraTransitionMatrix,2),1)-Order+DisconnectedNodes),". Aborting process."))
    }

    return(SupraTransitionMatrix)
}

GetMultiPageRankCentrality <- function(SupraAdjacencyMatrix,Layers,Nodes){
    #   References: 
    #   M. De Domenico, A. Sole-Ribalta, E. Omodei, S. Gomez, A. Arenas, Nature Communications 6, 6868  (2015)
    SupraTransitionMatrix <- BuildSupraTransitionMatrixFromSupraAdjacencyMatrix(SupraAdjacencyMatrix,Layers,Nodes)
    
    #we pass the transpose of the transition matrix to get the left eigenvectors
    tmp <- GetLargestEigenv(t(SupraTransitionMatrix))
    LeadingEigenvector <- tmp$QMatrix
    LeadingEigenvalue <- tmp$LMatrix
    
    if(abs(LeadingEigenvalue-1)>1e-6){
        stop(paste("  GetRWOverallOccupationProbability: ERROR! Expected leading eigenvalue equal to 1, obtained",LeadingEigenvalue, ". Aborting process."))
    }
    
    LeadingEigenvector <- LeadingEigenvector / sum(LeadingEigenvector)
    CentralityVector <- sumR(reshapeR(LeadingEigenvector,Nodes,Layers),2)
    CentralityVector <- CentralityVector/max(CentralityVector)

    return(CentralityVector)
}

###################################################################
## CONNECTED COMPONENTS IN MULTILAYER NETWORKS
###################################################################

GetConnectedComponents <- function(SupraAdjacencyMatrix,Layers,Nodes){
    # Return the component assignment for each node
    
    library(igraph)
    
    g <- as.undirected(graph.adjacency(SupraAdjacencyMatrix))
    clu <- components(g)
    Components <- as.numeric(clu$membership)
    ComponentsSize <- as.numeric(clu$csize)

    #first we have to check if the same entity is assigned to the same component
    #eg, if node A in layer 1 is assigned to component 1 and node A in layer 2 is assigned to component 2
    #then it makes no sense to collapse the information: if they are a unique entity, the nodes
    #should be assigned to the same component, and this happens if they are interconnected or
    #if some of the replicas are isolated components while the others are interconnected
    
    if (Layers > 1){
        newComponents <- rep(0,Nodes)
        for(n in 1:Nodes){
            comp <- Components[n]  #the component assigned to n in layer 1
            newComponents[n] <- comp
            
            for(l in 2:Layers){
                ctmp <- Components[ (l-1)*Nodes + n ]
                if(ctmp != comp){
                    #check if it is isolated
                    if( ComponentsSize[ctmp]!=1 && ComponentsSize[comp]!=1 ){
                        cat("  Impossible to find meaningful connected components\n")
                        cat(paste("  Node",n,"in layer 1 is in component",comp,"(size",ComponentsSize[comp],") while\n"))
                        cat(paste("  Node",n,"(abs id:",(l-1)*Nodes + n,") in layer",l,"is in component",ctmp,"(size",ComponentsSize[ctmp],")\n"))
                        cat("  Aborting process.\n")
                    }
                }
            }
        }
                
        Components <- rep(0,Nodes)
        comps <- unique(newComponents)
        
        #readjust the components label
        for(i in 1:length(comps)){
            Components[which(newComponents==comps[i])] <- i
        }           
    }

    return(Components)
}

GetGiantConnectedComponent <- function(SupraAdjacencyMatrix,Layers,Nodes){
    #Return the largest connected component

    Components <- GetConnectedComponents(SupraAdjacencyMatrix,Layers,Nodes)
    tmp <- table(Components)
    
    lcc.id <- as.numeric(names(tmp[which.max(tmp)]))

    lcc <- which(Components==lcc.id)

    return( lcc )
}

###################################################################
## REDUCIBILITY OF MULTILAYER NETWORKS
###################################################################

GetMultilayerReducibility <- function(SupraAdjacencyMatrix,Layers,Nodes,Method,Type){
    #Calculate the quantum Renyi entropy of a network
    #   References: 
    #   M. De Domenico et al, Phys. Rev. X 3, 041022 (2013)
    #   M. De Domenico, V. Nicosia, A. Arenas, V. Latora, Nature Communications 6, 6864 (2015)

    NodesTensor <- SupraAdjacencyToNodesTensor(SupraAdjacencyMatrix,Layers,Nodes)

    if(Layers > 1){
        #################################
        # VN Entropies
        #################################
        #single layer entropy
        SingleLayerEntropy <- rep(NA, Layers)
    
        for(i in 1:Layers){            
            SingleLayerEntropy[i] <- GetRenyiEntropyFromAdjacencyMatrix(NodesTensor[[i]],1)
            if(SingleLayerEntropy[i]<1e-12) SingleLayerEntropy[i] <- 0
            
            print(paste("DEBUG:", i, SingleLayerEntropy[i]))
        }
    
        ###########################
        #JSD
        ###########################
        JSD <- Matrix(0, Layers, Layers)
    
        if(Type=="Ordinal"){
            #this line is required, because a 0 means no distance..
            JSD <- Matrix(1, Layers, Layers)
            
            for(i in 1:(Layers-1)){
                j <- i + 1
                JSD[i,j] <- GetJensenShannonDivergence(NodesTensor[[i]], NodesTensor[[j]], 
                                                                                    SingleLayerEntropy[i],SingleLayerEntropy[j])
                #JSD[j,i] <- JSD[i,j]
            }  
        }
        if(Type=="Categorical"){
            for(i in 1:(Layers-1)){
                for(j in (i+1):Layers){
                    JSD[i,j] <- GetJensenShannonDivergence(NodesTensor[[i]], NodesTensor[[j]], 
                                                                                      SingleLayerEntropy[i],SingleLayerEntropy[j])
                    JSD[j,i] <- JSD[i,j]
                }
            }
        }
        JSD <- sqrt(JSD)

        #Quality function
        hc <- hclust(as.dist(JSD), method=Method)
    
        ## list of merging operations
        #see http://stackoverflow.com/questions/18215184/how-to-print-the-order-of-hierarchical-clustering-in-r

        MergeMatrix <- hc$merge
        
        #aggregate: I dont use the function in the library to avoid calculating again the nodesTensor
        AggregateMatrix <- NodesTensor[[1]]
        for(i in 2:Layers){
            AggregateMatrix <- AggregateMatrix + NodesTensor[[i]]
        }
        
        #aggregate entropy
        AggregateEntropy <- GetRenyiEntropyFromAdjacencyMatrix(AggregateMatrix,1)
        
        print(paste("DEBUG: Aggregate entropy", AggregateEntropy))
        print(MergeMatrix)
        
        #step zero: full colored-edge graph against fully aggregated network
        gQualityFunction <- rep(0, Layers)
        relgQualityFunction <- rep(0,Layers)

        cntCurrentLayers <- sum(SingleLayerEntropy>0)
        gQualityFunction[1] <- cntCurrentLayers*AggregateEntropy - 
                                             sum(SingleLayerEntropy[SingleLayerEntropy>0])
                        
        relgQualityFunction[1] <- gQualityFunction[1]/(cntCurrentLayers*AggregateEntropy)

        MergedTensor <- list()
        MergedEntropy <- rep(0,Layers)
        for(m in 1:nrow(MergeMatrix)){
            l.A <- MergeMatrix[m,1]
            l.B <- MergeMatrix[m,2]
            
            if(l.A<0){
                #it's an existing single layer
                A <- NodesTensor[[-l.A]]
                entropy.A <- SingleLayerEntropy[-l.A]
                SingleLayerEntropy[-l.A] <- 0
            }else{
                #it's an aggregated layer
                A <- MergedTensor[[l.A]]
                entropy.A <- MergedEntropy[l.A]
                MergedEntropy[l.A] <- 0
            }

            if(l.B<0){
                #it's an existing single layer
                B <- NodesTensor[[-l.B]]
                entropy.B <- SingleLayerEntropy[-l.B]
                SingleLayerEntropy[-l.B] <- 0
            }else{
                #it's an aggregated layer
                B <- MergedTensor[[l.B]]
                entropy.B <- MergedEntropy[l.B]
                MergedEntropy[l.B] <- 0
            }            
            
            cntCurrentLayers <- Layers - m
            
            MergedTensor[[m]] <- A + B
            MergedEntropy[m] <- GetRenyiEntropyFromAdjacencyMatrix(MergedTensor[[m]],1)
            if(MergedEntropy[m]<1e-12) MergedEntropy[m] <- 0
            
            gQualityFunction[m+1] <- cntCurrentLayers*AggregateEntropy - 
                                                     sum(SingleLayerEntropy[SingleLayerEntropy>0]) - 
                                                     sum(MergedEntropy[MergedEntropy>0])
            relgQualityFunction[m+1] <- gQualityFunction[m+1]/(cntCurrentLayers*AggregateEntropy)
            
            
        }
    }
    
#        MergedTensor <- list()
#        MergedEntropy <- list()
#        for(m in 1:(Layers-1)){
#            if(MergeMatrix[m,1]<0){
#                #we must use the network in NodesTensor
#                A <- NodesTensor[[ -MergeMatrix[m,1] ]]
#                entropyA <- SingleLayerEntropy[-MergeMatrix[m,1]]
#                SingleLayerEntropy[-MergeMatrix[m,1]] <- 0
#            }else{
#                #we must use the network stored in MergedTensor
#                A <- MergedTensor[[ MergeMatrix[m,1] ]]
#                entropyA <- MergedEntropy[[ MergeMatrix[m,1] ]]
#                MergedEntropy[[ MergeMatrix[m,1] ]] <- 0
#            }
#    
#            if(MergeMatrix[m,2]<0){
#                #we must use the network in NodesTensor
#                B <- NodesTensor[[ -MergeMatrix[m,2] ]]
#                entropyB <- SingleLayerEntropy[-MergeMatrix[m,2]]
#                SingleLayerEntropy[-MergeMatrix[m,2]] <- 0
#            }else{
#                #we must use the network stored in MergedTensor
#                B <- MergedTensor[[ MergeMatrix[m,2] ]]
#                entropyB <- MergedEntropy[[ MergeMatrix[m,2] ]]
#                MergedEntropy[[ MergeMatrix[m,2] ]] <- 0
#            }
#
#            MergedTensor[[m]] <- A + B
#            tmpLayerEntropy <- GetRenyiEntropyFromAdjacencyMatrix(MergedTensor[[m]],1)
#            MergedEntropy[[m]] <- tmpLayerEntropy
#            diffEntropy <- 2*tmpLayerEntropy - (entropyA + entropyB)
#            reldiffEntropy <- diffEntropy/(2*tmpLayerEntropy)
#            
#            cntCurrentLayers <- Layers - m
#
#            gQualityFunction[m+1] <- sum(SingleLayerEntropy[SingleLayerEntropy>0])
#
#            print(paste(cntCurrentLayers, sum(SingleLayerEntropy>0), tmpLayerEntropy , diffEntropy ))
#            #print(MergedTensor)
#            print(MergedEntropy)
#
#            for(i in 1:m){
#                # the current merge is accounted by position m
#                if(MergedEntropy[[i]]>0){
#                    #resetting the values as above, will guarantee that we consider only layers
#                    #that are still to be merged
#                    gQualityFunction[m+1] <- gQualityFunction[m+1] + MergedEntropy[[i]]
#                }
#            }
#            gQualityFunction[m+1] <- cntCurrentLayers*AggregateEntropy - gQualityFunction[m+1]
#            relgQualityFunction[m+1] <- gQualityFunction[m+1]/(cntCurrentLayers*AggregateEntropy)   
#        }
#    }

    return(list(JSD=JSD, gQualityFunction=gQualityFunction, relgQualityFunction=relgQualityFunction))
}