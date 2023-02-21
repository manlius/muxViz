####################################################
# MuxNetLib: Library for Multilayer Network Analysis in muxViz
#
# Version: 3.0
# Last update: Jan 2021
# Authors: Manlio De Domenico
#
# History:
#
# Jan 2021: From R source file to an R package
# Mar 2017: From Matlab to R!
# May 2014: First release, including part of muxNet
####################################################

# Good refs, to check in general:
# https://cran.r-project.org/doc/contrib/Hiebeler-matlabR.pdf
# http://mathesaurus.sourceforge.net/octave-r.html
# https://cran.r-project.org/web/packages/Matrix/Matrix.pdf


###################################################################
## BASIC OPERATIONS
###################################################################

#' Build layers tensor
#'
#'
#' @param Layers scalar, number of layers, number of layers
#' @param OmegaParameter scalar, weight of links (future inter-layer links)
#' @param MultisliceType "ordered": chain, undirected; "categorical": all-to-all, undirected; "temporal": chain, directed
#' @return
#' Tensor matrix.
#' @export
BuildLayersTensor <-
  function(Layers, OmegaParameter, MultisliceType) {
    MultisliceType <- tolower(MultisliceType)
    M <- Matrix::Matrix(0, Layers, Layers, sparse = T)
    if (Layers > 1) {
      if (MultisliceType == "ordered") {
        M <-
          (diagR(ones(1, Layers - 1), Layers, 1) + diagR(ones(1, Layers - 1), Layers, -1)) *
          OmegaParameter
      } else if (MultisliceType == "categorical") {
        M <- (ones(Layers, Layers) - speye(Layers)) * OmegaParameter
      } else if (MultisliceType == "temporal") {
        M <- (diagR(ones(1, Layers - 1), Layers, 1)) * OmegaParameter
      }
    } else {
      M <- 0
      cat("--> Algorithms for one layer will be used\n")
    }

    return(M)
  }

#' Build supra-adjacency matrix from edge lists
#'
#'
#' @param mEdges data frame with extended edge list, i.e a data frame with
#'   columns: \code{node.from, layer.from, node.to, layer.to weight}
#' @param Layers scalar, number of layers
#' @param Nodes scalar, number of nodes
#' @param isDirected logical
#' @return
#' Supra-adjacency matrix, a square matrix of dimension \code{Nodes * Layers}.
#' @export
BuildSupraAdjacencyMatrixFromExtendedEdgelist <-
  function(mEdges, Layers, Nodes, isDirected) {

    if (max(max(mEdges[, 2]), max(mEdges[, 4])) != Layers) {
      stop("Error: expected number of layers does not match the data. Aborting process.")
    }

    edges <- data.frame(
      from = mEdges[, 1] + Nodes * (mEdges[, 2] - 1),
      to = mEdges[, 3] + Nodes * (mEdges[, 4] - 1),
      weight = mEdges[, 5]
    )

    M <-
      Matrix::sparseMatrix(
        i = edges$from,
        j = edges$to,
        x = edges$weight,
        dims = c(Nodes * Layers, Nodes * Layers)
      )

    if (sum(abs(M - Matrix::t(M))) > 1e-12 && isDirected == FALSE) {
      message(
        "WARNING: The input data is directed but isDirected=FALSE, I am symmetrizing by average."
      )
      M <- (M + Matrix::t(M)) / 2
    }
    return(M)
  }

#' Builds extended edgelist from supra-adjacency matrix
#'
#'
#' @param SupraAdjacencyMatrix the supra-adjacency matrix
#' @param Layers scalar, number of layers
#' @param Nodes scalar, number of nodes
#' @param isDirected logical
#' @return
#' Data frame with columns: \code{node.from, layer.from, node.to, layer.to weight}
#' @export
BuildExtendedEdgelistFromSupraAdjacencyMatrix <-
  function(SupraAdjacencyMatrix,
           Layers,
           Nodes,
           isDirected) {
    if (sum(abs(SupraAdjacencyMatrix - Matrix::t(SupraAdjacencyMatrix))) > 1e-12 &&
        isDirected == FALSE) {
      message(
        "WARNING: The input data is directed but isDirected=FALSE, I am symmetrizing by average."
      )
      SupraAdjacencyMatrix <-
        (SupraAdjacencyMatrix + Matrix::t(SupraAdjacencyMatrix)) / 2
    }

    #Convert the supra-adjacency to a data.frame
    dfM <- Matrix::summary(SupraAdjacencyMatrix)

    #transform to layers and nodes
    node.from <- (dfM$i - 1) %% Nodes + 1
    node.to <- (dfM$j - 1) %% Nodes + 1
    layer.from <- 1 + floor((dfM$i - 1) / Nodes)
    layer.to <- 1 + floor((dfM$j - 1) / Nodes)
    weight <- dfM$x

    mEdges <- data.frame(
      node.from = node.from,
      layer.from = layer.from,
      node.to = node.to,
      layer.to = layer.to,
      weight = weight
    )

    return(mEdges)
  }


#' Build supra-adjacency matrix from edge-colored matrices
#'
#'
#' @param NodesTensor list of adjacency matrices, expected to be aligned (a node hassame index in any layer)
#' @param LayerTensor matrix
#' @param Layers scalar, number of layers
#' @param Nodes scalar, number of nodes
#' @return a list of matrices. Let us call \code{AdjMatrix} the output of the function,
#'   AdjMatrix (that here would play the role of NodesTensor) is vectorlist with
#'   \code{Layers + 1} entries. Use \code{AdjMatrix[1:Layers]} to obtain the expected result.
#' @export
BuildSupraAdjacencyMatrixFromEdgeColoredMatrices <-
  function(NodesTensor, LayerTensor, Layers, Nodes) {
    Identity <- speye(Nodes)
    M <- blkdiag(NodesTensor) + kron(LayerTensor, Identity)
    return(M)
  }

#' Return nodes tensor from supra-adjacency matrix
#'
#'
#' @param SupraAdjacencyMatrix sparse matrix
#' @param Layers scalar, number of layers
#' @param Nodes scalar, number of nodes
#' @return
#' The diagonal blocks from a supra-adjacency matrix.
#' @export
SupraAdjacencyToNodesTensor <-
  function(SupraAdjacencyMatrix, Layers, Nodes) {
    return(lapply(1:Layers, function(x)
      SupraAdjacencyMatrix[(1 + (x - 1) * Nodes):(x * Nodes),
                           (1 + (x - 1) *
                              Nodes):(x * Nodes)]))
  }

#' Return block tensor from supra-adjacency matrix
#'
#'
#' @param SupraAdjacencyMatrix sparse matrix
#' @param Layers scalar, number of layers
#' @param Nodes scalar, number of nodes
#' @return
#' The blocks from input supra-adjacency matrix
#' @export
SupraAdjacencyToBlockTensor <-
  function(SupraAdjacencyMatrix, Layers, Nodes) {
    #this is equivalent to Matlab's   BlockTensor = {}
    BlockTensor <- matrix(list(), Layers, Layers)

    lapply(1:Layers, function(i) {
      lapply(1:Layers, function(j) {
        BlockTensor[[i, j]] <<-
          SupraAdjacencyMatrix[(1 + (i - 1) * Nodes):(i * Nodes),
                               (1 + (j - 1) * Nodes):(j *
                                                        Nodes)]
      })
    })

    return(BlockTensor)
  }

#' Get aggregate matrix from nodes tensor
#'
#'
#' @param NodesTensor list of adjacency matrices, expected to be aligned (a node has same index in any layer)
#' @param Layers scalar, number of layers
#' @param Nodes scalar, number of nodes
#' @return Aggregate matrix
#' @export
GetAggregateMatrix <- function(NodesTensor, Layers, Nodes) {
  Aggregate <- zeros(Nodes, Nodes)

  for (i in 1:Layers) {
    Aggregate <- Aggregate + NodesTensor[[i]]
  }

  return(Aggregate)
}

#' Get aggregate matrix from supra-adjacency matrix
#'
#'
#' @param SupraAdjacencyMatrix the supra-adjacency matrix
#' @param Layers scalar, number of layers
#' @param Nodes scalar, number of nodes
#' @return Aggregate matrix
#' #' @export
GetAggregateMatrixFromSupraAdjacencyMatrix <-
  function(SupraAdjacencyMatrix, Layers, Nodes) {
    NodesTensor <-
      SupraAdjacencyToNodesTensor(SupraAdjacencyMatrix, Layers, Nodes)
    Aggregate <- GetAggregateMatrix(NodesTensor, Layers, Nodes)

    return(Aggregate)
  }


#' Get aggregate network from supra-adjacency matrix
#'
#'
#' @param SupraAdjacencyMatrix supra-adjacency matrix
#' @param Layers scalar, number of layers
#' @param Nodes scalar, number of nodes
#' @return Aggregate network
#' @export
GetAggregateNetworkFromSupraAdjacencyMatrix <-
  function(SupraAdjacencyMatrix, Layers, Nodes) {
    Aggregate <-
      GetAggregateMatrixFromSupraAdjacencyMatrix(SupraAdjacencyMatrix, Layers, Nodes)

    if (sum(abs(Aggregate - Matrix::t(Aggregate))) == 0) {
      return(igraph::as.undirected(igraph::graph.adjacency(Aggregate, weighted =
                                                             T)))
    } else {
      return(igraph::graph.adjacency(Aggregate, weighted = T))
    }
  }

#' Get aggregate Matrix from network List
#'
#'
#' @param g.list list of igraph objects
#' @return Aggregate matrix
#' @export
GetAggregateMatrixFromNetworkList <- function(g.list) {
  W <- Matrix::sparseMatrix(
    igraph::gorder(g.list[[1]]),
    igraph::gorder(g.list[[1]])
  )

  for (g in g.list) {
    if (!is.null(igraph::E(g)$weight)) {
      W <- W + igraph::as_adjacency_matrix(g, attr = "weight")
    } else {
      W <- W + igraph::as_adjacency_matrix(g)
    }
  }
  return(W)
}


#' Get aggregate Network from network List
#'
#'
#' @param g.list list of igraph objects
#' @return Aggregate network
#' @export
GetAggregateNetworkFromNetworkList <- function(g.list) {
  W <- Matrix::sparseMatrix(
    igraph::gorder(g.list[[1]]),
    igraph::gorder(g.list[[1]])
  )

  for (g in g.list) {
    if (!is.null(igraph::E(g)$weight)) {
      W <- W + igraph::as_adjacency_matrix(g, attr = "weight")
    } else {
      W <- W + igraph::as_adjacency_matrix(g)
    }
  }

  if (sum(W - Matrix::t(W)) == 0) {
    return(igraph::as.undirected(igraph::graph.adjacency(W, weighted = T)))
  } else {
    return(igraph::graph.adjacency(W, weighted = T))
  }
}


#' Get a sample of a Multiplex
#'
#'
#' @param Layers scalar, number of layers, number of layers
#' @param Nodes scalar, number of nodes
#' @param p scalar, parameter to build the sample
#' @return Adjacency Matrix of the Layer and nodes indicated
#' @export
GetSampleMultiplex <- function(Layers, Nodes, p) {
  NodeTensor <- lapply(1:Layers, function(x) {
    A <- rand(Nodes, Nodes)
    A[Matrix::which(A < p)] <- 1
    A[Matrix::which(row(A) == col(A))] <- 0
    A[Matrix::which(A < 1)] <- 0
    A[lower.tri(A)] <- 0
    A <- A + Matrix::t(A)
    return(A)
  })

  LayerTensor <- BuildLayersTensor(Layers, 1, "categorical")

  M <-
    BuildSupraAdjacencyMatrixFromEdgeColoredMatrices(NodeTensor, LayerTensor, Layers, Nodes)
  diag(M) <- 0

  return(Matrix::drop0(M))
}


#' Return the matrix of eigenvectors (Q) and the diagonal matrix of eigenvalues (L) as well as a vector with ordered eigenvalues (E)
# 'Note that it is assumed that we work with real values (up to now, no applications required to use complex numbers). A warning is raised if complex numbers emerge...
#'
#'
#' @param Matrix matrix
#' @return Matrix of eigenvectors (Q),
#'   Diagonal matrix of eigenvalues (L), and vector with ordered eigenvalues (E)
#' @export
SolveEigenvalueProblem <- function(Matrix) {
  tmp <- eigen(Matrix)

  if (is.complex(tmp$vectors)) {
    cat("  Warning! Complex eigenvectors. Using the real part.\n")
  }

  if (is.complex(tmp$values)) {
    cat("  Warning! Complex eigenvalues. Using the real part.\n")
  }

  return(list(
    QMatrix = Re(tmp$vectors),
    LMatrix = Matrix::Diagonal(Re(tmp$values), n = length(tmp$values)),
    Eigenvalues = cbind(sort(tmp$values))
  ))
}


#' Gets the largest Eigenvalue from a Matrix. A warning is raised if complex numbers emerge.
#'
#'
#' @param Matrix matrix
#' @return The eigenvector of largest eigenvalue, the largest eigenvalue
#' @export
GetLargestEigenv <- function(Matrix) {
  #we must distinguish between symmetric and non-symmetric matrices to have correct results
  # still to do for further improvements
  # tmp <- RSpectra::eigs(Matrix, 1, which = "LM")
  tmp <- eigen(Matrix, only.values = F)
  lm <- which.max(Re(tmp$values[abs(Im(tmp$values)) < 1e-6]))
  tmp$vectors <- tmp$vectors[, lm]
  tmp$values <- tmp$values[lm]
  # The result of some computation might return complex numbers with 0 imaginary part
  # If this is the case, we fix it, otherwise a warning is risen
  if (all(Im(tmp$vectors) == 0)) {
    tmp$vectors <- Re(tmp$vectors)
  } else {
    cat("Warning! Complex numbers in the leading eigenvector.\n")
  }

  if (all(Im(tmp$values) == 0)) {
    tmp$values <- Re(tmp$values)
  } else {
    cat("Warning! Complex numbers in the leading eigenvalue.\n")
  }

  #check if the eigenvector has all negative components.. in that case we change the sign
  #first, set to zero everything that is so small that can create problems even if it compatible with zero
  tmp$vectors[Matrix::which(tmp$vectors > -1e-12 & tmp$vectors < 1e-12)] <- 0
  #now verify that all components are negative and change sign
  if (all(tmp$vectors[Matrix::which(tmp$vectors != 0)] < 0)) {
    tmp$vectors <- -tmp$vectors
  }

  return(list(QMatrix = tmp$vectors, LMatrix = tmp$values))

  # remind to return a column vector result.. check always that returned result is compatible with original octave result
}

#' Binarize a matrix
#' 
#' Get a binarized version (only 0s and 1s) of a Matrix, which is assumed to be 
#' sparse. Matrix entries which are different from zero are set to one.
#'
#' @param A matrix
#' @return Matrix binarized
#' @export
binarizeMatrix <- function(A) {
  #A is assumed to be sparse
  A[Matrix::which(A != 0)] <- 1
  return(A)
}

#binarizeMatrix <- function(A){
#    return( Matrix::Matrix(as.numeric(A>0), dim(A)[1], dim(A)[2], sparse=T) )
#}


#' Return the ith canonical vector of a N-dimension basis
#'
#'
#' @param N sacalar, dimension of the basis
#' @param i sacalar, the ith vector
#' @return
#' Canonical vector

CanonicalVector <- function(N, i) {
  vec <- zeros(1, N)
  vec[i] <- 1
  return(vec)
}

###################################################################
## REDUCIBILITY OF MULTILAYER NETWORKS
###################################################################


#Warning: this should be made compatible with code in SpectralEntropyLib and SpectralGeometryLib

#' Network combinatorial Laplacian
#'
#' Given an adjacency matrix \eqn{A}, the function builds the
#'    combinatorial Laplacian \eqn{D - A} for a single-layer network.
#'
#' @param AdjacencyMatrix the adjacency matrix characterising the network
#' @return The network combinatorial Laplacian Matrix
#' @export
GetLaplacianMatrix <- function(AdjacencyMatrix) {
  #Calculate the laplacian matrix from an adjacency matrix

  N <- dim(AdjacencyMatrix)[1]
  u <- ones(N, 1)

  #laplacian
  LaplacianMatrix <-
    diagR(AdjacencyMatrix %*% u, N, 0) - AdjacencyMatrix

  #always check
  if (sum(LaplacianMatrix %*% u) > 1.e-8) {
    stop("ERROR! The Laplacian matrix has rows that don't sum to 0. Aborting process.\n")
  }

  return(Matrix::drop0(LaplacianMatrix))
}

#' Network normalized Laplacian
#'
#' Given an adjacency matrix \eqn{A}, the function builds the
#'    random walk (RW) normalised Laplacian \eqn{I - D^{-1}A} for a single-layer
#'    network.
#'    The RW normalised Laplacian is defined only for graphs without isolated
#'    nodes -- due to the inversion of the diagonal matrix of degrees
#'    \eqn{D^{-1}}.
#'    Despite this, a (classical) random walk is still defined also in presence
#'    of isolates or nodes without out-going edges, simply setting the
#'    transition probability from those nodes outwards to be zero.
#'    Consequently we can extend the definition of the RW normalised Laplacian
#'    setting \eqn{L_{ij} = 0} if \eqn{k_i = 0} for all \eqn{j}.
#' @param AdjacencyMatrix the adjacency matrix characterising the network
#' @return Normalized Laplacian Matrix
#' @export
GetNormalizedLaplacianMatrix <- function(AdjacencyMatrix) {
  #Calculate the laplacian matrix from an adjacency matrix

  N <- dim(AdjacencyMatrix)[1]
  u <- ones(N, 1)

  degs <- AdjacencyMatrix %*% u
  #StrengthMatrix <- diagR(degs, N)
  DisconnectedNodes <- sum(degs == 0)

  if (DisconnectedNodes > 0) {
    cat(paste0(
      " #Trapping nodes (no outgoing-links): ",
      DisconnectedNodes,
      "\n"
    ))
  }

  # laplacian
  # LaplacianMatrix <- GetLaplacianMatrix(AdjacencyMatrix)

  # WARNING
  # FOR NETWORKS WITH ISOLATED NODES, THE CALCULATION OF THE LAPLACIAN
  # IS CORRECT ONLY IF THE LAPLACIAN IS COMBINATORIAL.
  # FOR NORMALIZED LAPLACIAN, CARE MUST BE TAKEN (DIAGONAL ENTRIES SHOULD
  # BE SET TO 0). RIGHT NOW THE DIAG ENTRY WOULD BE 1, THAT'S WHY THE CHECK
  # CONSIDER DisconnectedNodes AGAINST THE SUM. BY SETTING DIAG ENTRIES
  # TO 0, THAT CHECK CAN AVOID TO SUBTRACT DisconnectedNodes
  # igraph with normalized=T gives the symmetric Laplacian, therefore this must be done
  # internally and ad hoc here.
  # THROWING A STOP

  invD <- 1 / degs
  invD[Matrix::which(is.infinite(invD))] <- 0
  invD <- diagR(invD, N, 0)
  LaplacianMatrix <- speye(N) - invD %*% AdjacencyMatrix

  if (DisconnectedNodes > 0) {
    cat(
      " WARNING. Trying to use a Normalized Laplacian from a network with disconnected nodes. \n WARNING. Look inside the code for further details, but this could lead to wrong result. \n WARNING. Solution is implemented but check the results.\n"
    )
    diag(LaplacianMatrix)[Matrix::which(degs == 0)] <- 0

    #w e have just corrected for disconnected nodes
    DisconnectedNodes <- 0
  }

  #always check
  if (sumR(sumR(LaplacianMatrix, 2), 1) - DisconnectedNodes > 1.e-8) {
    stop(
      "ERROR! The normalized Laplacian matrix has rows that don't sum to 0. Aborting process.\n"
    )
  }

  return(Matrix::drop0(LaplacianMatrix))
}


# Warning: this should be made compatible with code in SpectralEntropyLib
# Functions below should allow to choose for the type of density matrix (BGS/DDB) and laplacian

#' Calculate the density matrix from an adjacency matrix
#'
#'
#' @param AdjacencyMatrix an adjacency matrix
#' @return Density matrix for the provided matrix
#' @references
#'   S. L. Braunstein, S. Ghosh, S. Severini, Annals of Combinatorics 10, No 3, (2006)
#'
#'   De Domenico, M., Set al. (2013). Mathematical formulation of
#'     multilayer networks. Physical Review X, 3(4), 041022.
#'     \href{https://doi.org/10.1103/PhysRevX.3.041022}{doi 10.1103/PhysRevX.3.041022}
#' @export
BuildDensityMatrixBGS <- function(AdjacencyMatrix) {
  DensityMatrix <- GetLaplacianMatrix(AdjacencyMatrix)

  #normalize to degree sum
  return(DensityMatrix / (traceR(DensityMatrix)))
}


#' Calculate the eigenvalues of a density matrix
#'
#'
#' @param DensityMatrix an adjacency matrix
#' @return
#' The Eigenvalues for the provided Density matrix
#' @references
#' De Domenico, M., Set al. (2013). Mathematical formulation of
#'     multilayer networks. Physical Review X, 3(4), 041022.
#'     \href{https://doi.org/10.1103/PhysRevX.3.041022}{doi 10.1103/PhysRevX.3.041022}
#' @export
GetEigenvaluesOfDensityMatrix <- function(DensityMatrix) {
  Eigenvalues <- cbind(eigen(DensityMatrix)$values)

  #check that eigenvalues sum to 1
  if (abs(sum(Eigenvalues) - 1) > 1e-8) {
    stop("ERROR! Eigenvalues dont sum to 1! Aborting process.")
  }

  return(Eigenvalues)
}


#' Calculate the eigenvalues of a density matrix from an adjacency matrix
#'
#'
#' @param AdjacencyMatrix an adjacency matrix
#' @return
#' The Eigenvalues for the Density matrix of the provided adjacency matrix
#' @export
GetEigenvaluesOfDensityMatrixFromAdjacencyMatrix <-
  function(AdjacencyMatrix) {
    DensityMatrix <- BuildDensityMatrixBGS(AdjacencyMatrix)
    return(GetEigenvaluesOfDensityMatrix(DensityMatrix))
  }


#' Calculate the quantum Renyi entropy of a network
#'
#'
#' @param AdjacencyMatrix an adjacency matrix
#' @param Q scalar, computes for the relative entropy. Computing Von Neuman quantum entropy if Q=1 (default case), and Renyi quantum entropy otherwise
#' @return
#' Renyi Entropy matrix for the provided adjacency matrix
#' @references
#' De Domenico, M., Set al. (2013). Mathematical formulation of
#'     multilayer networks. Physical Review X, 3(4), 041022.
#'     \href{doi 10.1103/PhysRevX.3.041022}{https://doi.org/10.1103/PhysRevX.3.041022}
#'
#' De Domenico, M., et al. Structural reducibility of multilayer networks. Nat Commun 6, 6864 (2015).
#'   \href{https://doi.org/10.1038/ncomms7864}{doi 10.1038/ncomms7864}
#' @export
GetRenyiEntropyFromAdjacencyMatrix <-
  function(AdjacencyMatrix, Q = 1) {
    Eigenvalues <-
      GetEigenvaluesOfDensityMatrixFromAdjacencyMatrix(AdjacencyMatrix)

    if (Q == 1.) {
      #Von Neuman quantum entropy
      RenyiEntropy <-
        -sum(Eigenvalues[Eigenvalues > 0] * log(Eigenvalues[Eigenvalues > 0]))
    } else {
      #Renyi quantum entropy
      RenyiEntropy <- (1 - sum(Eigenvalues[Eigenvalues > 0] ^ Q)) / (Q - 1)
    }

    return(RenyiEntropy)
  }



#' Calculate the Jensen-Shannon Divergence of two networks
#'
#'
#' @param AdjacencyMatrix1 an adjacency matrix
#' @param AdjacencyMatrix2 an adjacency matrix
#' @param VNEntropy1 the value of the Von Neumann entropy of the corresponding (1st) adjacency matrix
#' @param VNEntropy2 the value of the Von Neumann entropy of the corresponding (2nd) adjacency matrix
#' @return
#' The Jensen–Shannon distance between the two networks
#' @references
#' De Domenico, M., et al. Structural reducibility of multilayer networks. Nat Commun 6, 6864 (2015).
#'   \href{https://doi.org/10.1038/ncomms7864}{doi 10.1038/ncomms7864}
#' @export
GetJensenShannonDivergence <-
  function(AdjacencyMatrix1,
           AdjacencyMatrix2,
           VNEntropy1,
           VNEntropy2) {
    #    %M = 0.5 * (RHO + SIGMA)
    #    %JSD: 0.5 * DKL( RHO || M ) + 0.5 * DKL( SIGMA || M )
    #    %DKL( A || B ) = tr[ A log A - A log B ] = -entropy(A) - tr[ A log B ]
    #    %
    #    %JSD: 0.5 * ( -entropy(RHO) - entropy(SIGMA) - tr[ RHO log M ] - tr[ SIGMA log M ] )
    #    %         -0.5 * [ entropy(RHO) + entropy(SIGMA) ] - tr[ M log M ] )
    #    %         -0.5 * [ entropy(RHO) + entropy(SIGMA) ] + entropy(M)

    DensityMatrix1 <- BuildDensityMatrixBGS(AdjacencyMatrix1)
    DensityMatrix2 <- BuildDensityMatrixBGS(AdjacencyMatrix2)
    DensityMatrixM <- (DensityMatrix1 + DensityMatrix2) / 2.

    EigenvaluesM <- eigen(DensityMatrixM)$values
    CrossEntropyM = -sum(EigenvaluesM[EigenvaluesM > 0] * log(EigenvaluesM[EigenvaluesM >
                                                                             0]))

    JSD <- CrossEntropyM - 0.5 * (VNEntropy1 + VNEntropy2)

    return(JSD)
  }

###################################################################
## TOPOLOGICAL DESCRIPTORS OF MULTILAYER NETWORKS
###################################################################


#' Calculate the global number of triangles
#'
#'
#' @param SupraAdjacencyMatrix the supra-adjacency matrix
#' @param Layers scalar, number of layers
#' @param Nodes scalar, number of nodes
#' @return
#' number of triangles
#' @references
#' De Domenico, M., Set al. (2013). Mathematical formulation of
#'     multilayer networks. Physical Review X, 3(4), 041022.
#'     \href{https://doi.org/10.1103/PhysRevX.3.041022}{doi 10.1103/PhysRevX.3.041022}
#' @export
GetGlobalNumberTriangles <-
  function(SupraAdjacencyMatrix, Layers, Nodes) {

    if (sum(abs(SupraAdjacencyMatrix - Matrix::t(SupraAdjacencyMatrix))) != 0) {
      num <-
        traceR((SupraAdjacencyMatrix %*% SupraAdjacencyMatrix) %*% SupraAdjacencyMatrix)
    } else {
      #undirected edges requires to account for multiple ways to produce the same triangle
      num <-
        traceR((SupraAdjacencyMatrix %*% SupraAdjacencyMatrix) %*% SupraAdjacencyMatrix) /
        6
    }
    return(num)
  }


#' Calculate Global Clustering Coefficient
#'
#'
#' @param SupraAdjacencyMatrix the supra-adjacency matrix
#' @param Layers scalar, number of layers
#' @param Nodes scalar, number of nodes
#' @return
#' Global clustering coefficient
#' @references
#' De Domenico, M., Set al. (2013). Mathematical formulation of
#'     multilayer networks. Physical Review X, 3(4), 041022.
#'     \href{https://doi.org/10.1103/PhysRevX.3.041022}{doi 10.1103/PhysRevX.3.041022}
#' @export
GetAverageGlobalClustering <-
  function(SupraAdjacencyMatrix, Layers, Nodes) {
    FMatrix <- ones(Nodes * Layers, Nodes * Layers) - speye(Nodes * Layers)
    num <-
      traceR((SupraAdjacencyMatrix %*% SupraAdjacencyMatrix) %*% SupraAdjacencyMatrix)
    den <-
      traceR((SupraAdjacencyMatrix %*% FMatrix) %*% SupraAdjacencyMatrix)

    return(num / (max(SupraAdjacencyMatrix) * den))
  }



#Note: can be optimized

#' Calculate Local Clustering Coefficient
#'
#'
#' @param SupraAdjacencyMatrix the supra-adjacency matrix
#' @param Layers scalar, number of layers
#' @param Nodes scalar, number of nodes
#' @return
#' Local clustering coefficient
#' @references
#' De Domenico, M., Set al. (2013). Mathematical formulation of
#'     multilayer networks. Physical Review X, 3(4), 041022.
#'     \href{https://doi.org/10.1103/PhysRevX.3.041022}{doi 10.1103/PhysRevX.3.041022}
#' @export
GetLocalClustering <- function(SupraAdjacencyMatrix, Layers, Nodes) {
  FMatrix <- ones(Nodes * Layers, Nodes * Layers) - speye(Nodes * Layers)
  M3 <-
    (SupraAdjacencyMatrix %*% SupraAdjacencyMatrix) %*% SupraAdjacencyMatrix
  F3 <- (SupraAdjacencyMatrix %*% FMatrix) %*% SupraAdjacencyMatrix


  blocks.num <- SupraAdjacencyToBlockTensor(M3, Layers, Nodes)
  blocks.den <- SupraAdjacencyToBlockTensor(F3, Layers, Nodes)

  B.num <- zeros(Nodes, Nodes)
  B.den <- zeros(Nodes, Nodes)
  for (i in 1:Layers) {
    for (j in 1:Layers) {
      B.num <- B.num + blocks.num[[i, j]]
      B.den <- B.den + blocks.den[[i, j]]
    }
  }

  clus <- cbind(diag(B.num) / diag(B.den))

  if (any(clus > 1 | clus < 0)) {
    stop(
      "GetLocalClustering:ERROR! Impossible clustering coefficients. Aborting process."
    )
  }

  return(clus)
}


#Note: can be optimized

#' Gets the average global edge overlap on the multilayer
#'
#'
#' @param SupraAdjacencyMatrix the supra-adjacency matrix
#' @param Layers scalar, number of layers
#' @param Nodes scalar, number of nodes
#' @param fastBinary logical, optimization for undirected, unweighted, networks
#' @return
#' Average Global edge overlap
#' @references
#' De Domenico, M., et al. Structural reducibility of multilayer networks. Nat Commun 6, 6864 (2015).
#'   \href{https://doi.org/10.1038/ncomms7864}{doi 10.1038/ncomms7864}
#' @export
GetAverageGlobalOverlapping <-
  function(SupraAdjacencyMatrix,
           Layers,
           Nodes,
           fastBinary = F) {
    #fastBinary: optimization for undirected, unweighted, networks

    if (Layers == 1) {
      stop(
        "GetAverageGlobalOverlapping:ERROR! At least two layers required. Aborting process."
      )
    }

    NodesTensor <-
      SupraAdjacencyToNodesTensor(SupraAdjacencyMatrix, Layers, Nodes)

    O <- pmin(NodesTensor[[1]], NodesTensor[[2]])
    NormTotal <- sum(sum(NodesTensor[[1]]))

    if (Layers > 2) {
      #assuming that LayerTensor is an undirected clique
      for (l in 2:Layers) {
        if (fastBinary) {
          O <- O * NodesTensor[[l]]
        } else {
          O <- pmin(O, NodesTensor[[l]])
        }
        NormTotal <- NormTotal + sum(sum(NodesTensor[[l]]))
      }
    }

    if (fastBinary) {
      O <- O > 0
    }

    AvGlobOverl <- Layers * sum(sum(O)) / NormTotal

    if (sum(SupraAdjacencyMatrix - Matrix::t(SupraAdjacencyMatrix)) == 0) {
      AvGlobOverl <- AvGlobOverl / 2
    }

    return(AvGlobOverl)
  }


#Note: can be optimized

#' Calculates the average global node overlap Matrix
#'
#'
#' @param SupraAdjacencyMatrix the supra-adjacency matrix
#' @param Layers scalar, number of layers
#' @param Nodes scalar, number of nodes
#' @return
#' Average Global Node Overlapping Matrix
#' @references
#' De Domenico, M., et al. Structural reducibility of multilayer networks. Nat Commun 6, 6864 (2015).
#'   \href{https://doi.org/10.1038/ncomms7864}{doi 10.1038/ncomms7864}
#' @export
GetAverageGlobalNodeOverlappingMatrix <-
  function(SupraAdjacencyMatrix, Layers, Nodes) {
    if (Layers == 1) {
      stop(
        "GetAverageGlobalNodeOverlappingMatrix:ERROR! At least two layers required. Aborting process.\n"
      )
    }

    NodesTensor <-
      SupraAdjacencyToNodesTensor(SupraAdjacencyMatrix, Layers, Nodes)

    existingNodes <- vector("list", Layers)

    for (l in 1:Layers) {
      #find cols and rows where sum > zero to identify connected nodes
      cols <- Matrix::which(sumR(NodesTensor[[l]], 2) != 0)
      rows <- Matrix::which(sumR(NodesTensor[[l]], 1) != 0)

      #merge the two (this approach is necessary to deal also with directed networks)
      existingNodes[[l]] <- union(cols, rows)
    }

    AvGlobOverlMatrix <- Matrix::Matrix(0, Layers, Layers, sparse = T)
    diag(AvGlobOverlMatrix) <- 1
    for (l1 in 1:(Layers - 1)) {
      for (l2 in (l1 + 1):Layers) {
        AvGlobOverlMatrix[l1, l2] <-
          length(intersect(existingNodes[[l1]], existingNodes[[l2]])) / Nodes

        AvGlobOverlMatrix[l2, l1] <- AvGlobOverlMatrix[l1, l2]
      }
    }

    return(AvGlobOverlMatrix)
  }


#Note: can be optimized

#' Calculates the average global edge overlap Matrix
#'
#'
#' @param SupraAdjacencyMatrix the supra-adjacency matrix
#' @param Layers scalar, number of layers
#' @param Nodes scalar, number of nodes
#' @param fastBinary logical, optimization for undirected, unweighted, networks
#' @return
#' Average Global Edge Overlapping Matrix
#' @references
#' De Domenico, M., et al. Structural reducibility of multilayer networks. Nat Commun 6, 6864 (2015).
#'   \href{https://doi.org/10.1038/ncomms7864}{doi 10.1038/ncomms7864}
#' @export
GetAverageGlobalOverlappingMatrix <-
  function(SupraAdjacencyMatrix,
           Layers,
           Nodes,
           fastBinary = F) {
    #fastBinary: optimization for undirected, unweighted, networks

    if (Layers == 1) {
      stop(
        "GetAverageGlobalOverlappingMatrix:ERROR! At least two layers required. Aborting process.\n"
      )
    }

    NodesTensor <-
      SupraAdjacencyToNodesTensor(SupraAdjacencyMatrix, Layers, Nodes)

    AvGlobOverlMatrix <- Matrix::Matrix(0, Layers, Layers, sparse = T)
    diag(AvGlobOverlMatrix) <- 1
    for (l1 in 1:(Layers - 1)) {
      Norm1 <- sum(sum(NodesTensor[[l1]]))
      for (l2 in (l1 + 1):Layers) {
        if (fastBinary) {
          O <- NodesTensor[[l1]] * NodesTensor[[l2]]
          O <- O > 0
        } else {
          O <- pmin(NodesTensor[[l1]], NodesTensor[[l2]])
        }
        AvGlobOverlMatrix[l1, l2] <-
          2 * sum(sum(O)) / (Norm1 + sum(sum(NodesTensor[[l2]])))
        AvGlobOverlMatrix[l2, l1] <- AvGlobOverlMatrix[l1, l2]
      }
    }

    return(AvGlobOverlMatrix)
  }


#' Returns pairwise similarity based on Frobenius norm
#'
#'
#' @param SupraAdjacencyMatrix the supra-adjacency matrix
#' @param Layers scalar, number of layers
#' @param Nodes scalar, number of nodes
#' @return
#' Pairwise similarity based on Frobenius norm
#' @export
GetSPSimilarityMatrix <-
  function(SupraAdjacencyMatrix, Layers, Nodes) {
    distanceList <- vector("list", Layers)

    g.list <-
      SupraAdjacencyToNetworkList(SupraAdjacencyMatrix, Layers, Nodes)

    for (l in 1:Layers) {
      distanceList[[l]] <- igraph::shortest.paths(g.list[[l]])
      distanceList[[l]][is.infinite(distanceList[[l]])] <- 1e8
    }

    frobeniusNorm <-
      Matrix::Matrix(0,
                     ncol = Layers,
                     nrow = Layers,
                     sparse = T)
    for (l1 in 1:(Layers - 1)) {
      for (l2 in (l1 + 1):Layers) {
        frobeniusNorm[l1, l2] <-
          sqrt(sum((distanceList[[l1]] - distanceList[[l2]]) ^ 2))
        frobeniusNorm[l2, l1] <- frobeniusNorm[l1, l2]
      }
    }
    frobeniusNorm <- 1 - frobeniusNorm / max(frobeniusNorm)

    return(frobeniusNorm)
  }



#' Return the multi-out-degree, not accounting for interlinks
#'
#'
#' @param SupraAdjacencyMatrix the supra-adjacency matrix
#' @param Layers scalar, number of layers
#' @param Nodes scalar, number of nodes
#' @param isDirected logical
#' @return
#' Vector of Multi-out-degree
#' @references
#' De Domenico, M., Set al. (2013). Mathematical formulation of
#'     multilayer networks. Physical Review X, 3(4), 041022.
#'     \href{https://doi.org/10.1103/PhysRevX.3.041022}{doi 10.1103/PhysRevX.3.041022}
#' @export
GetMultiOutDegree <-
  function(SupraAdjacencyMatrix,
           Layers,
           Nodes,
           isDirected) {
    NodesTensor <-
      SupraAdjacencyToNodesTensor(binarizeMatrix(SupraAdjacencyMatrix), Layers, Nodes)
    AggrMatrix <- GetAggregateMatrix(NodesTensor, Layers, Nodes)
    MultiOutDegreeVector <- sumR(AggrMatrix, 2)

    return(MultiOutDegreeVector)
  }



#' Return the multi-in-degree, not accounting for interlinks
#'
#'
#' @param SupraAdjacencyMatrix the supra-adjacency matrix
#' @param Layers scalar, number of layers
#' @param Nodes scalar, number of nodes
#' @param isDirected logical
#' @return
#' Vector of Multi-in-degree in column format
#' @references
#' De Domenico, M., Set al. (2013). Mathematical formulation of
#'     multilayer networks. Physical Review X, 3(4), 041022.
#'     \href{https://doi.org/10.1103/PhysRevX.3.041022}{doi 10.1103/PhysRevX.3.041022}
#' @export
GetMultiInDegree <-
  function(SupraAdjacencyMatrix,
           Layers,
           Nodes,
           isDirected) {
    NodesTensor <-
      SupraAdjacencyToNodesTensor(binarizeMatrix(SupraAdjacencyMatrix), Layers, Nodes)
    AggrMatrix <- GetAggregateMatrix(NodesTensor, Layers, Nodes)
    MultiInDegreeVector <- sumR(AggrMatrix, 1)

    #return in column format
    return(Matrix::t(MultiInDegreeVector))
  }



#' Return total multi-degree, not accounting for interlinks
#'
#'
#' @param SupraAdjacencyMatrix the supra-adjacency matrix
#' @param Layers scalar, number of layers
#' @param Nodes scalar, number of nodes
#' @param isDirected logical
#' @return
#' Vector of total multi-degree
#' @references
#' De Domenico, M., Set al. (2013). Mathematical formulation of
#'     multilayer networks. Physical Review X, 3(4), 041022.
#'     \href{https://doi.org/10.1103/PhysRevX.3.041022}{doi 10.1103/PhysRevX.3.041022}
#' @export
GetMultiDegree <-
  function(SupraAdjacencyMatrix,
           Layers,
           Nodes,
           isDirected) {
    MultiInDegreeVector <-
      GetMultiInDegree(SupraAdjacencyMatrix, Layers, Nodes, isDirected)
    MultiOutDegreeVector <-
      GetMultiOutDegree(SupraAdjacencyMatrix, Layers, Nodes, isDirected)

    if (!isDirected) {
      MultiDegreeVector <- (MultiInDegreeVector + MultiOutDegreeVector) / 2
    } else {
      MultiDegreeVector <- MultiInDegreeVector + MultiOutDegreeVector
    }

    return(MultiDegreeVector)
  }


#Note: can be optimized

#' Return Assortativity Inter-layer values
#'
#'
#' @param SupraAdjacencyMatrix the supra-adjacency matrix
#' @param Layers scalar, number of layers
#' @param Nodes scalar, number of nodes
#' @param isDirected logical
#' @param Type string, "OO", "II", "IO", "OI", and "TT". Indicates total, in or out multi-degree
#' @return
#' Assortativity values by Pearson and Spearman methods
#' @export
GetInterAssortativityTensor <-
  function(SupraAdjacencyMatrix,
           Layers,
           Nodes,
           isDirected,
           Type) {
    if (Layers == 1) {
      stop(
        "GetInterAssortativityTensor: ERROR! At least two layers required. Aborting process.\n"
      )
    }

    NodesTensor <-
      SupraAdjacencyToNodesTensor(SupraAdjacencyMatrix, Layers, Nodes)

    InterPearson <- speye(Layers)
    InterSpearman <- speye(Layers)

    if (Type == "IO" || Type == "OI") {
      InDegree <- vector("list", Layers)
      OutDegree <- vector("list", Layers)

      for (l in 1:Layers) {
        InDegree[[l]] <-
          GetMultiInDegree(NodesTensor[[l]], 1, Nodes, isDirected)
        OutDegree[[l]] <-
          GetMultiOutDegree(NodesTensor[[l]], 1, Nodes, isDirected)
      }

      for (l1 in 1:Layers) {
        for (l2 in 1:Layers) {
          InterPearson[l1, l2] <-
            stats::cor(x = InDegree[[l1]], OutDegree[[l2]], method = "pearson")
          InterSpearman[l1, l2] <-
            stats::cor(x = InDegree[[l1]], OutDegree[[l2]], method = "spearman")
        }
      }

      if (Type == "OI") {
        InterPearson <- Matrix::t(InterPearson)
        InterSpearman <- Matrix::t(InterSpearman)
      }
    } else {
      Degree <- vector("list", Layers)

      if (Type == "OO") {
        for (l in 1:Layers) {
          Degree[[l]] <-
            GetMultiOutDegree(NodesTensor[[l]], 1, Nodes, isDirected)
        }
      }

      if (Type == "II") {
        for (l in 1:Layers) {
          Degree[[l]] <- GetMultiInDegree(NodesTensor[[l]], 1, Nodes, isDirected)
        }
      }

      if (Type == "TT") {
        for (l in 1:Layers) {
          Degree[[l]] <- GetMultiDegree(NodesTensor[[l]], 1, Nodes, isDirected)
        }
      }


      for (l1 in 1:(Layers - 1)) {
        for (l2 in (l1 + 1):Layers) {
          InterPearson[l1, l2] <-
            stats::cor(x = Degree[[l1]],
                       y = Degree[[l2]],
                       method = "pearson")
          InterSpearman[l1, l2] <-
            stats::cor(x = Degree[[l1]],
                       y = Degree[[l2]],
                       method = "spearman")

          InterPearson[l2, l1] <- InterPearson[l1, l2]
          InterSpearman[l2, l1] <- InterSpearman[l1, l2]
        }
      }
    }

    return(list(InterPearson = InterPearson, InterSpearman = InterSpearman))
  }



#' Return the Sum of multi-out-degree, include multiple times the interlinks
#'
#'
#' @param SupraAdjacencyMatrix the supra-adjacency matrix
#' @param Layers scalar, number of layers
#' @param Nodes scalar, number of nodes
#' @param isDirected logical
#' @return
#' Vector of sum of multi-out-degree
#' @references
#' De Domenico, M., Set al. (2013). Mathematical formulation of
#'     multilayer networks. Physical Review X, 3(4), 041022.
#'     \href{https://doi.org/10.1103/PhysRevX.3.041022}{doi 10.1103/PhysRevX.3.041022}
#' @export
GetMultiOutDegreeSum <-
  function(SupraAdjacencyMatrix,
           Layers,
           Nodes,
           isDirected) {
    SupraDegree <- sumR(binarizeMatrix(SupraAdjacencyMatrix), 2)
    MultiOutDegreeVector <- sumR(reshapeR(SupraDegree, Nodes, Layers), 2)

    return(MultiOutDegreeVector)
  }


#' Return the Sum of multi-in-degree, include multiple times the interlinks
#'
#'
#' @param SupraAdjacencyMatrix the supra-adjacency matrix
#' @param Layers scalar, number of layers
#' @param Nodes scalar, number of nodes
#' @param isDirected logical
#' @return
#' Vector of sum of multi-in-degree
#' @references
#' De Domenico, M., Set al. (2013). Mathematical formulation of
#'     multilayer networks. Physical Review X, 3(4), 041022.
#'     \href{https://doi.org/10.1103/PhysRevX.3.041022}{doi 10.1103/PhysRevX.3.041022}
#' @export
GetMultiInDegreeSum <-
  function(SupraAdjacencyMatrix,
           Layers,
           Nodes,
           isDirected) {
    SupraDegree <- Matrix::t(sumR(binarizeMatrix(SupraAdjacencyMatrix), 1))
    MultiInDegreeVector <- sumR(reshapeR(SupraDegree, Nodes, Layers), 2)

    return(MultiInDegreeVector)
  }


#' Return the Sum of total multi-degree, include multiple times the interlinks
#'
#'
#' @param SupraAdjacencyMatrix the supra-adjacency matrix
#' @param Layers scalar, number of layers
#' @param Nodes scalar, number of nodes
#' @param isDirected logical
#' @return
#' Vector of sum of total multi-degree
#' @references
#' De Domenico, M., Set al. (2013). Mathematical formulation of
#'     multilayer networks. Physical Review X, 3(4), 041022.
#'     \href{https://doi.org/10.1103/PhysRevX.3.041022}{doi 10.1103/PhysRevX.3.041022}
#' @export
GetMultiDegreeSum <-
  function(SupraAdjacencyMatrix,
           Layers,
           Nodes,
           isDirected) {
    MultiInDegreeVector <-
      GetMultiInDegreeSum(SupraAdjacencyMatrix, Layers, Nodes, isDirected)
    MultiOutDegreeVector <-
      GetMultiOutDegreeSum(SupraAdjacencyMatrix, Layers, Nodes, isDirected)

    if (!isDirected) {
      MultiDegreeVector <- (MultiInDegreeVector + MultiOutDegreeVector) / 2
    } else {
      MultiDegreeVector <- MultiInDegreeVector + MultiOutDegreeVector
    }

    return(MultiDegreeVector)
  }


#' Return the multi-out-Strength, not accounting for interlinks
#'
#'
#' @param SupraAdjacencyMatrix the supra-adjacency matrix
#' @param Layers scalar, number of layers
#' @param Nodes scalar, number of nodes
#' @param isDirected logical
#' @return
#' Vector of multi-out-strength
#' @references
#' De Domenico, M., Set al. (2013). Mathematical formulation of
#'     multilayer networks. Physical Review X, 3(4), 041022.
#'     \href{https://doi.org/10.1103/PhysRevX.3.041022}{doi 10.1103/PhysRevX.3.041022}
#' @export
GetMultiOutStrength <-
  function(SupraAdjacencyMatrix,
           Layers,
           Nodes,
           isDirected) {
    NodesTensor <-
      SupraAdjacencyToNodesTensor(SupraAdjacencyMatrix, Layers, Nodes)
    AggrMatrix <- GetAggregateMatrix(NodesTensor, Layers, Nodes)
    MultiOutStrengthVector <- sumR(AggrMatrix, 2)

    return(MultiOutStrengthVector)
  }


#' Return the multi-in-Strength, not accounting for interlinks
#'
#'
#' @param SupraAdjacencyMatrix the supra-adjacency matrix
#' @param Layers scalar, number of layers
#' @param Nodes scalar, number of nodes
#' @param isDirected logical
#' @return
#' Vector of multi-in-strength in column format
#' @references
#' De Domenico, M., Set al. (2013). Mathematical formulation of
#'     multilayer networks. Physical Review X, 3(4), 041022.
#'     \href{https://doi.org/10.1103/PhysRevX.3.041022}{doi 10.1103/PhysRevX.3.041022}
#' @export
GetMultiInStrength <-
  function(SupraAdjacencyMatrix,
           Layers,
           Nodes,
           isDirected) {
    NodesTensor <-
      SupraAdjacencyToNodesTensor(SupraAdjacencyMatrix, Layers, Nodes)
    AggrMatrix <- GetAggregateMatrix(NodesTensor, Layers, Nodes)
    MultiInStrengthVector <- sumR(AggrMatrix, 1)

    #return in column format
    return(Matrix::t(MultiInStrengthVector))
  }


#' Return total multi-strength
#'
#'
#' @param SupraAdjacencyMatrix the supra-adjacency matrix
#' @param Layers scalar, number of layers
#' @param Nodes scalar, number of nodes
#' @param isDirected logical
#' @return
#' Vector of total multi-strength
#' @references
#' De Domenico, M., Set al. (2013). Mathematical formulation of
#'     multilayer networks. Physical Review X, 3(4), 041022.
#'     \href{https://doi.org/10.1103/PhysRevX.3.041022}{doi 10.1103/PhysRevX.3.041022}
#' @export
GetMultiStrength <-
  function(SupraAdjacencyMatrix,
           Layers,
           Nodes,
           isDirected) {
    MultiInStrengthVector <-
      GetMultiInStrength(SupraAdjacencyMatrix, Layers, Nodes, isDirected)
    MultiOutStrengthVector <-
      GetMultiOutStrength(SupraAdjacencyMatrix, Layers, Nodes, isDirected)

    if (!isDirected) {
      MultiStrengthVector <-
        (MultiInStrengthVector + MultiOutStrengthVector) / 2
    } else {
      MultiStrengthVector <-
        MultiInStrengthVector + MultiOutStrengthVector
    }

    return(MultiStrengthVector)
  }


#' Return the Sum of multi-out-Strength, include multiple times the interlinks
#'
#'
#' @param SupraAdjacencyMatrix the supra-adjacency matrix
#' @param Layers scalar, number of layers
#' @param Nodes scalar, number of nodes
#' @param isDirected logical
#' @return
#' Vector of sum of multi-out-Strength
#' @references
#' De Domenico, M., Set al. (2013). Mathematical formulation of
#'     multilayer networks. Physical Review X, 3(4), 041022.
#'     \href{https://doi.org/10.1103/PhysRevX.3.041022}{doi 10.1103/PhysRevX.3.041022}
#' @export
GetMultiOutStrengthSum <-
  function(SupraAdjacencyMatrix,
           Layers,
           Nodes,
           isDirected) {
    SupraStrength <- sumR(SupraAdjacencyMatrix, 2)
    MultiOutStrengthVector <-
      sumR(reshapeR(SupraStrength, Nodes, Layers), 2)

    return(MultiOutStrengthVector)
  }


#' Return the Sum of multi-in-Strength, include multiple times the interlinks
#'
#'
#' @param SupraAdjacencyMatrix the supra-adjacency matrix
#' @param Layers scalar, number of layers
#' @param Nodes scalar, number of nodes
#' @param isDirected logical
#' @return
#' Vector of sum of multi-in-Strength
#' @references
#' De Domenico, M., Set al. (2013). Mathematical formulation of
#'     multilayer networks. Physical Review X, 3(4), 041022.
#'     \href{https://doi.org/10.1103/PhysRevX.3.041022}{doi 10.1103/PhysRevX.3.041022}
#' @export
GetMultiInStrengthSum <-
  function(SupraAdjacencyMatrix,
           Layers,
           Nodes,
           isDirected) {
    SupraStrength <- Matrix::t(sumR(SupraAdjacencyMatrix, 1))
    MultiInStrengthVector <-
      sumR(reshapeR(SupraStrength, Nodes, Layers), 2)

    return(MultiInStrengthVector)
  }


#' Return the Sum of total multi-Strength, include multiple times the interlinks
#'
#'
#' @param SupraAdjacencyMatrix the supra-adjacency matrix
#' @param Layers scalar, number of layers
#' @param Nodes scalar, number of nodes
#' @param isDirected logical
#' @return
#' Vector of sum total multi-Strength
#' @references
#' De Domenico, M., Set al. (2013). Mathematical formulation of
#'     multilayer networks. Physical Review X, 3(4), 041022.
#'     \href{https://doi.org/10.1103/PhysRevX.3.041022}{doi 10.1103/PhysRevX.3.041022}
#' @export
GetMultiStrengthSum <-
  function(SupraAdjacencyMatrix,
           Layers,
           Nodes,
           isDirected) {
    MultiInStrengthVector <-
      GetMultiInStrengthSum(SupraAdjacencyMatrix, Layers, Nodes, isDirected)
    MultiOutStrengthVector <-
      GetMultiOutStrengthSum(SupraAdjacencyMatrix, Layers, Nodes, isDirected)

    if (!isDirected) {
      MultiStrengthVector <-
        (MultiInStrengthVector + MultiOutStrengthVector) / 2
    } else {
      MultiStrengthVector <-
        MultiInStrengthVector + MultiOutStrengthVector
    }

    return(MultiStrengthVector)
  }


#' Calculates Multi-Katz Centrality
#'
#'
#' @param SupraAdjacencyMatrix the supra-adjacency matrix
#' @param Layers scalar, number of layers
#' @param Nodes scalar, number of nodes
#' @return
#' Vector of Katz centralities
#' @references
#' De Domenico, M., Solé-Ribalta, A., Omodei, E. et al. Ranking in interconnected multilayer
#'   networks reveals versatile nodes. Nat Commun 6, 6868 (2015).
#'   \href{https://doi.org/10.1038/ncomms7868}{doi 10.1038/ncomms7868}
#' @export
GetMultiKatzCentrality <-
  function(SupraAdjacencyMatrix, Layers, Nodes) {
    # we pass the transpose of the transition matrix to get the left eigenvectors
    tmp <- GetLargestEigenv(Matrix::t(SupraAdjacencyMatrix))
    LeadingEigenvalue <- tmp$LMatrix

    #Katz kernel tensor
    deltaTensor <- kron(speye(Nodes), speye(Layers))

    #this ensures convergence of the Katz kernel tensor
    a <- 0.99999 / abs(LeadingEigenvalue)

    KatzKernelTensor <- inv(deltaTensor - a * SupraAdjacencyMatrix)

    KatzCentralitySupraVector <-
      KatzKernelTensor %*% ones(Nodes * Layers, 1)
    CentralityVector <-
      sumR(reshapeR(KatzCentralitySupraVector, Nodes, Layers), 2)
    CentralityVector <- CentralityVector / max(CentralityVector)

    return(CentralityVector)
  }


#' Calculates Multi-Eigenvector-Centrality
#'
#'
#' @param SupraAdjacencyMatrix the supra-adjacency matrix
#' @param Layers scalar, number of layers
#' @param Nodes scalar, number of nodes
#' @return
#' Vector of Eigenvector centralities
#' @references
#' De Domenico, M., Solé-Ribalta, A., Omodei, E. et al. Ranking in interconnected multilayer
#'   networks reveals versatile nodes. Nat Commun 6, 6868 (2015).
#'   \href{https://doi.org/10.1038/ncomms7868}{doi 10.1038/ncomms7868}
#' @export
GetMultiEigenvectorCentrality <-
  function(SupraAdjacencyMatrix, Layers, Nodes) {
    #we pass the transpose of the transition matrix to get the left eigenvectors
    tmp <- GetLargestEigenv(Matrix::t(SupraAdjacencyMatrix))
    LeadingEigenvector <- tmp$QMatrix

    CentralityVector <-
      sumR(reshapeR(LeadingEigenvector, Nodes, Layers), 2)
    CentralityVector <- CentralityVector / max(CentralityVector)

    return(CentralityVector)
  }


#' Calculates Multi-Hub-Centrality
#'
#'
#' @param SupraAdjacencyMatrix the supra-adjacency matrix
#' @param Layers scalar, number of layers
#' @param Nodes scalar, number of nodes
#' @return
#' Vector of Hub centralities
#' @references
#' De Domenico, M., Solé-Ribalta, A., Omodei, E. et al. Ranking in interconnected multilayer
#'   networks reveals versatile nodes. Nat Commun 6, 6868 (2015).
#'   \href{https://doi.org/10.1038/ncomms7868}{doi 10.1038/ncomms7868}
#' @export
GetMultiHubCentrality <-
  function(SupraAdjacencyMatrix, Layers, Nodes) {
    #build the A A'
    SupraMatrix <- SupraAdjacencyMatrix %*% Matrix::t(SupraAdjacencyMatrix)

    #we pass the matrix to get the right eigenvectors
    #to deal with the possible degeneracy of the leading eigenvalue, we add an eps to the matrix
    #this ensures that we can apply the Perron-Frobenius theorem to say that there is a unique
    #leading eigenvector. Here we add eps, a very very small number (<1e-8, generally)
    tmp <- GetLargestEigenv(SupraMatrix + 1e-16)
    LeadingEigenvector <- tmp$QMatrix

    CentralityVector <-
      sumR(reshapeR(LeadingEigenvector, Nodes, Layers), 2)
    CentralityVector <- CentralityVector / max(CentralityVector)

    return(CentralityVector)
  }


#' Calculates Multi-Authority-Centrality
#'
#'
#' @param SupraAdjacencyMatrix the supra-adjacency matrix
#' @param Layers scalar, number of layers
#' @param Nodes scalar, number of nodes
#' @return
#' Vector of Authority centralities
#' @references
#' De Domenico, M., Solé-Ribalta, A., Omodei, E. et al. Ranking in interconnected multilayer
#'   networks reveals versatile nodes. Nat Commun 6, 6868 (2015).
#'   \href{https://doi.org/10.1038/ncomms7868}{doi 10.1038/ncomms7868}
#' @export
GetMultiAuthCentrality <-
  function(SupraAdjacencyMatrix, Layers, Nodes) {
    #build the A' A
    SupraMatrix <- Matrix::t(SupraAdjacencyMatrix) %*% SupraAdjacencyMatrix

    #we pass the matrix to get the right eigenvectors
    #to deal with the possible degeneracy of the leading eigenvalue, we add an eps to the matrix
    #this ensures that we can apply the Perron-Frobenius theorem to say that there is a unique
    #leading eigenvector. Here we add eps, a very very small number (<1e-8, generally)
    tmp <- GetLargestEigenv(SupraMatrix + 1e-16)
    LeadingEigenvector <- tmp$QMatrix

    CentralityVector <-
      sumR(reshapeR(LeadingEigenvector, Nodes, Layers), 2)
    CentralityVector <- CentralityVector / max(CentralityVector)

    return(CentralityVector)
  }



#' Calculates Multi-KCore Centrality. Computes centrality in each layer separately and then get the max per node.
#'
#'
#' @param SupraAdjacencyMatrix the supra-adjacency matrix
#' @param Layers scalar, number of layers
#' @param Nodes scalar, number of nodes
#' @return
#' Vector of K-core centralities
#' @export
GetMultiKCoreCentrality <-
  function(SupraAdjacencyMatrix, Layers, Nodes) {
    #calculate centrality in each layer separately and then get the max per node
    kcore.table <- matrix(0, nrow = Nodes, ncol = Layers)
    NodesTensor <-
      SupraAdjacencyToNodesTensor(SupraAdjacencyMatrix, Layers, Nodes)

    for (l in 1:Layers) {
      g.tmp <- igraph::graph.adjacency(NodesTensor[[l]], weighted = T)
      kcore.table[, l] <- igraph::graph.coreness(g.tmp, mode = "all")
    }

    CentralityVector <- apply(kcore.table, 1, min)
    return(CentralityVector)
  }



#' Calculates Multiplexity Centrality.
#'
#'
#' @param SupraAdjacencyMatrix the supra-adjacency matrix
#' @param Layers scalar, number of layers
#' @param Nodes scalar, number of nodes
#' @return
#' Vector of Multiplexity centralities
#' @export
GetMultiplexityCentrality <-
  function(SupraAdjacencyMatrix, Layers, Nodes) {
    NodesTensor <-
      SupraAdjacencyToNodesTensor(SupraAdjacencyMatrix, Layers, Nodes)

    existingNodes <- vector("list", Layers)
    nodeMultiplexity <- rep(0, Nodes)

    for (l in 1:Layers) {
      #find cols and rows where sum > zero to identify connected nodes
      cols <- Matrix::which(sumR(NodesTensor[[l]], 2) != 0)
      rows <- Matrix::which(sumR(NodesTensor[[l]], 1) != 0)

      #merge the two (this approach is necessary to deal also with directed networks)
      existingNodes[[l]] <- union(cols, rows)
      nodeMultiplexity[existingNodes[[l]]] <-
        nodeMultiplexity[existingNodes[[l]]] + 1
    }

    CentralityVector <- cbind(c(nodeMultiplexity)) / Layers
    return(CentralityVector)
  }


###################################################################
## RANDOM WALKS IN MULTILAYER NETWORKS
###################################################################

#' Build the supra-transition matrix for a multilayer network
#'
#' @description Given a supra-adjacency matrix, build supra-transition the
#'   matrix corresponding to different type of random walks (discrete time
#'   Markov chains).
#'   The types of RW considered here are the same as in Ref.2-Supplementary Material: classical, diffusive, PageRank, physical, physical with relaxation
#'   and maximum-entropy.
#'
#'   The maximum entropy random walk (MERW) choses the stochastic matrix which
#'   maximises \eqn{H(S)}, so that the walker can explore every walk of the
#'   same length with equal probability.
#'   Let \eqn{\lambda_N, \phi} be the leading eigenvalue and
#'   corresponding right eigenvector of the adjacency matrix \eqn{A}. Then
#'   \eqn{T_{ij} = \frac{A_{ij}}{\lambda_N}\frac{\phi_j}{\phi_i}.}
#'
#' @param SupraAdjacencyMatrix the supra-adjacency matrix, which contains
#'    the information in intra-layer connectivity \eqn{\{W^{\alpha}_{ij}\}} and
#'    on inter-layer coupling \eqn{D^{\alpha \beta}_{i}\}_i}.
#' @param Layers integer, number of layers
#' @param Nodes integer, number of nodes
#' @param Type the type of the random walk, default \code{"pagerank"}.
#'    Other types:
#'    \code{c("classical", "diffusive", "maxent", "physical", "relaxed-physical")}.
#'    For details on the transition rules see Refs.2-3.
#' @param r relaxation parameter, should be a float in the range \eqn{[0, 1]}.
#'    Default is NULL, which implies \eqn{r=0.5} for the physical random
#'    walk with relaxation (\code{Type = "relaxed-physical"}).
#' @return the supra-transition matrix of probabilities for a
#'    discrete-time random walk on the given multi-layer network.
#' @references
#'   1. De Domenico, M., Set al. (2013). Mathematical formulation of
#'     multilayer networks. Physical Review X, 3(4), 041022.
#'     \href{https://doi.org/10.1103/PhysRevX.3.041022}{doi 10.1103/PhysRevX.3.041022}
#'   2. De Domenico, M., et al. (2014). Navigability of interconnected networks
#'     under random failures. PNAS, 111(23).
#'     \href{https://doi.org/10.1073/pnas.1318469111}{doi 10.1073/pnas.1318469111}
#'   3. Bertagnolli, G., & De Domenico, M. (2020). Diffusion Geometry of
#'     Multiplex and Interdependent Systems.
#'     \href{http://arxiv.org/abs/2006.13032}{arxiv preprint arxiv:2006.13032}
#' @export
BuildSupraTransitionMatrixFromSupraAdjacencyMatrix <-
  function(SupraAdjacencyMatrix,
           Layers,
           Nodes,
           Type = "pagerank",
           r = NULL) {
    Type <- tolower(Type)
    Order <- Layers * Nodes

    SupraStrengthMatrix <- sumR(SupraAdjacencyMatrix, 2)
    # SupraStrengthMatrix = $S_{i, \alpha} + s_{i, \alpha}$
    # count nodes with supra strength $S_{i, \alpha} = 0$
    # nodes without intra- and inter-layer connections
    DisconnectedNodes <- length(SupraStrengthMatrix[SupraStrengthMatrix == 0])
    # count nodes with in-layer strength $s_{i, \alpha} = 0$
    # nodes without intra-layer connections
    in_layer_disconnected_count <- 0

    if (DisconnectedNodes > 0) {
      cat(paste0(
        " #Trapping nodes (no outgoing-links): ",
        DisconnectedNodes,
        "\n"
      ))
    }

    if (Type == "pagerank" || Type == "classical") {
      SupraStrengthMatrix[SupraStrengthMatrix > 0] <-
        1. / SupraStrengthMatrix[SupraStrengthMatrix > 0]
      SupraStrengthMatrix <- diagR(SupraStrengthMatrix, Order, 0)

      SupraTransitionMatrix <-
        SupraStrengthMatrix %*% SupraAdjacencyMatrix
    }
    if (Type == "diffusive") {
      maxStrength <- max(SupraStrengthMatrix)
      DiffMatrix <-
        speye(Order) * maxStrength - diagR(SupraStrengthMatrix, Order, 0)
      #diag(DiffMatrix)[DisconnectedNodes.ids] <- 1

      SupraStrengthMatrix <- diagR(1 / maxStrength, Order, 0)

      SupraTransitionMatrix <-
        SupraStrengthMatrix %*% (SupraAdjacencyMatrix + DiffMatrix)
    }
    # --- new code ---
    # March, 19 2020
    # Giulia
    # ---
    if (Type == "maxent") {
      # leading eigenvalue and corresponding right eigenvector of
      # supraA
      tmp <- GetLargestEigenv(SupraAdjacencyMatrix)
      LeadingEigenvector <- tmp$QMatrix
      LeadingEigenvalue <- tmp$LMatrix
      rm(tmp)
      # main diagonal
      # P^{\alpha \alpha}_{ii} = D_I^{\alpha \alpha} / \lambda_{max}
      SupraTransitionMatrix <- 1 / LeadingEigenvalue * diagR(1 / LeadingEigenvector, Order, 0) %*%
        SupraAdjacencyMatrix %*% diagR(LeadingEigenvector, Order, 0)
    }

    if (Type == "physical") {
      # istantaneous propagation among replicas
      # two factors determine transitions
      # ---
      # 1. separate $S_{i, \alpha}$ and $s_{i, \alpha}$
      #    assumption: no self-loops in the ech layer network
      #    D_i^{\alpha \alpha} = 0 no cost of switching for the same layer
      # strength of vertices discarding inter-layer connections
      strengthList <- lapply(1:Layers, function(alpha) {
        is <- (alpha - 1) * Nodes + 1:Nodes
        # SupraAdjacencyMatrix[is, is] %*% u
        # the following line is faster
        rowSums(SupraAdjacencyMatrix[is, is])
      });
      # nodes strengths WITHOUT inter-layer connections, i.e.
      # $s_{i \alpha}~\forall i, \alpha$
      # StrengthVector <- do.call("rbind", strengthList)
      # or column vector
      StrengthVector <- Matrix::Matrix(unlist(strengthList), nrow = Order,
                                       ncol = 1)
      # $S_{i, \alpha} = supraS - s_{i, \alpha}$
      CouplingStrengthVector <- SupraStrengthMatrix - StrengthVector
      # nodes strength wrt intra- and inter-layers connections
      SupraStrengthMatrix[SupraStrengthMatrix > 0] <-
        1. / CouplingStrengthVector[CouplingStrengthVector > 0]

      # count nodes that are isolated in some layer (without inter-layer links)
      in_layer_disconnected_count <- length(StrengthVector[StrengthVector == 0])
      StrengthVector[StrengthVector > 0] <- 1. / StrengthVector[StrengthVector > 0]
      # allocate matrix of zeros
      SupraTransitionMatrix <- Matrix::Matrix(0, nrow = Order, ncol = Order,
                                              sparse = TRUE)
      for (alpha in 1:Layers) {
        for (beta in 1:Layers) {
          is <- (alpha - 1) * Nodes + 1:Nodes
          js <- (beta - 1) * Nodes + 1:Nodes

          SupraTransitionMatrix[is, js] <-
            SupraAdjacencyMatrix[js, js] *
            diag(SupraAdjacencyMatrix[is, js]) *
            StrengthVector[js] * SupraStrengthMatrix[is]
          diag(SupraTransitionMatrix[is, js]) <- 0
        }
      }
    }

    if (Type == "relaxed-physical") {
      # istantaneous propagation among replicas, with relaxation
      # De Domenico et al. PRX (2015)
      if (is.null(r)) {
        warning("Relaxation parameter not provided; using default value r = .5\n")
        r <- .5
      }
      strengthList <- lapply(1:Layers, function(alpha) {
        is <- (alpha - 1) * Nodes + 1:Nodes
        # SupraAdjacencyMatrix[is, is] %*% u
        # the following line is faster
        Matrix::rowSums(SupraAdjacencyMatrix[is, is])
      });
      # nodes strengths WITHOUT inter-layer connections, i.e. $s_{i \alpha}~\forall i, \alpha$
      # StrengthVector <- do.call("rbind", strengthList)
      # or column vector
      StrengthVector <- Matrix::Matrix(unlist(strengthList), nrow = Order,
                                       ncol = 1)
      # $S_{i} = \sum_{\alpha} s_{i, \alpha}$
      NodeStrengthSum <- Reduce(f = "+", strengthList)
      NodeStrengthSum[NodeStrengthSum > 0] <- 1. / NodeStrengthSum[NodeStrengthSum > 0]
      # nodes strength wrt intra- and inter-layers connections
      # SupraStrengthMatrix[SupraStrengthMatrix > 0] <-
      #   1. / CouplingStrengthVector[CouplingStrengthVector > 0]

      # count nodes that are isolated in some layer (without inter-layer links)
      in_layer_disconnected_count <- length(StrengthVector[StrengthVector == 0])
      StrengthVector[StrengthVector > 0] <- 1. / StrengthVector[StrengthVector > 0]
      # allocate matrix of zeros
      SupraTransitionMatrix <- Matrix::Matrix(0, nrow = Order,
                                              ncol = Order, sparse = T)
      for (alpha in 1:Layers) {
        for (beta in 1:Layers) {
          delta_alpha_beta <- ifelse(alpha == beta, 1, 0)
          is <- (alpha - 1) * Nodes + 1:Nodes
          js <- (beta - 1) * Nodes + 1:Nodes
          SupraTransitionMatrix[is, js] <-
            (1 - r) * delta_alpha_beta * SupraAdjacencyMatrix[js, js] * StrengthVector[js] +
            r * SupraAdjacencyMatrix[js, js] * NodeStrengthSum
          # diag(SupraTransitionMatrix[is, js]) <- 0
        }
      }
    }
    # --- end new code ---
    alpha <- NA
    if (Type == "pagerank") {
      alpha <- 0.85
    } else if (Type == "classical") {
      alpha <- 1
    } else {
      # all other types, i.e. diffusive | maxent | physical
      alpha <- 1
    }
    # ---
    # disconnected nodes
    # an issue for PageRank, classical and physical RWs
    # ---
    if (DisconnectedNodes > 0) {
      DisconnectedNodes.ids <- Matrix::which(sumR(SupraTransitionMatrix, 2) == 0)

      if (Type == "pagerank") {
        cat(paste0(
          " #Using uniform teleportation from nodes with no outgoing links\n"
        ))

        #to normalize correctly in the case of nodes with no outgoing links:
        SupraTransitionMatrix[DisconnectedNodes.ids, ] <- 1 / Order
        SupraTransitionMatrix[-DisconnectedNodes.ids, ] <-
          alpha * SupraTransitionMatrix[-DisconnectedNodes.ids, ] +
          (1 - alpha) / Order
      }
      if (Type == "classical") {
        #not required for diffusive
        cat(paste0(" #Using self-loops for isolated nodes\n"))
        # to normalize correctly in the case of nodes with no outgoing links
        # set:
        # diag(SupraTransitionMatrix)[DisconnectedNodes.ids] <- 1
        # BUT this choice leads to higher occupation probability for the
        # disconnected nodes!
        #
        # The following choice is more democratic and restores the fact that
        # disconnected nodes should reduce their overall occupation probability
        # replace the whole row by 1 / NL
        # the random walker is teleported anywhere in the multiplex with equal
        # probability.
        SupraTransitionMatrix[DisconnectedNodes.ids, ] <- 1 / Order
      }
      # --- new code Giulia ---
      if (Type == "maxent") {
        cat(paste0(" #Using uniform teleportation from nodes with no outgoing links\n"))
        # cat(paste0(" #Using self-loops for isolated nodes\n"))
        # to normalize correctly in the case of nodes with no outgoing links
        SupraTransitionMatrix[DisconnectedNodes.ids, ] <- 1. / Order
      }
      if (Type == "physical") {
        # cat(paste0(" #Using self-loops for isolated nodes\n"))
        # diag(SupraTransitionMatrix)[DisconnectedNodes.ids] <- 1
        Matrix::diag(SupraTransitionMatrix)[DisconnectedNodes.ids] <- 1
        if (in_layer_disconnected_count > 0) {
          # there are also nodes that are isolated in some layer(s)
          # i.e. such that $s_{i, \alpha} = 0}
          in_layer_disconnected_ids <- Matrix::which(StrengthVector == 0)
          for (beta in 1:Layers) {
            is <- (beta - 1) * Nodes + (in_layer_disconnected_ids %% Nodes)
            if (length(is) == 1) {
              SupraTransitionMatrix[is, in_layer_disconnected_ids] <- 1 / CouplingStrengthVector[is]
            } else {
              Matrix::diag(SupraTransitionMatrix[is, in_layer_disconnected_ids]) <- 1 / CouplingStrengthVector[is]
            }
          }
        }
      }
      if (Type == "relaxed-physical") {
        # diag(SupraTransitionMatrix)[DisconnectedNodes.ids] <- 1
        SupraTransitionMatrix[DisconnectedNodes.ids, ] <- 1 / Order
        if (in_layer_disconnected_count > 0) {
          # there are also nodes that are isolated in some layer(s)
          # i.e. such that $s_{i, \alpha} = 0}
          in_layer_disconnected_ids <- Matrix::which(StrengthVector == 0)
          if (length(in_layer_disconnected_ids) > 1) {
            Matrix::diag(SupraTransitionMatrix[in_layer_disconnected_ids, in_layer_disconnected_ids]) <- (1 - r) * 1
          } else {
            SupraTransitionMatrix[in_layer_disconnected_ids, in_layer_disconnected_ids] <- (1 - r) * 1
          }
        }
      }
      # --- end new code ---
    } else {
      # i.e. DisconnectedNode == 0
      # we still have to correct for in-layer disconnected nodes
      if (Type == "physical") {
        if (in_layer_disconnected_count > 0) {
          # there are also nodes that are isolated in some layer(s)
          # i.e. such that $s_{i, \alpha} = 0}
          in_layer_disconnected_ids <- Matrix::which(StrengthVector == 0)
          for (beta in 1:Layers) {
            is <- (beta - 1) * Nodes + (in_layer_disconnected_ids %% Nodes)
            if (length(is) == 1) {
              SupraTransitionMatrix[is, in_layer_disconnected_ids] <- 1 / CouplingStrengthVector[is]
            } else {
              Matrix::diag(SupraTransitionMatrix[is, in_layer_disconnected_ids]) <- 1 / CouplingStrengthVector[is]
            }
          }
        }
      }
      if (Type == "relaxed-physical") {
        if (in_layer_disconnected_count > 0) {
          # there are also nodes that are isolated in some layer(s)
          # i.e. such that $s_{i, \alpha} = 0}
          in_layer_disconnected_ids <- Matrix::which(StrengthVector == 0)
          if (length(in_layer_disconnected_ids) > 1) {
            Matrix::diag(SupraTransitionMatrix[in_layer_disconnected_ids, in_layer_disconnected_ids]) <- (1 - r) * 1
          } else {
            SupraTransitionMatrix[in_layer_disconnected_ids, in_layer_disconnected_ids] <- (1 - r) * 1
          }
        }
      }
      # case without disconnected nodes, i.e. DisconnectedNodes == 0
      # and in_layer_disconnected_count == 0
      # alpha in {.85 (PageRank), 1 (classical), 1 (diffusive, maxent, physical,
      #   realxed-physical)}
      SupraTransitionMatrix <-
        alpha * SupraTransitionMatrix + (1 - alpha) / Order
    }

    # reset counter of disconnected nodes
    # no more disconnected nodes
    DisconnectedNodes <- 0


    # check stochasticity of transition matrix, equivalently
    # if sum_k (sum_l supraA[k, l]) == NL
    if (abs(sumR(sumR(SupraTransitionMatrix, 2), 1) - Order + DisconnectedNodes) > 1e-6) {
      stop(
        paste(
          "  BuildSupraTransitionMatrixFromSupraAdjacencyMatrix: ERROR! Problems in building the supra-transition matrix -> ",
          abs(sumR(sumR(
            SupraTransitionMatrix, 2
          ), 1) - Order + DisconnectedNodes),
          ". Aborting process."
        )
      )
    }
    return(Matrix::drop0(SupraTransitionMatrix))
  }

#' Random walk transition matrix for an edge-coloured network.
#' Given the node tensor for the edge-coloured multi-layer network
#'   the function builds the supra transition matrix for a discrete time RW.
#'
#' @param NodesTensor list of adjacency matrices, expected to be aligned (a node
#'    has same index in any layer).
#' @param Layers integer, number of layers
#' @param Nodes integer, number of nodes
#' @param Type the type of the random walk, default \code{"classical"}.
#'    Other types: \code{c("pagerank", "diffusive", "maxent")}.
#'    For details on the transition rules see Refs.2-3.
#' @return a square matrix of dimension \code{Nodes} of transition probabilities for a
#'    discrete-time random walk on the given edge-coloured network.
#' @references
#'   1. De Domenico, M., Set al. (2013). Mathematical formulation of
#'     multilayer networks. Physical Review X, 3(4), 041022.
#'     \href{https://doi.org/10.1103/PhysRevX.3.041022}{doi 10.1103/PhysRevX.3.041022}
#'   2. De Domenico, M., et al. (2014). Navigability of interconnected networks
#'     under random failures. PNAS, 111(23).
#'     \href{https://doi.org/10.1073/pnas.1318469111}{doi 10.1073/pnas.1318469111}
#'   3. Bertagnolli, G., & De Domenico, M. (2020). Diffusion Geometry of
#'     Multiplex and Interdependent Systems.
#'     \href{http://arxiv.org/abs/2006.13032}{arxiv preprint arxiv:2006.13032}
#' @export
BuildTransitionMatrixFromEdgeColoredMatrices <- function(NodesTensor,
                                                         Layers,
                                                         Nodes,
                                                         Type = "classical"
) {

  # Todo: physical;
  # Todo:
  # All layers are weighted equally, for the moment
  # It can be generalized by providing a vector of layerWeights that can be used instead
  Type <- tolower(Type)
  Identity <- speye(Nodes)
  TransitionMatrix <- Matrix::Matrix(0, Nodes, Nodes)

  for (l in 1:Layers) {
    u <- ones(Nodes, 1)
    degs <- NodesTensor[[l]] %*% u
    DisconnectedNodes <- sum(degs == 0)
    if (DisconnectedNodes > 0) {
      cat(paste0(
        " #Trapping nodes (no outgoing-links): ",
        DisconnectedNodes,
        "\n"
      ))
    }
    # --- new code ---
    # March, 19 2020
    # Giulia
    # ---
    if (Type == "pagerank" || Type == "classical") {
      invD <- 1 / degs
      invD[Matrix::which(is.infinite(invD))] <- 0
      invD <- diagR(invD, Nodes, 0)

      TransitionMatrix <- TransitionMatrix + invD %*% NodesTensor[[l]]
    }
    if (Type == "diffusive") {
      # s_{max} max degree in layer s
      maxStrength <- max(degs)
      # D_s = s_{max} I - D
      DiffMatrix <- speye(Nodes) * maxStrength - diagR(degs, Nodes, 0)

      StrengthMatrix <- diagR(1 / maxStrength, Nodes, 0)
      # T = 1 / s_max I (A + D_s)
      TransitionMatrix <- StrengthMatrix %*% (NodesTensor[[l]] + DiffMatrix)
    }
    if (Type == "maxent") {
      tmp <- eigen(NodesTensor[[l]], only.values = F)
      leading_eigenvalue <- tmp$values[which.max(tmp$values)]
      leading_eigenvector <- c(tmp$vectors[, which.max(tmp$values)])
      if (all(leading_eigenvector[which(leading_eigenvector != 0)] < 0)) {
        leading_eigenvector <- -leading_eigenvector
      }
      rm(tmp)

      TransitionMatrix <- 1. / leading_eigenvalue *
        Matrix::Matrix(diag(1 / leading_eigenvector)) %*%
        NodesTensor[[l]] %*%
        Matrix::Matrix(diag(leading_eigenvector))

      # check
      if (abs(sum(rowSums(as.matrix(TransitionMatrix))) - Nodes) > 1e-6) {
        stop(
          paste0(
            " Problems in building the MERW-transition matrix -> ",
            abs(sum(rowSums(as.matrix(T))) - Nodes), " at layer ->", l,
            ". Aborting process."
          )
        )
      }
    }
  }
  # --- end new code

  # here one should correct for node multiplexity:
  # this step is required to adjust transition probabilities in case of nodes missing
  # in some layers, however, since we do this correction in the function
  # GetNormalizedLaplacianMatrix, we are safe
  # TransitionMatrix <- TransitionMatrix/Layers

  existingNodes <- vector("list", Layers)
  nodeMultiplexity <- rep(0, Nodes)

  for (l in 1:Layers) {
    #find cols and rows where sum > zero to identify connected nodes
    cols <- Matrix::which(sumR(NodesTensor[[l]], 2) != 0)
    rows <- Matrix::which(sumR(NodesTensor[[l]], 1) != 0)

    #merge the two (this approach is necessary to deal also with directed networks)
    existingNodes[[l]] <- union(cols, rows)
    nodeMultiplexity[existingNodes[[l]]] <-
      nodeMultiplexity[existingNodes[[l]]] + 1
  }
  nodeMultiplexity <- cbind(c(nodeMultiplexity)) / Layers

  # correct the transition matrix by L times node multiplexity
  TransitionMatrix <-
    TransitionMatrix / kron(Layers * nodeMultiplexity, ones(1, Nodes))

  AggregateMatrix <- GetAggregateMatrix(NodesTensor, Layers, Nodes)

  # Nodes should be reachable with probability 0 (if disconnected across all layers)
  # or 1 (if they are connected in at least one layer)
  DisconnectedNodes.ids <- Matrix::which(sumR(AggregateMatrix, 2) < 1e-8)
  DisconnectedNodes <- length(DisconnectedNodes.ids)

  if (DisconnectedNodes > 0) {
    stop(
      "ERROR! BuildTransitionMatrixFromEdgeColoredMatrices does not allow for networks with disconnected nodes."
    )
  }

  if (abs(sumR(sumR(TransitionMatrix, 2), 1) - Nodes) > 1e-8) {
    stop(
      "ERROR! BuildTransitionMatrixFromEdgeColoredMatrices leads to a wrong transition matrix."
    )
  }

  # TransitionMatrix[which(sumR(TransitionMatrix,2)==0),] <-

  # which( abs(sumR(TransitionMatrix,2)-1)>1e-8 )

  # if(DisconnectedNodes>0){
  #     #to normalize correctly in the case of nodes with no outgoing links:
  #     SupraTransitionMatrix[which(sumR(SupraTransitionMatrix,2)==0),] <- 1/Order
  #     SupraTransitionMatrix[which(sumR(SupraTransitionMatrix,2)!=0),] <- alpha * SupraTransitionMatrix[which(sumR(SupraTransitionMatrix,2)!=0),] + (1-alpha)/Order
  # }else {
  #     SupraTransitionMatrix <- alpha * SupraTransitionMatrix + (1-alpha)/Order
  # }
  # TransitionMatrix <- TransitionMatrix/Layers


  return(Matrix::drop0(TransitionMatrix))
}

#' Returns a Transition Matrix from a single Layer
#'
#'
#' @param AdjacencyMatrix matrix
#' @param Nodes scalar, number of nodes
#' @return Transition Matrix
#' @export
BuildTransitionMatrixFromSingleLayer <-
  function(AdjacencyMatrix, Nodes) {
    return(BuildTransitionMatrixFromEdgeColoredMatrices(list(AdjacencyMatrix), 1, Nodes))
  }


#' Returns Multi Random Walk Centrality
#'
#'
#' @param SupraAdjacencyMatrix the supra-adjacency matrix
#' @param Layers scalar, number of layers, number of layers
#' @param Nodes scalar, number of nodes
#' @param Type string,  "classical", "pagerank", or "diffusive"
#' @param Method string,  "multilayer", or "perlayer"
#' @return
#' Vector of Multi-RandomWalk centralities
#' @references
#' De Domenico, M., Solé-Ribalta, A., Omodei, E. et al. Ranking in interconnected multilayer
#'   networks reveals versatile nodes. Nat Commun 6, 6868 (2015).
#'   \href{https://doi.org/10.1038/ncomms7868}{doi 10.1038/ncomms7868}
#' @export
GetMultiRWCentrality <-
  function(SupraAdjacencyMatrix,
           Layers,
           Nodes,
           Type = "classical",
           Method = "multilayer") {
    SupraTransitionMatrix <- BuildSupraTransitionMatrixFromSupraAdjacencyMatrix(
      SupraAdjacencyMatrix, Layers, Nodes, Type = Type
    )

    # we pass the transpose of the transition matrix to get the left eigenvectors
    tmp <- GetLargestEigenv(Matrix::t(SupraTransitionMatrix))
    LeadingEigenvector <- tmp$QMatrix
    LeadingEigenvalue <- tmp$LMatrix

    if (abs(LeadingEigenvalue - 1) > 1e-6) {
      stop(
        paste(
          "  GetRWOverallOccupationProbability: ERROR! Expected leading eigenvalue equal to 1, obtained",
          LeadingEigenvalue,
          ". Aborting process."
        )
      )
    }

    CentralityVector <- LeadingEigenvector / sum(LeadingEigenvector)

    if (Method == "multilayer") {
      CentralityVector <- sumR(reshapeR(CentralityVector, Nodes, Layers), 2)
    }

    CentralityVector <- CentralityVector / max(CentralityVector)


    return(CentralityVector)
  }


#' Calculates Multi-PageRank-Centrality (based on Multi-RandomWalk-Centrality)
#'
#'
#' @param SupraAdjacencyMatrix the supra-adjacency matrix
#' @param Layers scalar, number of layers
#' @param Nodes scalar, number of nodes
#' @return
#' Vector of PageRank centralities
#' @references
#' De Domenico, M., Solé-Ribalta, A., Omodei, E. et al. Ranking in interconnected multilayer
#'   networks reveals versatile nodes. Nat Commun 6, 6868 (2015).
#'   \href{https://doi.org/10.1038/ncomms7868}{doi 10.1038/ncomms7868}
#' @export
GetMultiPageRankCentrality <-
  function(SupraAdjacencyMatrix, Layers, Nodes) {
    return(GetMultiRWCentrality(SupraAdjacencyMatrix, Layers, Nodes, Type =
                                  "pagerank"))
  }


###################################################################
## CONNECTED COMPONENTS IN MULTILAYER NETWORKS
###################################################################


#' Return the component assignment for each node
#'
#'
#' @param SupraAdjacencyMatrix the supra-adjacency matrix
#' @param Layers scalar, number of layers
#' @param Nodes scalar, number of nodes
#' @return
#' List of Connected Components
#' @export
GetConnectedComponents <-
  function(SupraAdjacencyMatrix, Layers, Nodes) {
    g <-
      igraph::as.undirected(igraph::graph.adjacency(SupraAdjacencyMatrix))
    clu <- igraph::components(g)
    Components <- as.numeric(clu$membership)
    ComponentsSize <- as.numeric(clu$csize)

    #first we have to check if the same entity is assigned to the same component
    #eg, if node A in layer 1 is assigned to component 1 and node A in layer 2 is assigned to component 2
    #then it makes no sense to collapse the information: if they are a unique entity, the nodes
    #should be assigned to the same component, and this happens if they are interconnected or
    #if some of the replicas are isolated components while the others are interconnected

    if (Layers > 1) {
      newComponents <- rep(0, Nodes)
      for (n in 1:Nodes) {
        comp <- Components[n]  #the component assigned to n in layer 1
        newComponents[n] <- comp

        for (l in 2:Layers) {
          ctmp <- Components[(l - 1) * Nodes + n]
          if (ctmp != comp) {
            #check if it is isolated
            if (ComponentsSize[ctmp] != 1 &&
                ComponentsSize[comp] != 1) {
              cat("  Impossible to find meaningful connected components\n")
              cat(
                paste(
                  "  Node",
                  n,
                  "in layer 1 is in component",
                  comp,
                  "(size",
                  ComponentsSize[comp],
                  ") while\n"
                )
              )
              cat(
                paste(
                  "  Node",
                  n,
                  "(abs id:",
                  (l - 1) * Nodes + n,
                  ") in layer",
                  l,
                  "is in component",
                  ctmp,
                  "(size",
                  ComponentsSize[ctmp],
                  ")\n"
                )
              )
              cat("  Aborting process.\n")
            }
          }
        }
      }

      Components <- rep(0, Nodes)
      comps <- unique(newComponents)

      #readjust the components label
      for (i in 1:length(comps)) {
        Components[which(newComponents == comps[i])] <- i
      }
    }

    return(Components)
  }


#' Return the largest connected component
#'
#'
#' @param SupraAdjacencyMatrix the supra-adjacency matrix
#' @param Layers scalar, number of layers
#' @param Nodes scalar, number of nodes
#' @return
#' Largest Conected Component
#' @export
GetGiantConnectedComponent <-
  function(SupraAdjacencyMatrix, Layers, Nodes) {
    Components <-
      GetConnectedComponents(SupraAdjacencyMatrix, Layers, Nodes)
    tmp <- table(Components)

    lcc.id <- as.numeric(names(tmp[which.max(tmp)]))

    lcc <- which(Components == lcc.id)

    return(lcc)
  }


###################################################################
## REDUCIBILITY OF MULTILAYER NETWORKS
###################################################################

#' Calculate the Reducibility of a multilayer network
#'
#'
#' @param SupraAdjacencyMatrix the supra-adjacency matrix
#' @param Layers scalar, number of layers
#' @param Nodes scalar, number of nodes
#' @param Method string
#' @param Type string,  "Ordinal" or "Categorical"
#' @return
#' A list which element are: the Jensen–Shannon distance, the global quality function, and the relative quality function
#' @references
#' De Domenico, M., et al. Structural reducibility of multilayer networks. Nat Commun 6, 6864 (2015).
#'   \href{https://doi.org/10.1038/ncomms7864}{doi 10.1038/ncomms7864}
#'
#' De Domenico, M., Set al. (2013). Mathematical formulation of
#'     multilayer networks. Physical Review X, 3(4), 041022.
#'     \href{https://doi.org/10.1103/PhysRevX.3.041022}{doi 10.1103/PhysRevX.3.041022}
#' @export
GetMultilayerReducibility <-
  function(SupraAdjacencyMatrix,
           Layers,
           Nodes,
           Method,
           Type) {
    NodesTensor <-
      SupraAdjacencyToNodesTensor(SupraAdjacencyMatrix, Layers, Nodes)

    if (Layers > 1) {
      #################################
      # VN Entropies
      #################################
      #single layer entropy
      SingleLayerEntropy <- rep(NA, Layers)

      for (i in 1:Layers) {
        SingleLayerEntropy[i] <-
          GetRenyiEntropyFromAdjacencyMatrix(NodesTensor[[i]], 1)
        if (SingleLayerEntropy[i] < 1e-12)
          SingleLayerEntropy[i] <- 0

        print(paste("DEBUG:", i, SingleLayerEntropy[i]))
      }

      ###########################
      #JSD
      ###########################
      JSD <- Matrix::Matrix(0, Layers, Layers)

      if (Type == "Ordinal") {
        #this line is required, because a 0 means no distance..
        JSD <- Matrix::Matrix(1, Layers, Layers)

        for (i in 1:(Layers - 1)) {
          j <- i + 1
          JSD[i, j] <-
            GetJensenShannonDivergence(NodesTensor[[i]],
                                       NodesTensor[[j]],
                                       SingleLayerEntropy[i],
                                       SingleLayerEntropy[j])
          #JSD[j,i] <- JSD[i,j]
        }
      }
      if (Type == "Categorical") {
        for (i in 1:(Layers - 1)) {
          for (j in (i + 1):Layers) {
            JSD[i, j] <-
              GetJensenShannonDivergence(NodesTensor[[i]],
                                         NodesTensor[[j]],
                                         SingleLayerEntropy[i],
                                         SingleLayerEntropy[j])
            JSD[j, i] <- JSD[i, j]
          }
        }
      }
      JSD <- sqrt(JSD)

      #Quality function
      hc <- stats::hclust(stats::as.dist(JSD), method = Method)

      ## list of merging operations
      #see http://stackoverflow.com/questions/18215184/how-to-print-the-order-of-hierarchical-clustering-in-r

      MergeMatrix <- hc$merge

      #aggregate: I dont use the function in the library to avoid calculating again the nodesTensor
      AggregateMatrix <- NodesTensor[[1]]
      for (i in 2:Layers) {
        AggregateMatrix <- AggregateMatrix + NodesTensor[[i]]
      }

      #aggregate entropy
      AggregateEntropy <-
        GetRenyiEntropyFromAdjacencyMatrix(AggregateMatrix, 1)

      print(paste("DEBUG: Aggregate entropy", AggregateEntropy))
      print(MergeMatrix)

      #step zero: full colored-edge graph against fully aggregated network
      gQualityFunction <- rep(0, Layers)
      relgQualityFunction <- rep(0, Layers)

      cntCurrentLayers <- sum(SingleLayerEntropy > 0)
      gQualityFunction[1] <-
        cntCurrentLayers * AggregateEntropy - sum(SingleLayerEntropy[SingleLayerEntropy >
                                                                       0])

      relgQualityFunction[1] <-
        gQualityFunction[1] / (cntCurrentLayers * AggregateEntropy)

      MergedTensor <- list()
      MergedEntropy <- list()
      for (m in 1:(Layers - 1)) {
        if (MergeMatrix[m, 1] < 0) {
          #we must use the network in NodesTensor
          A <- NodesTensor[[-MergeMatrix[m, 1]]]
          entropyA <- SingleLayerEntropy[-MergeMatrix[m, 1]]
          SingleLayerEntropy[-MergeMatrix[m, 1]] <- 0
        } else {
          #we must use the network stored in MergedTensor
          A <- MergedTensor[[MergeMatrix[m, 1]]]
          entropyA <- MergedEntropy[[MergeMatrix[m, 1]]]
          MergedEntropy[[MergeMatrix[m, 1]]] <- 0
        }

        if (MergeMatrix[m, 2] < 0) {
          #we must use the network in NodesTensor
          B <- NodesTensor[[-MergeMatrix[m, 2]]]
          entropyB <- SingleLayerEntropy[-MergeMatrix[m, 2]]
          SingleLayerEntropy[-MergeMatrix[m, 2]] <- 0
        } else {
          #we must use the network stored in MergedTensor
          B <- MergedTensor[[MergeMatrix[m, 2]]]
          entropyB <- MergedEntropy[[MergeMatrix[m, 2]]]
          MergedEntropy[[MergeMatrix[m, 2]]] <- 0
        }

        MergedTensor[[m]] <- A + B
        tmpLayerEntropy <-
          GetRenyiEntropyFromAdjacencyMatrix(MergedTensor[[m]], 1)
        if (abs(tmpLayerEntropy) < 1e-12)
          tmpLayerEntropy <- 0
        MergedEntropy[[m]] <- tmpLayerEntropy

        diffEntropy <- 2 * tmpLayerEntropy - (entropyA + entropyB)
        reldiffEntropy <- diffEntropy / (2 * tmpLayerEntropy)

        cntCurrentLayers <- Layers - m

        gQualityFunction[m + 1] <-
          sum(SingleLayerEntropy[SingleLayerEntropy > 0])

        for (i in 1:m) {
          # the current merge is accounted by position m
          if (MergedEntropy[[i]] > 0) {
            #resetting the values as above, will guarantee that we consider only layers
            #that are still to be merged
            gQualityFunction[m + 1] <-
              gQualityFunction[m + 1] + MergedEntropy[[i]]
          }
        }
        gQualityFunction[m + 1] <-
          cntCurrentLayers * AggregateEntropy - gQualityFunction[m + 1]
        relgQualityFunction[m + 1] <-
          gQualityFunction[m + 1] / (cntCurrentLayers * AggregateEntropy)
      }
    }

    return(
      list(
        JSD = JSD,
        gQualityFunction = gQualityFunction,
        relgQualityFunction = relgQualityFunction
      )
    )
  }

###################################################################
## BETA FUNCTIONS
###################################################################

#' Return the closed triangles from a monoplex network
#'
#'
#' @param g graph
#' @return
#' A dataframe containing the triangles of the network
#' @references
#' De Domenico, M., Set al. (2013). Mathematical formulation of
#'     multilayer networks. Physical Review X, 3(4), 041022.
#'     \href{https://doi.org/10.1103/PhysRevX.3.041022}{doi 10.1103/PhysRevX.3.041022}
#'
#' Cozzo, E.,Kivela, M., De Domenico, M., et al, New J. of Phys. 17, 073029 (2015)
#'   \href{https://doi.org/10.1088/1367-2630/17/7/073029}{doi 10.1088/1367-2630/17/7/073029}
#' @export
GetMonoplexTriads <- function(g) {
  #triangles with node IDs
  triangles <- igraph::cliques(g, min = 3, max = 3)
  #sort the IDs for removing duplicates later
  triangles <- lapply(triangles, function(x)
    sort(x))
  triangles.df <-
    data.frame(matrix(unlist(triangles), ncol = 3, byrow = T))

  return(triangles.df[!duplicated(triangles.df), ])
}


#' Return the closed triangles in single layers, aggregate and multilayer network
#'
#'
#' @param g.list, a list of igraph objects encoding layers
#' @param g.agg, igraph object for the aggregate network
#' @param verbose logical
#' @return
#' A dataframe containing the closed triangles in single layers, aggregate and multilayer network
#' @references
#' De Domenico, M., Set al. (2013). Mathematical formulation of
#'     multilayer networks. Physical Review X, 3(4), 041022.
#'     \href{https://doi.org/10.1103/PhysRevX.3.041022}{doi 10.1103/PhysRevX.3.041022}
#'
#' Cozzo, E.,Kivela, M., De Domenico, M., et al, New J. of Phys. 17, 073029 (2015)
#'   \href{https://doi.org/10.1088/1367-2630/17/7/073029}{doi 10.1088/1367-2630/17/7/073029}
#' @export
GetMultiplexTriadsFromNetworkList <-
  function(g.list,
           g.agg = NULL,
           verbose = T) {

    Layers <- length(g.list)

    if (is.null(g.agg)) {
      cat("+ Building aggregate network...\n")
      g.agg <- GetAggregateNetworkFromNetworkList(g.list)
    }

    if (verbose)
      cat("+ Analyzing triads...\n")

    if (verbose)
      cat(paste(" -> Aggregate\n"))

    triads.agg <- GetMonoplexTriads(g.agg)

    triads.single <- data.frame()
    null.single.exp <- 0

    for (l in 1:Layers) {
      if (verbose)
        cat(paste(" -> Layer:", l, "\n"))
      Nodes <- igraph::gorder(g.list[[l]])
      triads.single <-
        rbind(triads.single, GetMonoplexTriads(g.list[[l]]))

      #random expectation
      p.ER <- igraph::gsize(g.list[[l]]) / choose(Nodes, 2)
      null.single.exp <- null.single.exp + choose(Nodes, 3) * p.ER ^ 3
    }
    triads.single <- triads.single[!duplicated(triads.single), ]

    p.ER <- igraph::gsize(g.agg) / choose(Nodes, 2)
    null.agg.exp <- choose(Nodes, 3) * p.ER ^ 3

    cat("Summary\n")
    cat("-------\n")
    cat(paste("State triangles:", nrow(triads.single), "\n"))
    cat(paste("Random expectation:", round(null.single.exp, 2), "\n"))
    cat("\n")
    cat(paste("Aggregate triangles:", nrow(triads.agg), "\n"))
    cat(paste("Random expectation:", round(null.agg.exp, 2), "\n"))

    #mux triangles are the ones existing in the aggregate but not in single layers
    # require(tidyverse)
    triads.mux <- dplyr::anti_join(triads.agg, triads.single)
    triads.mux <- triads.mux[!duplicated(triads.mux), ]

    cat(paste("Multiplex triangles:", nrow(triads.mux), "\n"))
    #ratio could be interesting, aggregate should be upper bound

    triads.single$type <- "state"
    triads.agg$type <- "aggregate"
    triads.mux$type <- "multiplex"

    return(rbind(triads.single, triads.agg, triads.mux))
  }


#' Return a list of igraph objects corresponding to layers
#'
#'
#' @param SupraAdjacencyMatrix the supra-adjacency sparse matrix
#' @param Layers scalar, number of layers
#' @param Nodes scalar, number of nodes
#' @return
#' List of igraph objects corresponding to layers
#' @references
#' De Domenico, M., Set al. (2013). Mathematical formulation of
#'     multilayer networks. Physical Review X, 3(4), 041022.
#'     \href{https://doi.org/10.1103/PhysRevX.3.041022}{doi 10.1103/PhysRevX.3.041022}
#' @export
SupraAdjacencyToNetworkList <-
  function(SupraAdjacencyMatrix, Layers, Nodes) {
    NodesTensor <-
      SupraAdjacencyToNodesTensor(SupraAdjacencyMatrix, Layers, Nodes)

    flag_directed <- F
    if (sum(SupraAdjacencyMatrix - Matrix::t(SupraAdjacencyMatrix)) != 0) {
      flag_directed <- T
    }

    g.list <- list()
    for (l in 1:Layers) {
      if (!flag_directed) {
        g.list[[l]] <-
          igraph::as.undirected(
            igraph::graph_from_adjacency_matrix(NodesTensor[[l]], weighted = T, mode =
                                                  "undirected")
          )
      } else {
        g.list[[l]] <-
          igraph::graph_from_adjacency_matrix(NodesTensor[[l]], weighted = T, mode =
                                                "directed")
      }
    }

    return(g.list)
  }


#' Return the closed triangles in single layers, aggregate and multilayer network, but working directly with tensors
#'
#'
#' @param SupraAdjacencyMatrix the supra-adjacency sparse matrix
#' @param Layers scalar, number of layers
#' @param Nodes scalar, number of nodes
#' @return
#' A dataframe containing the closed triangles in single layers, aggregate and multilayer network
#' @references
#' De Domenico, M., Set al. (2013). Mathematical formulation of
#'     multilayer networks. Physical Review X, 3(4), 041022.
#'     \href{https://doi.org/10.1103/PhysRevX.3.041022}{doi 10.1103/PhysRevX.3.041022}
#'
#' Cozzo, E.,Kivela, M., De Domenico, M., et al, New J. of Phys. 17, 073029 (2015)
#'   \href{https://doi.org/10.1088/1367-2630/17/7/073029}{doi 10.1088/1367-2630/17/7/073029}
#' @export
GetMultiplexTriads <- function(SupraAdjacencyMatrix, Layers, Nodes) {
  g.list <-
    SupraAdjacencyToNetworkList(SupraAdjacencyMatrix, Layers, Nodes)
  g.agg <- GetAggregateNetworkFromNetworkList(g.list)

  return(GetMultiplexTriadsFromNetworkList(g.list, g.agg = g.agg, verbose =
                                             T))
}


#' Return the Shortest-Path from the given node to other nodes
#'
#'
#' @param g an igraph object obtained from adjacency matrix
#' @param n integer the node id
#' @param Nodes scalar, number of nodes
#' @return
#' A igraph object containing the list of shortest paths
#' @export
ShortestPathFromNode <- function(g, n, Nodes) {
  #g is an igraph object obtained from adjacency matrix
  #n is the node (integer) id

  #just from node n, calculate shortest paths on state nodes
  x <-
    igraph::shortest_paths(g,
                           from = igraph::V(g)[n],
                           to = igraph::V(g),
                           mode = "all")$vpath

  return(x)
}


#' Return the Shortest-Path from given node to other nodes, with option for counting replicates
#'
#'
#' @param g.ext igraph object obtained from supra-adjacency matrix
#' @param n integer the node id
#' @param Nodes scalar, number of nodes
#' @param countReplicas logical
#' @return
#' A igraph object containing the list of shortest paths
#' @export
MultiShortestPathFromNode <-
  function(g.ext, n, Nodes, countReplicas = F) {
    #g.ext is an igraph object obtained from supra-adjacency matrix
    #n is the node (integer) id

    #just from node n, calculate shortest paths on state nodes
    x <-
      igraph::shortest_paths(
        g.ext,
        from = igraph::V(g.ext)[n],
        to = igraph::V(g.ext),
        mode = "all"
      )$vpath

    #The change of layer can be accounted for or not as an additional step

    if (!countReplicas) {
      #renormalize paths to physical node ids
      y <- lapply(x, function(v)
        unique((v - 1) %% Nodes + 1))
      return(y)
    } else {
      return(x)
    }
  }


#' Return statistics based on multilayer paths
#'
#'
#' @param SupraAdjacencyMatrix the supra-adjacency sparse matrix
#' @param Layers scalar, number of layers
#' @param Nodes scalar, number of nodes
#' @return
#' A a list containing the shortest-path distance matrix, the average path length, and the closeness versatility
#' @export
GetMultiPathStatistics <-
  function(SupraAdjacencyMatrix, Layers, Nodes) {
    if (Layers == 1) {
      #standard monoplex analysis
      #g <- igraph::graph_from_adjacency_matrix(SupraAdjacencyMatrix, weighted=T, mode="undirected")
      g <-
        igraph::graph_from_adjacency_matrix(SupraAdjacencyMatrix, weighted = T)
      #TODO: I removed the restriction to undirected mode only, worth checking that it does not create issues

      DM <- igraph::distances(g, mode = "all")
    } else {
      #multilayer analysis
      #g.ext <- igraph::graph_from_adjacency_matrix(SupraAdjacencyMatrix, weighted=T, mode="undirected")
      #TODO: I removed the restriction to undirected mode only, worth checking that it does not create issues
      g.ext <-
        igraph::graph_from_adjacency_matrix(SupraAdjacencyMatrix, weighted = T)

      DM <- igraph::distances(g.ext, mode = "all")
    }

    ###############
    #distance matrix
    ###############
    if (Layers > 1) {
      DM.blocks <- SupraAdjacencyToBlockTensor(DM, Layers, Nodes)

      DM.min <- DM.blocks[[1, 1]]
      for (l1 in 1:Layers) {
        for (l2 in l1:Layers) {
          DM.min <- pmin(DM.min, DM.blocks[[l1, l2]])
        }
      }
      DM <- DM.min
    }

    ###############
    #closeness
    ###############
    #Opsahl, T., Agneessens, F., Skvoretz, J. (2010). Node centrality in weighted networks: Generalizing degree and shortest paths. Social Networks 32, 245-251
    #https://toreopsahl.com/2010/03/20/closeness-centrality-in-networks-with-disconnected-components/

    closeness <-
      unlist(lapply(1:Nodes, function(n)
        mean(1 / DM[n, ][-n])))
    #0 if disconnected, 1 if connected to all other nodes

    ###############
    #avg path length
    ###############
    #this is a personal definition, should be checked.
    #of course, for networks with isolated nodes mean(1/closeness) = Inf, instead the following is safe:
    avg.path.length <- 1 / mean(closeness)

    ###############
    #betweenness
    ###############
    #todo

    return(
      list(
        "distance.matrix" = DM,
        "avg.path.length" = avg.path.length,
        "closeness" = closeness
      )
    )
  }


#' Returns Multi Closeness Centrality
#'
#'
#' @param SupraAdjacencyMatrix the supra-adjacency matrix
#' @param Layers scalar, number of layers
#' @param Nodes scalar, number of nodes
#' @return
#' Vector of Closeness centralities
#' @export
GetMultiClosenessCentrality <-
  function(SupraAdjacencyMatrix, Layers, Nodes) {
    CentralityVector <-
      GetMultiPathStatistics(SupraAdjacencyMatrix, Layers, Nodes)["closeness"]
    return(CentralityVector)
  }


#' Generate multiplex networks with given amount of edge overlapping
#'
#'
#' @param g graph network object
#' @param ov scalar in \eqn{[0, 1]} tuning the overlap
#' @param tol scalar, tolerance between current and desired overlap. Default 1e-3
#' @param verbose logical, default FALSE
#' @return
#' An igraph dataframe containing the rewiring produced
#' @export
GetRewiredLayerWithOverlappingEdges <-
  function(g, ov, tol = 1e-3, verbose = FALSE) {
    if (igraph::is.directed(g)) {
      stop("This function applies to undirected networks only.")
    }

    if (ov < 0 || ov > 1) {
      stop("Bad overlap value.")
    }

    edges <- igraph::get.edges(g, igraph::E(g))
    edges.orig <- edges
    ov.curr <- 1

    while (abs(ov.curr - ov) >= tol) {
      if (verbose)
        cat(paste("  Current Overlap:", ov.curr, "\n"))

      #sample a pair of edges
      eids <- sample(1:nrow(edges), 2, replace = F)

      #check to avoid possible self-loops
      if (edges[eids, ][1, 1] != edges[eids, ][2, 2] &&
          edges[eids, ][1, 2] != edges[eids, ][2, 1]) {
        #check to avoid multiple edges

        edge1 <- c(edges[eids, ][1, 1], edges[eids, ][2, 2])
        edge2 <- c(edges[eids, ][2, 1], edges[eids, ][1, 2])

        check1 <- igraph::are.connected(g, edge1[1], edge1[2])
        check2 <- igraph::are.connected(g, edge2[1], edge2[2])

        if (!check1 && !check2) {
          #swap
          edges[eids[1], 2] <- edge1[2]
          edges[eids[2], 2] <- edge2[2]

          ov.curr <-
            length(intersect(
              paste(edges[, 1], edges[, 2], sep = ","),
              paste(edges.orig[, 1], edges.orig[, 2], sep =
                      ",")
            )) / nrow(edges)


        }
      }
    }

    g.ov <-
      igraph::graph.data.frame(edges, directed = F, vertices = as.character(igraph::V(g)))

    if (!igraph::is.simple(g.ov)) {
      cat("WARNING! The rewiring produced self-loops or multiple edges. This was unlikely.\n")
    }

    if (abs(igraph::gsize(g.ov) - igraph::gsize(g)) > 0) {
      cat("WARNING! The rewiring altered the number of edges. This was unexpected.\n")
    }

    return(g.ov)

  }


#' Generate random multiplex networks with fixed degree sequences, while destroying intra-layer degree correlations, but not inter-layer ones
#'
#'
#' @param g.list list of igraph network objects
#' @param isDirected specifies if the networks are directed or not. Default: NULL (automatic selection)
#' @param method specifies which igraph method to use. Default: vl (slower but more accurate), other options: "simple", "simple.no.multiple"
#' @param verbose logical Default: F (automatic selection)
#' @return
#' A list of igraph network objects
#' @export
GetConfigurationModelTypeIFromNetworkList <-
  function(g.list,
           isDirected = NULL,
           method = "vl",
           verbose = F) {
    #TODO: manage inter-layer links as well
    message("WARNING: this function will not act on inter-layer links, if any.")

    Layers <- length(g.list)
    g.list.cm <- g.list

    if (is.null(isDirected)) {
      #automatic selection
      for (l in 1:Layers) {
        if (igraph::is.directed(g.list[[l]])) {
          deg.out <- igraph::degree(g.list[[l]], mode = "out")
          deg.in <- igraph::degree(g.list[[l]], mode = "in")

          g.list.cm[[l]] <-
            igraph::degree.sequence.game(out.deg = deg.out,
                                         in.deg = deg.in,
                                         method = method)
        } else {
          degs <- igraph::degree(g.list[[l]], mode = "total")
          g.list.cm[[l]] <-
            igraph::degree.sequence.game(out.deg = degs,
                                         in.deg = NULL,
                                         method = method)
        }
      }
    } else {
      #use the one imposed by the user
      for (l in 1:Layers) {
        if (isDirected) {
          deg.out <- igraph::degree(g.list[[l]], mode = "out")
          deg.in <- igraph::degree(g.list[[l]], mode = "in")

          g.list.cm[[l]] <-
            igraph::degree.sequence.game(out.deg = deg.out,
                                         in.deg = deg.in,
                                         method = method)
        } else {
          degs <- igraph::degree(g.list[[l]], mode = "total")
          g.list.cm[[l]] <-
            igraph::degree.sequence.game(out.deg = degs,
                                         in.deg = NULL,
                                         method = method)
        }
      }
    }

    return(g.list.cm)
  }


#' Generate random multiplex networks with fixed degree sequences, while destroying both intra- and inter-layer degree correlations
#'
#'
#' @param g.list list of igraph network objects
#' @param isDirected specifies if the networks are directed or not. Default: NULL (automatic selection)
#' @param method specifies which igraph method to use. Default: vl (slower but more accurate), other options: "simple", "simple.no.multiple"
#' @param verbose logical Default: F (automatic selection)
#' @return
#' A list of igraph network objects
#' @export
GetConfigurationModelTypeIIFromNetworkList <-
  function(g.list,
           isDirected = NULL,
           method = "vl",
           verbose = F) {
    #TODO: manage inter-layer links as well
    message("WARNING: this function will not act on inter-layer links, if any.")

    Layers <- length(g.list)
    g.list.cm <- g.list

    if (is.null(isDirected)) {
      #automatic selection
      for (l in 1:Layers) {
        if (igraph::is.directed(g.list[[l]])) {
          deg.out <- igraph::degree(g.list[[l]], mode = "out")
          deg.in <- igraph::degree(g.list[[l]], mode = "in")

          g.list.cm[[l]] <-
            igraph::degree.sequence.game(out.deg = deg.out,
                                         in.deg = deg.in,
                                         method = method)
          g.list.cm[[l]] <-
            igraph::permute(g.list.cm[[l]], sample(igraph::vcount(g.list.cm[[l]])))
        } else {
          degs <- igraph::degree(g.list[[l]], mode = "total")
          g.list.cm[[l]] <-
            igraph::degree.sequence.game(out.deg = degs,
                                         in.deg = NULL,
                                         method = method)
          g.list.cm[[l]] <-
            igraph::permute(g.list.cm[[l]], sample(igraph::vcount(g.list.cm[[l]])))
        }
      }
    } else {
      #use the one imposed by the user
      for (l in 1:Layers) {
        if (isDirected) {
          deg.out <- igraph::degree(g.list[[l]], mode = "out")
          deg.in <- igraph::degree(g.list[[l]], mode = "in")

          g.list.cm[[l]] <-
            igraph::degree.sequence.game(out.deg = deg.out,
                                         in.deg = deg.in,
                                         method = method)
          g.list.cm[[l]] <-
            igraph::permute(g.list.cm[[l]], sample(igraph::vcount(g.list.cm[[l]])))
        } else {
          degs <- igraph::degree(g.list[[l]], mode = "total")
          g.list.cm[[l]] <-
            igraph::degree.sequence.game(out.deg = degs,
                                         in.deg = NULL,
                                         method = method)
          g.list.cm[[l]] <-
            igraph::permute(g.list.cm[[l]], sample(igraph::vcount(g.list.cm[[l]])))
        }
      }
    }

    return(g.list.cm)
  }


#' Return the largest viable cluster from network list. Assumption: the network is an edge-colored multigraph
#'
#'
#' @param g.list list of igraph network objects
#' @return
#' The largest viable cluster
#' @references
#' Baxter et al, Physical Review Letters 109, 248701 (2012)
#' 10.1103/PhysRevLett.109.248701
#' @export
GetGiantViableComponentFromNetworkList <- function(g.list) {
  Layers <- length(g.list)
  lvc <- NULL
  flag_nochange <- F

  #set node names, because we want to preserve labels while pruning later
  for (l in 1:Layers) {
    igraph::V(g.list[[l]])$name <- as.character(igraph::V(g.list[[l]]))
  }

  iteration <- 0
  while (is.null(lvc) || !flag_nochange) {
    iteration <- iteration + 1
    cat(paste(" LVC Iteration #", iteration, "...", "\n"))
    #calculate the intersection of LCCs
    l <- 1
    clu <- igraph::clusters(g.list[[l]])
    lic <- names(which(clu$membership == which.max(clu$csize)))

    if (Layers > 1) {
      for (l in 2:Layers) {
        clu <- igraph::clusters(g.list[[l]])
        lcc <- names(which(clu$membership == which.max(clu$csize)))
        lic <- intersect(lic, lcc)
      }
    }

    if (is.null(lvc)) {
      lvc <- lic
      flag_nochange <- F
    } else {
      if (length(lic) == length(lvc)) {
        if (all(sort(lic) == sort(lvc))) {
          #stop the algorithm
          flag_nochange <- T
          return(as.numeric(lvc))
        } else {
          flag_nochange <- F
        }
      } else {
        flag_nochange <- F
      }
    }

    if (!flag_nochange) {
      #clear each layer from nodes not in the intersection
      for (l in 1:Layers) {
        g.list[[l]] <- igraph::induced.subgraph(g.list[[l]], vids = lic)
      }
      lvc <- lic
    }
  }

  return(as.numeric(lvc))
}


#' Return the largest viable cluster
#'
#'
#' @param SupraAdjacencyMatrix the supra-adjacency matrix
#' @param Layers scalar, number of layers
#' @param Nodes scalar, number of nodes
#' @return
#' The largest viable cluster
#' @references
#' Baxter et al, Physical Review Letters 109, 248701 (2012)
#' 10.1103/PhysRevLett.109.248701
#' @export
GetGiantViableComponent <-
  function(SupraAdjacencyMatrix, Layers, Nodes) {
    #Return the largest viable cluster. Assumption: the network is an edge-colored multigraph

    g.list <-
      SupraAdjacencyToNetworkList(SupraAdjacencyMatrix, Layers, Nodes)
    return(GetGiantViableComponentFromNetworkList(g.list))
  }


#' Return the largest connected component obtained by intersecting LCC of each layer. Assumption: the network is an edge-colored multigraph
#'
#'
#' @param g.list list of igraph network objects
#' @return
#' The largest connected component
#' @export
GetGiantIntersectionComponentFromNetworkList <- function(g.list) {
  Layers <- length(g.list)

  l <- 1
  clu <- igraph::clusters(g.list[[l]])
  lic <- which(clu$membership == which.max(clu$csize))

  if (Layers > 1) {
    for (l in 2:Layers) {
      clu <- igraph::clusters(g.list[[l]])
      lcc <- which(clu$membership == which.max(clu$csize))
      lic <- intersect(lic, lcc)
    }
  }

  return(lic)
}


#' Return the largest connected component obtained by intersecting LCC of each layer. Assumption: the network is an edge-colored multigraph
#'
#'
#' @param SupraAdjacencyMatrix the supra-adjacency matrix
#' @param Layers scalar, number of layers
#' @param Nodes scalar, number of nodes
#' @return
#' The largest connected component
#' @export
GetGiantIntersectionComponent <-
  function(SupraAdjacencyMatrix, Layers, Nodes) {
    g.list <-
      SupraAdjacencyToNetworkList(SupraAdjacencyMatrix, Layers, Nodes)
    return(GetGiantIntersectionComponentFromNetworkList(g.list))
  }

###################################################################
## OTHER FUNCTIONS
###################################################################


#' Return a Multilayer Network from Muxviz files
#'
#'
#' @param config.file path, to the config file
#' @param isDirected logical, for considering directed networks
#' @param isWeighted logical, for considering weighted networks
#' @param MultisliceType string, "ordinal" or "categorical" or "temporal"
#' @param LayerCouplingStrength used only if format is muxviz edge-colored
#' @param format string, "muxviz edge-colored" or "muxviz general"
#' @param verbose logical, for messages about processing
#' @return
#' A list including nodes, layers and the graph features
#' @export
buildMultilayerNetworkFromMuxvizFiles <-
  function(config.file,
           isDirected,
           isWeighted,
           MultisliceType,
           LayerCouplingStrength = 1,
           format = "muxviz edge-colored",
           verbose = T) {
    if (format == "muxviz edge-colored") {
      # Expected format: layer_file;layer_label;layout_file
      df.config <- utils::read.table(config.file, sep = ";", header = F)
      colnames(df.config) <-
        c("layer.file", "layer.label", "layout.file")
      Layers <- nrow(df.config)
      layerTensor <-
        BuildLayersTensor(
          Layers = Layers,
          OmegaParameter = LayerCouplingStrength,
          MultisliceType = MultisliceType
        )
      layerLabels <- df.config$layer.label

      if (verbose)
        cat(paste("Found", Layers, "layers...\n"))

      layout.file <- unique(as.character(df.config$layout.file))
      if (length(layout.file) > 1)
        stop("More than one layout file specified.")

      #Expected format: nodeID nodeLabel (optional)
      df.layout <- utils::read.table(layout.file, sep = " ", header = T)

      nodeIDs <- df.layout$nodeID
      nodeLabels <- df.layout$nodeLabel
      nodeX <- df.layout$nodeX
      nodeY <- df.layout$nodeY

      Nodes <- length(nodeIDs)
      if (verbose)
        cat(paste("Found", Nodes, "nodes\n"))

      #Read the edge-colored network
      nodeTensor <- list()
      g.list <- list()
      l <- 1
      for (input.file in as.character(df.config$layer.file)) {
        if (verbose)
          cat(paste("  Reading layer from file", input.file, "...\n"))
        edges <- utils::read.table(input.file, header = F)
        if (ncol(edges) == 3) {
          colnames(edges) <- c("from", "to", "weight")
          if (!isWeighted) {
            cat(
              paste(
                "  WARNING! You asked for an unweighted network but weights are found. Assigning 1 by default.\n"
              )
            )
            edges$weight <- 1
          }
        } else if (ncol(edges) == 2) {
          colnames(edges) <- c("from", "to")
          if (isWeighted) {
            cat(
              paste(
                "  WARNING! You assume a weighted network but no weights are found. Assigning 1 by default.\n"
              )
            )
            edges$weight <- 1
          }
        }

        g.list[[l]] <-
          igraph::graph.data.frame(edges, directed = isDirected, vertices = nodeIDs)
        #g.list[[l]] <- simplify(g.list[[l]])
        nodeTensor[[l]] <-
          igraph::as_adjacency_matrix(g.list[[l]], attr = "weight")
        l <- l + 1
      }

      #Build the multilayer adjacency tensor
      M <-
        BuildSupraAdjacencyMatrixFromEdgeColoredMatrices(nodeTensor, layerTensor, Layers, Nodes)

      #Build the aggregate matrix and network
      aggregateTensor <- GetAggregateMatrixFromNetworkList(g.list)
      g.agg <- GetAggregateNetworkFromNetworkList(g.list)

      return(
        list(
          Nodes = Nodes,
          nodeIDs = nodeIDs,
          nodeLabels = nodeLabels,
          nodeX = nodeX,
          nodeY = nodeY,
          nodeTensor = nodeTensor,
          Layers = Layers,
          layerIDs = 1:Layers,
          layerLabels = layerLabels,
          layerTensor = layerTensor,
          adjacencyTensor = M,
          g.list = g.list,
          isDirected = isDirected,
          isWeighted = isWeighted,
          aggregateTensor = aggregateTensor,
          g.agg = g.agg
        )
      )
    } else if (format == "muxviz general") {
      #Expected format: layers_file;layer_label;layout_file
      df.config <- utils::read.table(config.file, sep = ";", header = F)
      colnames(df.config) <-
        c("layers.file", "layer.label.file", "layout.file")

      layerLabels <-
        utils::read.table(as.character(df.config$layer.label.file), header = T)

      #Expected format: node layer node layer weight
      mEdges <-
        utils::read.table(as.character(df.config$layers.file), header = F)

      inter.edges <- mEdges[mEdges[, 2] != mEdges[, 4], ]
      intra.edges <- mEdges[mEdges[, 2] == mEdges[, 4], ]

      Layers <- max(max(mEdges[, 2]), max(mEdges[, 4]))
      Nodes <- max(max(mEdges[, 1]), max(mEdges[, 3]))

      if (verbose)
        cat(paste("Found", Layers, "layers...\n"))

      if (nrow(inter.edges) == 0) {
        cat("Warning: no inter-layer links found, input network is edge-colored.\n")
        cat(
          paste(
            "Applying",
            MultisliceType,
            "coupling with intensity",
            LayerCouplingStrength,
            ".\n"
          )
        )
        layerTensor <-
          BuildLayersTensor(
            Layers = Layers,
            OmegaParameter = LayerCouplingStrength,
            MultisliceType = MultisliceType
          )
      }


      layout.file <- unique(as.character(df.config$layout.file))
      if (length(layout.file) > 1)
        stop("Error: More than one layout file specified.")

      #Expected format: nodeID nodeLabel (optional)
      df.layout <- utils::read.table(layout.file, sep = " ", header = T)

      nodeIDs <- df.layout$nodeID
      nodeLabels <- df.layout$nodeLabel
      nodeX <- df.layout$nodeX
      nodeY <- df.layout$nodeY

      Nodes2 <- length(nodeIDs)

      if (Nodes != Nodes2) {
        stop("Error: Nodes specified in the layout do not match nodes used in the edges list.\n")
      }

      if (verbose)
        cat(paste("Found", Nodes, "nodes\n"))

      if (verbose) {
        cat(paste("  Inter-links:", nrow(inter.edges), "\n"))
        cat(paste("  Intra-links:", nrow(intra.edges), "\n"))
      }

      #Read the layers
      nodeTensor <- list()
      g.list <- list()

      for (l in 1:Layers) {
        if (verbose)
          cat(paste("  Reading layer from file", df.config$layers.file, "...\n"))

        if (ncol(mEdges) == 5) {
          layerEdges[[l]] <- mEdges[mEdges[, 2] == l &
                                      mEdges[, 4] == l, c(1, 3, 5)]
        } else {
          layerEdges[[l]] <- mEdges[mEdges[, 2] == l & mEdges[, 4] == l, c(1, 3)]
        }

        if (ncol(layerEdges[[l]]) == 3) {
          colnames(layerEdges[[l]]) <- c("from", "to", "weight")
          if (!isWeighted) {
            cat(
              paste(
                "  WARNING! You asked for an unweighted network but weights are found. Assigning 1 by default.\n"
              )
            )
            layerEdges[[l]]$weight <- 1
          }
        } else if (ncol(layerEdges[[l]]) == 2) {
          colnames(layerEdges[[l]]) <- c("from", "to")
          if (isWeighted) {
            cat(
              paste(
                "  WARNING! You assume a weighted network but no weights are found. Assigning 1 by default.\n"
              )
            )
            layerEdges[[l]]$weight <- 1
          }
        }

        g.list[[l]] <-
          igraph::graph.data.frame(layerEdges[[l]], directed = isDirected, vertices =
                                     nodeIDs)
        #g.list[[l]] <- simplify(g.list[[l]])
        nodeTensor[[l]] <-
          igraph::as_adjacency_matrix(g.list[[l]], attr = "weight")
      }

      #Build the multilayer adjacency tensor
      M <-
        BuildSupraAdjacencyMatrixFromExtendedEdgelist(mEdges, Layers, Nodes, isDirected)

      #Build the aggregate matrix and network
      aggregateTensor <- GetAggregateMatrixFromNetworkList(g.list)
      g.agg <- GetAggregateNetworkFromNetworkList(g.list)

      return(
        list(
          Nodes = Nodes,
          nodeIDs = nodeIDs,
          nodeLabels = nodeLabels,
          nodeX = nodeX,
          nodeY = nodeY,
          nodeTensor = nodeTensor,
          Layers = Layers,
          layerIDs = 1:Layers,
          layerLabels = layerLabels,
          layerTensor = layerTensor,
          adjacencyTensor = M,
          g.list = g.list,
          isDirected = isDirected,
          isWeighted = isWeighted,
          aggregateTensor = aggregateTensor,
          g.agg = g.agg
        )
      )
    } else {
      stop("Format not recognized.")
    }
  }

#' Return the Versatility Profile for a given centrality
#'
#'
#' @param mux object obtained from buildMultilayerNetworkFromMuxvizFiles
#' @param type string, versatility type (\[multi\]\[in-|out-\]degree,
#'   \[multi\]\[in-|out-\]strength, pagerank, hub, authority, multiplexity, eigenvector, katz, kcore)
#' @return
#' Vector of centralities depending the given type
#' @export
getVersatilityProfile <- function(mux, type) {
  if (type == "degree")
    return(
      GetMultiDegree(
        mux$adjacencyTensor,
        mux$Layers,
        mux$Nodes,
        isDirected = mux$isDirected
      )
    )
  if (type == "strength")
    return(
      GetMultiStrength(
        mux$adjacencyTensor,
        mux$Layers,
        mux$Nodes,
        isDirected = mux$isDirected
      )
    )
  if (type == "in-degree")
    return(
      GetMultiInDegree(
        mux$adjacencyTensor,
        mux$Layers,
        mux$Nodes,
        isDirected = mux$isDirected
      )
    )
  if (type == "out-degree")
    return(
      GetMultiOutDegree(
        mux$adjacencyTensor,
        mux$Layers,
        mux$Nodes,
        isDirected = mux$isDirected
      )
    )
  if (type == "in-strength")
    return(
      GetMultiInStrength(
        mux$adjacencyTensor,
        mux$Layers,
        mux$Nodes,
        isDirected = mux$isDirected
      )
    )
  if (type == "out-strength")
    return(
      GetMultiOutStrength(
        mux$adjacencyTensor,
        mux$Layers,
        mux$Nodes,
        isDirected = mux$isDirected
      )
    )
  if (type == "multi-degree")
    return(
      GetMultiDegreeSum(
        mux$adjacencyTensor,
        mux$Layers,
        mux$Nodes,
        isDirected = mux$isDirected
      )
    )
  if (type == "multi-strength")
    return(
      GetMultiStrengthSum(
        mux$adjacencyTensor,
        mux$Layers,
        mux$Nodes,
        isDirected = mux$isDirected
      )
    )
  if (type == "multi-in-degree")
    return(
      GetMultiInDegreeSum(
        mux$adjacencyTensor,
        mux$Layers,
        mux$Nodes,
        isDirected = mux$isDirected
      )
    )
  if (type == "multi-out-degree")
    return(
      GetMultiOutDegreeSum(
        mux$adjacencyTensor,
        mux$Layers,
        mux$Nodes,
        isDirected = mux$isDirected
      )
    )
  if (type == "multi-in-strength")
    return(
      GetMultiInStrengthSum(
        mux$adjacencyTensor,
        mux$Layers,
        mux$Nodes,
        isDirected = mux$isDirected
      )
    )
  if (type == "multi-out-strength")
    return(
      GetMultiOutStrengthSum(
        mux$adjacencyTensor,
        mux$Layers,
        mux$Nodes,
        isDirected = mux$isDirected
      )
    )
  if (type == "pagerank")
    return(GetMultiPageRankCentrality(mux$adjacencyTensor, mux$Layers, mux$Nodes))
  if (type == "multiplexity")
    return(GetMultiplexityCentrality(mux$adjacencyTensor, mux$Layers, mux$Nodes))
  if (type == "hub")
    return(GetMultiHubCentrality(mux$adjacencyTensor, mux$Layers, mux$Nodes))
  if (type == "authority")
    return(GetMultiAuthCentrality(mux$adjacencyTensor, mux$Layers, mux$Nodes))
  if (type == "katz")
    return(GetMultiKatzCentrality(mux$adjacencyTensor, mux$Layers, mux$Nodes))
  if (type == "kcore")
    return(GetMultiKCoreCentrality(mux$adjacencyTensor, mux$Layers, mux$Nodes))
  if (type == "eigenvector")
    return(GetMultiEigenvectorCentrality(mux$adjacencyTensor, mux$Layers, mux$Nodes))
  if (type == "closeness")
    return(GetMultiClosenessCentrality(mux$adjacencyTensor, mux$Layers, mux$Nodes))
}


###################################################################
## MULTILAYER MOTIFS
###################################################################

#' Calculates the Motifs of a Multilayer from a network list
#'
#'
#' @param g.list list of igraph network objects
#' @param bin.path path, full path where the FANMOD binary executable is located
#' @param motifSize integer, size of motifs to find (3 or 4)
#' @param motifSamples integer, number of samples used to determine approx. # of motifs
#' @param isDirected logical, indicates if the search is for an undirected or directed network
#' @param motifNullModel string, "Local const", "Global const", or "No regard"
#' @param randomNetworks integer, number of random networks
#' @param randomExchangePerEdges integer, number of exchanges per edge
#' @param randomExchangeAttempts  integer, number of exchange attempts per edge
#' @return
#' Motifs Table
#' @references:
#' Sebastian Wernicke and Florian Rasche, Bioinformatics 22, 1152 (2006)
#' \href{https://doi.org/10.1093/bioinformatics/btl038}{doi 10.1093/bioinformatics/btl038}
#'
#' Version implemented by Tomer Benyamini, Yoav Teboulle, Tel-Aviv University
#' \href{github/gtremper/Network-Motif}{https://github.com/gtremper/Network-Motif/tree/master/fanmod/FANMOD-command_line-source}
#' @export
GetMultilayerMotifsFromNetworkList <-
  function(g.list,
           bin.path = NA,
           motifSize = 3,
           motifSamples = 100000,
           isDirected = FALSE,
           motifNullModel = "Local const",
           randomNetworks = 1000,
           randomExchangePerEdges = 3,
           randomExchangeAttempts = 3) {
    if (is.na(bin.path) || !file.exists(bin.path)) {
      stop(
        "Error! You must provide a valid path to the FANMOD bin.
        Likely you will find it in the bin/ folder of muxviz, or you must compile it from source in src/ folder.
        If this is the case, just unzip the fanmod archive and run ./make.sh that will generate executable fanmod.
        Feel free to move the file where you prefer and provide the full path as an argument to this function."
      )
    }

    #to output full numbers
    options(scipen = 999)

    Layers <- length(g.list)

    cat('1/2 Setting up the algorithms...\n')

    #fanmod format assume 0-starting labeling for nodes, 1-starting for layers
    mergedEdgelist <- data.frame()
    layerLabels <- NULL

    for (l in 1:Layers) {
      edges <- igraph::get.edgelist(g.list[[l]])
      mergedEdgelist <-
        rbind(mergedEdgelist,
              data.frame(
                from = edges[, 1] - 1,
                to = edges[, 2] -
                  1,
                layer = l
              ))
      layerLabels <- c(layerLabels, paste0("Layer", l))
    }

    tmpname <- tempfile(pattern = "file", tmpdir = tempdir())
    inputFile <- paste0(tmpname, "_fanmod.edges")
    utils::write.table(
      file = inputFile,
      mergedEdgelist,
      row.names = F,
      col.names = F,
      quote = F
    )
    resultFile <- paste0(tmpname, "_fanmod.csv")

    cat('2/2 Calculating motifs...\n')

    nullModelID <- 2
    if (motifNullModel == "Local const") {
      nullModelID <- 2
    } else if (motifNullModel == "Global const") {
      nullModelID <- 1
    } else if (motifNullModel == "No regard") {
      nullModelID <- 0
    }

    exePath <- bin.path
    exeFlags <- paste(
      as.numeric(motifSize),
      as.numeric(motifSamples),
      1,
      inputFile,
      as.numeric(isDirected),
      0,
      1,
      nullModelID,
      0,
      1,
      0,
      as.numeric(randomNetworks),
      as.numeric(randomExchangePerEdges),
      as.numeric(randomExchangeAttempts),
      resultFile,
      0,
      0
    )

    #call fanmod
    system(paste(exePath, exeFlags), intern = T)

    #read output. Here I could redirect the output inside the R environment.. but
    #for compatibility with the rest of the code I prefer to read a file
    #ID,Adj-Matrix,Frequency,Mean-Freq,Standard-Dev,Z-Score,p-Value
    motifsTable <-
      utils::read.table(
        resultFile,
        header = T,
        sep = ",",
        colClasses = c("character", "character", rep("numeric", 5))
      )

    motifsTable$ID <- 1:nrow(motifsTable)

    #to reset output options
    options(scipen = 0)
    cat('Calculation Completed!\n')

    if (file.exists(paste0(tmpname, "_fanmod.edges")))
      file.remove(paste0(tmpname, "_fanmod.edges"))
    if (file.exists(paste0(tmpname, "_fanmod.csv")))
      file.remove(paste0(tmpname, "_fanmod.csv"))
    if (file.exists(paste0(tmpname, "_fanmod.csv.log")))
      file.remove(paste0(tmpname, "_fanmod.csv.log"))

    return(motifsTable)
  }

#' Return the multilayer motifs.
#' Note that even if inter-links are present, they will be neglected and the network treated as an edge-colored multigraph
#'
#'
#' @param SupraAdjacencyMatrix the supra-adjacency matrix
#' @param Layers scalar, number of layers
#' @param Nodes scalar, number of nodes
#' @param bin.path path, full path where the FANMOD binary executable is located
#' @param motifSize integer, size of motifs to find (3 or 4)
#' @param motifSamples integer, number of samples used to determine approx. # of motifs
#' @param isDirected logical, indicates if the search is for an undirected or directed network
#' @param motifNullModel string, "Local const", "Global const", or "No regard"
#' @param randomNetworks integer, number of random networks
#' @param randomExchangePerEdges integer, number of exchanges per edge
#' @param randomExchangeAttempts  integer, number of exchange attempts per edge
#' @return
#' Motifs Table
#' @export
GetMultilayerMotifs <-
  function(SupraAdjacencyMatrix,
           Layers,
           Nodes,
           bin.path = NA,
           motifSize = 3,
           motifSamples = 100000,
           isDirected = FALSE,
           motifNullModel = "Local const",
           randomNetworks = 1000,
           randomExchangePerEdges = 3,
           randomExchangeAttempts = 3) {
    #
    #they will be neglected and the network treated as an edge-colored multigraph

    g.list <-
      SupraAdjacencyToNetworkList(SupraAdjacencyMatrix, Layers, Nodes)
    return(
      GetMultilayerMotifsFromNetworkList(
        g.list,
        bin.path,
        motifSize,
        motifSamples,
        isDirected,
        motifNullModel,
        randomNetworks,
        randomExchangePerEdges,
        randomExchangeAttempts
      )
    )
  }



###################################################################
## MULTILAYER COMMUNITY DETECTION
###################################################################

#' Return multilayer communities. INFOMAP wrapper
#'
#'
#' @param g.list list of igraph network objects
#' @param bin.path path, full path where the INFOMAP binary executable is located
#' @param isDirected logical, indicates if the analysis is for an undirected or directed network
#' @param seed Integer, Infomap seed
#' @param includeSelfLinks logical
#' @param numTrials Integer, number of Infomap trials to minimize L
#' @param twoLevel logical, two level Infomap comunity detection
#' @param preclusterMultiplex logical
#' @param addMissingPhysicalNodes logical
#' @param hardPartitions logical
#' @param verbose logical
#' @param addAggregateAnalysis logical
#' @param multilayerRelaxRate Float, between 0 and 1
#' @param multilayerJSRelaxRate Float, between 0 and 1
#' @param outputPrefix String
#' @return
#' Community List
#' @references
#' Manlio De Domenico, Andrea Lancichinetti, Alex Arenas, and Martin Rosvall, Physical Review X 5, 011027 (2015)
#' \href{https://doi.org/10.1103/PhysRevX.5.011027}{doi 10.1103/PhysRevX.5.011027}
#' \href{http://www.mapequation.org/code.html}{Source}
#' @export
GetMultiplexCommunities_Infomap <-
  function(g.list,
           bin.path = NA,
           isDirected,
           seed = 12345,
           includeSelfLinks = F,
           numTrials = 100,
           twoLevel = T,
           preclusterMultiplex = F,
           addMissingPhysicalNodes = T,
           hardPartitions = F,
           verbose = T,
           addAggregateAnalysis = T,
           multilayerRelaxRate = NA,
           multilayerJSRelaxRate = NA,
           outputPrefix = "multimap") {
    if (is.na(bin.path) || !file.exists(bin.path)) {
      stop(
        "Error! You must provide a valid path to the INFOMAP bin. Likely you will find it in the bin/ folder of muxviz, or you must compile it from source in src/ folder. If this is the case, just unzip the infomap archive and run  make  that will generate executable Infomap. Feel free to move the file where you prefer and provide the full path as an argument to this function."
      )
    }

    Layers <- length(g.list)

    tmpname <- outputPrefix
    inputFile <- paste0(tmpname, "_infomap.edges")
    if (file.exists(inputFile))
      file.remove(inputFile)
    fileConn <- file(inputFile, open = "at")

    cat('1/2 Setting up the algorithms...\n')

    mergedEdgelist <- data.frame()

    #write in the Infomap multilayer format for edge-colored networks
    #this part is different from the one in GetMultilayerCommunities_Infomap
    writeLines(c("*Intra", "#level node node weight"), fileConn)

    for (l in 1:Layers) {
      edges <- igraph::get.edgelist(g.list[[l]])
      weights <- igraph::E(g.list[[l]])$weight
      if (is.null(weights))
        weights <- rep(1, igraph::gsize(g.list[[l]]))
      mergedEdgelist <- rbind(mergedEdgelist,
                              data.frame(
                                layer = l,
                                from = edges[, 1],
                                to = edges[, 2],
                                weight = weights
                              ))

      if (!isDirected) {
        #this is because multimap requires both directions specified, even for undirected networks
        mergedEdgelist <- rbind(mergedEdgelist,
                                data.frame(
                                  layer = l,
                                  from = edges[, 2],
                                  to = edges[, 1],
                                  weight = weights
                                ))
      }
    }
    utils::write.table(
      mergedEdgelist,
      file = fileConn,
      row.names = F,
      col.names = F,
      quote = F
    )
    close(fileConn)

    cat('2/2 Finding communities...\n')
    cat(' + Multiplex network...\n')

    exePath <- bin.path

    outname <- tmpname
    outdir <- getwd()

    #default flags
    exeFlags <- paste(inputFile, outdir)
    exeFlags <- paste(exeFlags, "--input-format multilayer")
    exeFlags <- paste(exeFlags, "--clu --map --tree --expanded")

    exeFlags <- paste(exeFlags, "--seed", seed)
    exeFlags <- paste(exeFlags, "--num-trials", numTrials)

    if (isDirected) {
      exeFlags <- paste(exeFlags, "-d")
    } else {
      exeFlags <- paste(exeFlags, "-u")
    }

    if (is.na(multilayerRelaxRate) && is.na(multilayerJSRelaxRate)) {
      stop("ERROR! You must specify a non-negative value for the relax rate or the JS relax
           rate.")
    } else if (is.na(multilayerRelaxRate)) {
      # then multilayerJSRelaxRate must be non-negative
      if (multilayerJSRelaxRate >= 0) {
        exeFlags <- paste(
          exeFlags,
          "--multilayer-js-relax-rate",
          multilayerJSRelaxRate
        )
      }
    } else if (is.na(multilayerJSRelaxRate)) {
      # then multilayerRelaxRate must be non-negative
      if (multilayerRelaxRate >= 0) {
        exeFlags <- paste(
          exeFlags,
          "--multilayer-relax-rate",
          multilayerRelaxRate
        )
      }
    } else if (!is.na(multilayerRelaxRate) && !is.na(multilayerJSRelaxRate)) {
      if (multilayerRelaxRate >= 0 && multilayerJSRelaxRate >= 0) {
        stop("ERROR! You must specify either the relax rate or the JS relax rate, not both.")
      }
    }

    if (includeSelfLinks) {
      exeFlags <- paste(exeFlags, "--include-self-links")
    }

    if (twoLevel) {
      exeFlags <- paste(exeFlags, "--two-level")
    }

    if (preclusterMultiplex) {
      exeFlags <- paste(exeFlags, "--pre-cluster-multiplex")
    }

    if (addMissingPhysicalNodes) {
      exeFlags <- paste(exeFlags, "--multilayer-add-missing-nodes")
    }

    if (hardPartitions) {
      exeFlags <- paste(exeFlags, "--hard-partitions")
    }

    if (verbose) {
      exeFlags <- paste(exeFlags, "-vvv")
    }

    exeFlags <- paste(exeFlags, "--out-name", outname)

    # call infomap
    system(paste(exePath, exeFlags), intern = T)


    #read output. Here I could redirect the output inside the R environment.. but
    #for compatibility with the rest of the code I prefer to read a file
    communityList <- list()

    #import the results (clu and modularity value)
    resultFile <- paste0(outputPrefix, "_expanded.clu")
    wmemb_membership <- utils::read.table(resultFile, header = F, sep = " ")

    communityList$membership.multi <- wmemb_membership

    #if(!hardPartitions){
    #same columns regardless of this flag
    colnames(communityList$membership.multi) <-
      c("layer", "node", "module", "flow")
    #}
    #reorder, for easier inspection
    communityList$membership.multi <-
      communityList$membership.multi[order(communityList$membership.multi$layer,
                                           communityList$membership.multi$node), ]


    resultFile <- paste0(outputPrefix, "_expanded.map")
    wtcod <-
      as.numeric(strsplit(readLines(resultFile, n = 5), " ")[[5]][3])

    communityList$codelength.multi <- wtcod

    cat(paste("    Code length Multiplex: ", wtcod, "\n"))
    numComms <- max(wmemb_membership$V3)
    cat(paste("    Communities Multiplex: ", numComms, "\n"))

    communityList$modules.multi <- numComms

    communityList$msize.multi <-
      table(communityList$membership.multi$module)

    #depending on flags, Infomap can transform into layer IDs the id of isolated nodes.
    #let's remove those ones
    communityList$membership.multi <-
      communityList$membership.multi[which(communityList$membership.multi$layer <=
                                             Layers), ]


    #TODO for the future: calculate modularity of the partition. No direct multiplex way from igraph
    #one possibility is to pass the expanded representation of the network
    #but in case of edgecolored the supradjacency matrix would empty off-diagonal
    #resulting in huge modularity due to layers, not modules..
    #igraph::modularity(x, membership, weights = NULL, ...)

    if (addAggregateAnalysis) {
      cat(' + Aggregate network...\n')

      #calculate same things for the aggregate using R-igraph infomap
      g.agg <- GetAggregateNetworkFromNetworkList(g.list)
      infocom <-
        igraph::cluster_infomap(g.agg, modularity = TRUE)
      wmemb_membership_aggregate <-
        as.numeric(igraph::membership(infocom))
      wtcod_aggregate <- igraph::code_len(infocom)

      communityList$membership.aggr <-
        data.frame(node = 1:length(wmemb_membership_aggregate),
                   module = wmemb_membership_aggregate)
      communityList$codelength.aggr <- wtcod_aggregate

      cat(paste("    Code length Aggregate: ", wtcod_aggregate, "\n"))
      numCommsAggr <- max(wmemb_membership_aggregate)
      cat(paste("    Communities Aggregate: ", numCommsAggr, "\n"))

      communityList$modules.aggr <- numCommsAggr
      communityList$msize.aggr <-
        table(communityList$membership.aggr$module)
    }

    cat('Calculation Completed!\n')

    return(communityList)
  }


#' Return multilayer communities for interconnected multilayer networks
#'
#'
#' @param SupraAdjacencyMatrix the supra-adjacency matrix
#' @param Layers scalar, number of layers
#' @param Nodes scalar, number of nodes
#' @param bin.path path, full path where the INFOMAP binary executable is located
#' @param isDirected logical, indicates if the analysis is for an undirected or directed network
#' @param seed Integer, Infomap seed
#' @param includeSelfLinks logical
#' @param numTrials Integer, number of Infomap trials to minimize L
#' @param twoLevel logical, two level Infomap comunity detection
#' @param preclusterMultiplex logical
#' @param addMissingPhysicalNodes logical
#' @param hardPartitions logical
#' @param verbose logical
#' @param addAggregateAnalysis logical
#' @param outputPrefix String
#' @return
#' Community List
#' @references
#' Manlio De Domenico, Andrea Lancichinetti, Alex Arenas, and Martin Rosvall, Physical Review X 5, 011027 (2015)
#' \href{https://doi.org/10.1103/PhysRevX.5.011027}{doi 10.1103/PhysRevX.5.011027}
#' \href{http://www.mapequation.org/code.html}{Source}
#' @export
GetMultilayerCommunities_Infomap <-
  function(SupraAdjacencyMatrix,
           Layers,
           Nodes,
           bin.path = NA,
           isDirected,
           seed = 12345,
           includeSelfLinks = F,
           numTrials = 100,
           twoLevel = T,
           preclusterMultiplex = F,
           addMissingPhysicalNodes = T,
           hardPartitions = F,
           verbose = T,
           addAggregateAnalysis = T,
           outputPrefix = "multimap") {
    #

    if (is.na(bin.path) || !file.exists(bin.path)) {
      stop(
        "Error! You must provide a valid path to the INFOMAP bin.
        Likely you will find it in the bin/ folder of muxviz, or you must compile it from
        source in src/ folder. If this is the case, just unzip the infomap archive and
        run make that will generate executable Infomap.
        Feel free to move the file where you prefer and provide the full path as an
        argument to this function."
      )
    }

    tmpname <- outputPrefix
    inputFile <- paste0(tmpname, "_infomap.edges")
    if (file.exists(inputFile))
      file.remove(inputFile)
    fileConn <- file(inputFile, open = "at")

    cat('1/2 Setting up the algorithms...\n')

    # obtain an extended edgelist representation in format:
    # node.from layer.from node.to layer.to weight
    multilayerEdges <- BuildExtendedEdgelistFromSupraAdjacencyMatrix(
                         SupraAdjacencyMatrix,
                         Layers,
                         Nodes,
                         FALSE
                       )

    # write in the Infomap multilayer format for general multilayer networks
    # this part is different from the one in GetMultiplexCommunities_Infomap

    writeLines(c("*Intra", "#layer node node weight"), fileConn)
    mergedEdgelist <- data.frame()

    submulti <-
      multilayerEdges[multilayerEdges$layer.from == multilayerEdges$layer.to,]
    mergedEdgelist <-
      rbind(
        mergedEdgelist,
        data.frame(
          layer = submulti$layer.from,
          from = submulti$node.from,
          to = submulti$node.to,
          weight = submulti$weight
        )
      )
    if (!isDirected) {
      #this is because multimap requires both directions specified, even for undirected networks
      mergedEdgelist <-
        rbind(
          mergedEdgelist,
          data.frame(
            layer = submulti$layer.from,
            from = submulti$node.to,
            to = submulti$node.from,
            weight = submulti$weight
          )
        )
    }
    utils::write.table(
      mergedEdgelist,
      file = fileConn,
      row.names = F,
      col.names = F,
      quote = F
    )

    writeLines(c("*Inter", "#layer node layer weight"), fileConn)
    mergedEdgelist <- data.frame()

    submulti <-
      multilayerEdges[multilayerEdges$node.from == multilayerEdges$node.to,]
    mergedEdgelist <-
      rbind(
        mergedEdgelist,
        data.frame(
          from = submulti$layer.from,
          node = submulti$node.from,
          to = submulti$layer.to,
          weight = submulti$weight
        )
      )
    if (!isDirected) {
      #this is because multimap requires both directions specified, even for undirected networks
      mergedEdgelist <-
        rbind(
          mergedEdgelist,
          data.frame(
            from = submulti$layer.to,
            node = submulti$node.from,
            to = submulti$layer.from,
            weight = submulti$weight
          )
        )
    }
    utils::write.table(
      mergedEdgelist,
      file = fileConn,
      row.names = F,
      col.names = F,
      quote = F
    )

    close(fileConn)

    cat('2/2 Finding communities...\n')
    cat(' + Multiplex network...\n')

    exePath <- bin.path

    outname <- tmpname
    outdir <- getwd()

    #default flags
    exeFlags <- paste(inputFile, outdir)
    exeFlags <- paste(exeFlags, "--input-format multilayer")
    exeFlags <- paste(exeFlags, "--clu --map --tree --expanded")

    exeFlags <- paste(exeFlags, "--seed", seed)
    exeFlags <- paste(exeFlags, "--num-trials", numTrials)

    if (isDirected) {
      exeFlags <- paste(exeFlags, "-d")
    } else {
      exeFlags <- paste(exeFlags, "-u")
    }

    if (includeSelfLinks) {
      exeFlags <- paste(exeFlags, "--include-self-links")
    }

    if (twoLevel) {
      exeFlags <- paste(exeFlags, "--two-level")
    }

    if (preclusterMultiplex) {
      exeFlags <- paste(exeFlags, "--pre-cluster-multiplex")
    }

    if (addMissingPhysicalNodes) {
      exeFlags <- paste(exeFlags, "--multilayer-add-missing-nodes")
    }

    if (hardPartitions) {
      exeFlags <- paste(exeFlags, "--hard-partitions")
    }

    if (verbose) {
      exeFlags <- paste(exeFlags, "-vvv")
    }

    exeFlags <- paste(exeFlags, "--out-name", outname)

    #call infomap
    system(paste(exePath, exeFlags), intern = T)


    #read output. Here I could redirect the output inside the R environment.. but
    #for compatibility with the rest of the code I prefer to read a file
    communityList <- list()

    #import the results (clu and modularity value)
    resultFile <- paste0(outputPrefix, "_expanded.clu")
    wmemb_membership <- utils::read.table(resultFile, header = F, sep = " ")

    communityList$membership.multi <- wmemb_membership

    #if(!hardPartitions){
    #same columns regardless of this flag
    colnames(communityList$membership.multi) <-
      c("layer", "node", "module", "flow")
    #}
    #reorder, for easier inspection
    communityList$membership.multi <-
      communityList$membership.multi[order(communityList$membership.multi$layer,
                                           communityList$membership.multi$node), ]


    resultFile <- paste0(outputPrefix, "_expanded.map")
    wtcod <-
      as.numeric(strsplit(readLines(resultFile, n = 5), " ")[[5]][3])

    communityList$codelength.multi <- wtcod

    cat(paste("    Code length Multiplex: ", wtcod, "\n"))
    numComms <- max(wmemb_membership$V3)
    cat(paste("    Communities Multiplex: ", numComms, "\n"))

    communityList$modules.multi <- numComms

    communityList$msize.multi <-
      table(communityList$membership.multi$module)

    #depending on flags, Infomap can transform into layer IDs the id of isolated nodes.
    #let's remove those ones
    communityList$membership.multi <-
      communityList$membership.multi[which(communityList$membership.multi$layer <=
                                             Layers), ]

    #TODO for the future: calculate modularity of the partition. No direct multiplex way from igraph
    #one possibility is to pass the expanded representation of the network
    #but in case of edgecolored the supradjacency matrix would empty off-diagonal
    #resulting in huge modularity due to layers, not modules..
    #igraph::modularity(x, membership, weights = NULL, ...)

    if (addAggregateAnalysis) {
      cat(' + Aggregate network...\n')

      #calculate same things for the aggregate using R-igraph infomap
      g.agg <-
        GetAggregateNetworkFromSupraAdjacencyMatrix(SupraAdjacencyMatrix, Layers, Nodes)

      infocom <-
        igraph::cluster_infomap(g.agg, modularity = TRUE)
      wmemb_membership_aggregate <-
        as.numeric(igraph::membership(infocom))
      wtcod_aggregate <- igraph::code_len(infocom)

      communityList$membership.aggr <-
        data.frame(node = 1:length(wmemb_membership_aggregate),
                   module = wmemb_membership_aggregate)
      communityList$codelength.aggr <- wtcod_aggregate

      cat(paste("    Code length Aggregate: ", wtcod_aggregate, "\n"))
      numCommsAggr <- max(wmemb_membership_aggregate)
      cat(paste("    Communities Aggregate: ", numCommsAggr, "\n"))

      communityList$modules.aggr <- numCommsAggr
      communityList$msize.aggr <-
        table(communityList$membership.aggr$module)
    }

    cat('Calculation Completed!\n')

    return(communityList)
  }



#' Get the Coverage Evolution of a Multilayer Network
#'
#'
#' @param SupraTransitionMatrix a Supratransition Matrix
#' @param Layers Layers
#' @param Nodes Layers
#' @param TimeSequence Time sequence
#' @param Approximate logical, use approximate method. Default FALSE
#' @param Approximate.disconnected Numeric for approximate method. Default 0
#' @return data frame coverage \code{rho} across time \code{tau}
#' @references
#' M. De Domenico, A. Sole-Ribalta, S. Gomez, A. Arenas, PNAS 11, 8351 (2014)
#' @export
GetCoverageEvolutionMultilayer <- function(SupraTransitionMatrix,
                                           Layers,
                                           Nodes,
                                           TimeSequence,
                                           Approximate = FALSE,
                                           Approximate.disconnected = 0) {
  Order <- Layers * Nodes
  SupraLaplacianMatrix <- speye(Order) - SupraTransitionMatrix
  cat(paste("  :: Eigendecomposing...", "\n"))
  # I don't use the SolveEigenvalueProblem function because I expect complex numbers
  # in the process

  if (Approximate.disconnected == 0)
    Approximate.disconnected <- Order

  # this cannot be done, because we need the invQ later,
  # and invQ = Matrix::t(Q) only if the Laplacian is symmetric..
  # and for RW it is not.

  # if (Approximate) {
  #     L.eigendec <- RSpectra::eigs(SupraLaplacianMatrix, k=Approximate.disconnected+1)
  # } else {
  #   L.eigendec <- eigen(SupraLaplacianMatrix)
  # }
  L.eigendec <- eigen(SupraLaplacianMatrix)
  QM <- as.matrix(L.eigendec$vectors)

  cat(paste("  :: Inverting...", "\n"))

  invQM <- inv(QM)
  LM <- L.eigendec$values

  zero.idxs <- which(abs(Re(LM)) < 1e-12)
  lambda2.idx <- which.min(LM[-zero.idxs])

  cat(paste("  :: There are", length(zero.idxs), "zero eigenvalues", "\n"))

  lambdas.sort <- sort(LM[-zero.idxs])
  cat(paste("  :: l3 - l2 =", lambdas.sort[2] - lambdas.sort[1], "\n"))
  cat(paste(
    "  :: 90% difference expected for tau >",-log(1 - 0.9) / (lambdas.sort[2] - lambdas.sort[1]),
    "\n"
  ))

  # print(LM)
  # print(paste("Smallest > 0:", which.min(LM[-zero.idxs]), LM[which.min(LM[-zero.idxs])] ))

  cat(paste("  :: Building matrices...", "\n"))
  #Following the notation in the PNAS paper:
  Nu <- list()

  if (!Approximate) {
    lapply(1:Order, function(l) {
      #include here product by P to save time later
      Nu[[l]] <<-
        (QM[, l] %*% Matrix::t(invQM[l,])) %*% as.matrix(SupraTransitionMatrix)
    })
  } else {
    cat(paste(
      "WARNING! Approximation might be very poor if the network is not connected.\n"
    ))
    lapply(c(zero.idxs, lambda2.idx), function(l) {
      #include here product by P to save time later
      Nu[[l]] <<-
        (QM[, l] %*% Matrix::t(invQM[l,])) %*% as.matrix(SupraTransitionMatrix)
    })
  }

  rho.df <- data.frame()

  # DEBUG CROSS CHECK
  # for (l in zero.idxs[2:length(zero.idxs)]) {
  #   print( sum(abs(Nu[[l]] - Nu[[zero.idxs[1]]])) )
  # }

  Coverage.raw <- function(Nu,
                           LM,
                           Layers,
                           Nodes,
                           taus,
                           Approximate,
                           zero.idxs,
                           lambda2.idx) {
    rho <- rep(0, length(taus))
    Order <- Layers * Nodes

    pb <- utils::txtProgressBar(min = 1,
                                max = Nodes,
                                style = 3)

    lapply(1:Nodes, function(i) {
      #Ei <- Matrix::t(as.matrix(kron(ones(1,Layers), CanonicalVector(Nodes,i))))
      Ei <- i + Nodes * (1:Layers - 1)

      #for(j in 1:Nodes){
      lapply(1:Nodes, function(j) {
        deltaij0 <- 1
        if (i == j) {
          deltaij0 <- 0
        }

        exparg <- rep(0, length(taus))

        if (!Approximate) {
          lapply(1:Order, function(l) {
            #Cijl <- as.numeric(Pj0 %*% Nu[[l]] %*% Ei)
            #Cijl <- as.numeric(Nu[[l]][j,] %*% Ei)
            #Cijl <- sum(Nu[[l]][j,which(Ei>0)])
            Cijl <- sum(Nu[[l]][j, Ei])

            if (l %in% zero.idxs) {
              exparg <<- exparg + Cijl * taus
            } else {
              exparg <<- exparg + Cijl * (1 - exp(-LM[l] * taus)) / LM[l]
            }
          })
        } else {
          lapply(c(zero.idxs, lambda2.idx), function(l) {
            #Cijl <- as.numeric(Pj0 %*% Nu[[l]] %*% Ei)
            #Cijl <- as.numeric(Nu[[l]][j,] %*% Ei)
            #Cijl <- sum(Nu[[l]][j,which(Ei>0)])
            Cijl <- sum(Nu[[l]][j, Ei])

            if (l %in% zero.idxs) {
              exparg <<- exparg + Cijl * taus
            } else {
              exparg <<- exparg + Cijl * (1 - exp(-LM[l] * taus)) / LM[l]
            }
          })

        }

        rho <<- rho + exp(-exparg) * deltaij0
        #}
      })
      utils::setTxtProgressBar(pb, i)
      #}
    })
    close(pb)

    cat("\n")

    rho <- 1 - rho / (Nodes ^ 2)
    print(rho)
    return(rho)
  }

  Coverage <- compiler::cmpfun(Coverage.raw)

  cat(paste("  :: Calculating coverage...", "\n"))

  rho <-
    Coverage(Nu,
             LM,
             Layers,
             Nodes,
             TimeSequence,
             Approximate,
             zero.idxs,
             lambda2.idx)
  rho.df <- data.frame(tau = TimeSequence, rho = rho)

  return(rho.df)
}

#' Get the Coverage Evolution of an edge-colored network
#'
#' This function calls \link{GetCoverageEvolutionMultilayer} with \code{Layers = 1}.
#'
#' @param TransitionMatrix the non-interconnected multiplex transition matrix of
#'   an edge-colored network, as returned from
#'   \link{BuildTransitionMatrixFromEdgeColoredMatrices}
#' @param Layers Layers
#' @param Nodes Layers
#' @param TimeSequence Time sequence
#' @param Approximate logical, use approximate method. Default FALSE
#' @param Approximate.disconnected Numeric for approximate method. Default 0
#' @return data frame coverage \code{rho} across time \code{tau}
#' @export
GetCoverageEvolutionEdgeColored <-
  function(TransitionMatrix,
           Layers,
           Nodes,
           TimeSequence,
           Approximate = FALSE,
           Approximate.disconnected = 0) {
    #   References:
    #   Todo

    # library(compiler)
    Layers <- 1 # there is only "one layer" for this problem

    return(
      GetCoverageEvolutionMultilayer(
        TransitionMatrix,
        Layers,
        Nodes,
        TimeSequence,
        Approximate,
        Approximate.disconnected
      )
    )
  }

#' Get the Coverage Evolution of a single-layer network
#'
#' This function calls \link{GetCoverageEvolutionMultilayer} with \code{Layers = 1}.
#'
#' @param TransitionMatrix the transition matrix
#' @param Nodes Layers
#' @param TimeSequence Time sequence
#' @param Approximate logical, use approximate method. Default FALSE
#' @param Approximate.disconnected Numeric for approximate method. Default 0
#' @return data frame coverage \code{rho} across time \code{tau}
#' @export
GetCoverageEvolutionSingleLayer <-
  function(TransitionMatrix,
           Nodes,
           TimeSequence,
           Approximate = FALSE,
           Approximate.disconnected = 0) {
    #   References:
    #   Todo

    # library(compiler)
    Layers <- 1 #there is only "one layer" for this problem

    return(
      GetCoverageEvolutionMultilayer(
        TransitionMatrix,
        Layers,
        Nodes,
        TimeSequence,
        Approximate,
        Approximate.disconnected
      )
    )
  }
