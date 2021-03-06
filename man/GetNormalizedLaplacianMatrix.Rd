% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/muxLib_annotated.R
\name{GetNormalizedLaplacianMatrix}
\alias{GetNormalizedLaplacianMatrix}
\title{Network normalized Laplacian}
\usage{
GetNormalizedLaplacianMatrix(AdjacencyMatrix)
}
\arguments{
\item{AdjacencyMatrix}{the adjacency matrix characterising the network}
}
\value{
Normalized Lapalacian Matrix
}
\description{
Given an adjacency matrix \eqn{A}, the function builds the
random walk (RW) normalised Laplacian \eqn{I - D^{-1}A} for a single-layer
network.
The RW normalised Laplacian is defined only for graphs without isolated
nodes -- due to the inversion of the diagonal matrix of degrees
\eqn{D^{-1}}.
Despite this, a (classical) random walk is still defined also in presence
of isolates or nodes without out-going edges, simply setting the
transition probability from those nodes outwards to be zero.
Consequently we can extend the definition of the RW normalised Laplacian
setting \eqn{L_{ij} = 0} if \eqn{k_i = 0} for all \eqn{j}.
}
