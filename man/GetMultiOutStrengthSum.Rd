% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/muxLib_annotated.R
\name{GetMultiOutStrengthSum}
\alias{GetMultiOutStrengthSum}
\title{Return the Sum of multi-out-Strength, include multiple times the interlinks}
\usage{
GetMultiOutStrengthSum(SupraAdjacencyMatrix, Layers, Nodes, isDirected)
}
\arguments{
\item{SupraAdjacencyMatrix}{the supra-adajcency matrix}

\item{Layers}{scalar, number of layers}

\item{Nodes}{scalar, number of nodes}

\item{isDirected}{logical}
}
\value{
Vector of sum of multi-out-Strength
}
\description{
Return the Sum of multi-out-Strength, include multiple times the interlinks
}
\references{
De Domenico, M., Set al. (2013). Mathematical formulation of
multilayer networks. Physical Review X, 3(4), 041022.
\href{https://doi.org/10.1103/PhysRevX.3.041022}{doi 10.1103/PhysRevX.3.041022}
}
