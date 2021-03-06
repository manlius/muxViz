% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/muxLib_annotated.R
\name{GetMultilayerCommunities_Infomap}
\alias{GetMultilayerCommunities_Infomap}
\title{Return multilayer communities for interconnected multilayer networks}
\usage{
GetMultilayerCommunities_Infomap(
  SupraAdjacencyMatrix,
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
  outputPrefix = "multimap"
)
}
\arguments{
\item{SupraAdjacencyMatrix}{the supra-adajcency matrix}

\item{Layers}{scalar, number of layers}

\item{Nodes}{scalar, number of nodes}

\item{bin.path}{path, full path where the INFOMAP binary executable is located}

\item{isDirected}{logical, indicates if the analysis is for an undirected or directed network}

\item{seed}{Integer, Infomap seed}

\item{includeSelfLinks}{logical}

\item{numTrials}{Integer, number of Infomap trials to minimize L}

\item{twoLevel}{logical, two level Infomap comunity detection}

\item{preclusterMultiplex}{logical}

\item{addMissingPhysicalNodes}{logical}

\item{hardPartitions}{logical}

\item{verbose}{logical}

\item{addAggregateAnalysis}{logical}

\item{outputPrefix}{String}
}
\value{
Community List
}
\description{
Return multilayer communities for interconnected multilayer networks
}
\references{
Manlio De Domenico, Andrea Lancichinetti, Alex Arenas, and Martin Rosvall, Physical Review X 5, 011027 (2015)
\href{https://doi.org/10.1103/PhysRevX.5.011027}{doi 10.1103/PhysRevX.5.011027}
\href{http://www.mapequation.org/code.html}{Source}
}
