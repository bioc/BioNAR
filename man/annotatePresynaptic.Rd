% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/annotateNetwork.R
\name{annotatePresynaptic}
\alias{annotatePresynaptic}
\title{Add presynaptic functional groups}
\usage{
annotatePresynaptic(gg, anno, idatt = "name")
}
\arguments{
\item{gg}{graph to update}

\item{anno}{annotation matrix in Pair form}

\item{idatt}{optional name of the vertex attributes that contains Entrez IDs}
}
\value{
annotated igraph object
}
\description{
Function takes from \code{anno} matrix manually curated presynaptic genes
functional annotation derived from
Boyken at al. (2013) \url{doi:10.1016/j.neuron.2013.02.027}
and add them to attributes \code{PRESYNAPTIC}.
}
\examples{
file <- system.file("extdata", "PPI_Presynaptic.gml", package = "BioNAR")
gg <- igraph::read_graph(file, format="gml")
sfile<-system.file("extdata", "PresynAn.csv", package = "BioNAR")
pres <- read.csv(sfile,skip=1,header=FALSE,strip.white=TRUE,quote="")
gg <- annotatePresynaptic(gg, pres)
}
\seealso{
getAnnotationVertexList
}
