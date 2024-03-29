% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/annotateNetwork.R
\name{getAnnotationList}
\alias{getAnnotationList}
\title{Extract unique values from annotations.}
\usage{
getAnnotationList(
  annVec,
  col = COLLAPSE,
  sort = c("none", "string", "frequency")
)
}
\arguments{
\item{annVec}{vector of annotation strings}

\item{col}{list separator character}

\item{sort}{how to sort the result list}
}
\value{
vector of unique annotation terms
}
\description{
It is not uncommon to find both duplicated vertex annotation terms, and
vertices annotated with multiple terms, in a given annotation list. This
function creates a vector of unique annotation terms for each vertex given
an input annotation list.
}
\examples{
file <- system.file("extdata", "PPI_Presynaptic.gml", package = "BioNAR")
gg <- igraph::read_graph(file, format="gml")
annVec<-V(gg)$TopOntoOVG
al<-getAnnotationList(annVec)
al
}
\seealso{
getAnnotationVertexList
}
