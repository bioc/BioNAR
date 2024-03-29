% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/diseasePairs.R
\name{degreeBinnedGDAs}
\alias{degreeBinnedGDAs}
\title{Prepare mapping for degree-aware annotation shuffling.}
\usage{
degreeBinnedGDAs(gg, GDA, dtype)
}
\arguments{
\item{gg}{graph to analyse}

\item{GDA}{vertex annotations returned by \code{\link{prepareGDA}}}

\item{dtype}{list of unique annotation terms to analyze}
}
\value{
mapping matrix between vertices, vertex-degree groups and
annotation terms.
}
\description{
Function to randomly shuffle vertex annotation terms, whilst preserving the
vertex degree originally found with that annotation term.
}
\examples{
options("show.error.messages"=TRUE)
file <- system.file("extdata", "PPI_Presynaptic.gml", package = "BioNAR")
gg <- igraph::read_graph(file, format="gml")
agg<-annotateGeneNames(gg)
gda<-prepareGDA(agg, 'TopOntoOVGHDOID')
m<-degreeBinnedGDAs(agg, gda, getAnnotationList(gda))
c(dim(m), vcount(agg), length(getAnnotationList(gda)))
head(m)
}
\seealso{
prepareGDA

getAnnotationList

sampleDegBinnedGDA
}
