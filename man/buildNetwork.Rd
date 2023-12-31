% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/buildNetwork.R
\name{buildNetwork}
\alias{buildNetwork}
\title{Build network from data.table}
\usage{
buildNetwork(ff, kw = NA, LCC = TRUE, simplify = TRUE)
}
\arguments{
\item{ff}{network structure data.frame with first two columns defining the
network edge nodes}

\item{kw}{pmid keyword annotation data.frame. If \code{NA}
no annotation will be added}

\item{LCC}{if TRUE only largest connected component is returned}

\item{simplify}{if TRUE loops and multiple edges will be removed}
}
\value{
igraph object of the largest connected component
}
\description{
Wrapper for \code{\link[igraph]{graph_from_data_frame}} function which will
always return the largest connect component for a given network \code{ff}.
The function will also annotated the edges in \code{ff} with PubMed data
from \code{kw} if provided.
}
\examples{
f<-data.frame(A=c('A', 'A', 'B', 'D'), B=c('B', 'C', 'C', 'E'))
gg<-buildNetwork(f)
V(gg)$name
}
