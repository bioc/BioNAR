% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/annotateNetwork.R
\name{annotateGeneNames}
\alias{annotateGeneNames}
\title{Annotate Human Gene Names}
\usage{
annotateGeneNames(gg, orgDB = org.Hs.eg.db, keytype = "ENTREZID")
}
\arguments{
\item{gg}{igraph object to annotate}

\item{orgDB}{ordDB object, by default human is assumed from
\code{\link[org.Hs.eg.db]{org.Hs.eg.db}}}

\item{keytype}{type of IDs stored in the \code{name} vertex attribute,
by default \code{ENTREZID} is assumed.}
}
\value{
igraph object with new vertex attribute \code{GeneName}
}
\description{
For the protein-protein interaction (PPI) or disease gene interaction (DGN)
graphs that have EntrezID as a vertex \code{name} this function extract
standard name from \code{\link[org.Hs.eg.db]{org.Hs.eg.db}} and annotate
vertices.
}
\details{
If vertex \code{name} attrubite stores not EntrezID or network is build
not from human genes, other \code{\link[AnnotationDbi]{OrgDb-class}}
object could be provided in \code{orgDB} and one of
\code{\link[AnnotationDbi]{keytypes}} from that object
that correspond to the nature of the vertex \code{name} attrubite could
be provided in the \code{keytype} attribute.

If for some vertices \code{name} attrubite does not match
\code{\link[AnnotationDbi]{keys}} with
particular \code{\link[AnnotationDbi]{keytypes}} in the
\code{orgDB} object, empty string is added as GeneName.
}
\examples{
file <- system.file("extdata", "PPI_Presynaptic.gml", package = "BioNAR")
gg <- igraph::read_graph(file, format="gml")
agg<-annotateGeneNames(gg)
# due to error in org.Hs.eg.db we have to manually check annotation of one node
idx <- which(V(agg)$name == '80273')
paste(V(agg)$GeneName[idx], 'GRPEL1')
}
