\name{keggDF2graph}
\alias{keggDF2graph}
\alias{KOgraph}
\alias{keggOrthDF}
\title{create a graph from a specific data frame format
for KEGG orthology}
\description{create a graph from a specific data frame format
for KEGG orthology
}
\usage{
keggDF2graph(df, root="KO.June07root")
data(KOgraph)
}
\arguments{
  \item{df}{ the data frame}
  \item{root}{ a name for root node}
}
\details{
the obvious directed graph structure from root
to leaf nodes (pathway names) is instantiated
for the orthology, nodeData attribute tag
is loaded with the numerical tag for the term in KEGG,
and nodeData attribute depth is loaded with depth
from root

Based on keggorthology read of KEGG orthology, March 2 2010.
Specifically, we run wget on
\url{ftp://ftp.genome.jp/pub/kegg/brite/ko/ko00001.keg} and use parsing
and modeling code given in inst/keggHTML to generate a data frame
respecting the hierarchy, and then \code{\link{keggDF2graph}} to
construct the graph.

}
\value{
a \code{\link[graph]{graphNEL-class}} instance
}
%\references{  }
\author{Vince Carey <stvjc@channing.harvard.edu>}
\note{This is only a support function.  The graph is
serialized in the package data directory.  }


%\seealso{  }

\examples{
data(keggOrthDF)
keggOrthDF[1:5,]
data(KOgraph)
nodes(KOgraph)[1:4]
nodeData(KOgraph,,"tag")[1:5]
nodeData(KOgraph,,"depth")[1:5]
}
\keyword{ models }
