\name{indRender}
\alias{indRender}
\title{indented textual rendering of nodes of a hierarchical graph  }
\description{
indented textual rendering of nodes of a hierarchical graph  
}
\usage{
indRender(klike, from=nodes(klike)[1], indent="  ")
}
\arguments{
  \item{klike}{ a graph, with tree structure similar to \code{\link{KOgraph}}}
  \item{from}{ a node name from which the rendering should proceed to all leaves}
  \item{indent}{ token to use for indentation -- will be replicated to depth of
node to be rendered to its left}
}
\details{
Based on keggorthology read of KEGG orthology, March 2 2010.
Specifically, we run wget on
\url{ftp://ftp.genome.jp/pub/kegg/brite/ko/ko00001.keg} and use parsing
and modeling code given in inst/keggHTML to generate a data frame
respecting the hierarchy, and then \code{\link{keggDF2graph}} to
construct the graph.
}
\value{
NULL
}
%\references{  }
\author{Vince Carey <stvjc@channing.harvard.edu>}
%\note{  }


%\seealso{  }

\examples{
data(KOgraph)
indRender(KOgraph, "Human Diseases")
}
\keyword{ models }
