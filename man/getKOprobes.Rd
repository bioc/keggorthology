\name{getKOprobes}
\alias{getKOprobes}
\alias{getKOtags}
\title{obtain probe set IDs associated with a KO term  }
\description{obtain probe set IDs associated with a KO term
}
\usage{
getKOprobes(str, useAcc=TRUE, plat="hgu95av2", na.action=na.omit)
}
\arguments{
  \item{str}{ string giving a KEGG orthology term}
  \item{useAcc}{ logical -- use all accessible terms?}
  \item{plat}{ platform corresponding to a bioconductor
annotation package, e.g., hgu95av2.db}
 \item{na.action}{function for dealing with NA}
}
\details{
Based on keggorthology read of KEGG orthology, March 2 2010.
Specifically, we run wget on
\url{ftp://ftp.genome.jp/pub/kegg/brite/ko/ko00001.keg} and use parsing
and modeling code given in inst/keggHTML to generate a data frame
respecting the hierarchy, and then \code{\link{keggDF2graph}} to
construct the graph.


Looks up the requested term and gives back the unique
probe set ids on the platform.
}
\value{
character vector, typically processed by \code{na.omit}
}
%\references{  }
\author{Vince Carey <stvjc@channing.harvard.edu>}
%\note{  }


%\seealso{  }

\examples{
getKOtags("insulin")
es =  acc(KOgraph, "Endocrine System")
nm = names(es[[1]])
nm
esp = lapply(nm, getKOprobes)
names(esp) = nm
sapply(esp, length)
}
\keyword{ models }
