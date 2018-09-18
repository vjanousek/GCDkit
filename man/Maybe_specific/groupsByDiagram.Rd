\name{groupsByDiagram}
\title{Groups by diagram}
\alias{groupsByDiagram}

\description{
    Grouping the data on a basis of selected classification diagram.
}

\usage{
    groupsByDiagram(fun = NULL, silent = TRUE)
}

\arguments{
    \item{fun}{character; name of the classification function available in the system.}
    \item{silent}{logical; should be echoed the information about classification each of the samples?}
}

\value{
  \item{groups}{character vector: the grouping information}
  \item{grouping}{set to -1.}
}

\seealso{
    \link{classify}  \link{groupsByLabel} \link{groupsByCluster}
    
    \code{\link{AFM}},  
    \code{\link{PeceTaylor}},  
    \code{\link{Shand}}, 
    \code{\link{NaAlK}},
    \code{\link{TAS}}, 
    \code{\link{Cox}}, 
    \code{\link{TASMiddlemost}},   
    \code{\link{Jensen}},  
    \code{\link{Laroche}}, 
    \code{\link{WinFloyd1}},  
    \code{\link{WinFloyd2}},  
    \code{\link{DebonPQ}},  
    \code{\link{DebonBA}},  
    \code{\link{Middlemost}}, 
    \code{\link{QAPF}}, 
    \code{\link{OConnor}}
    \code{\link{Miyashiro}}
    \code{\link{Hastie}}
    \code{\link{Pearce1996}}
    \code{\link{Villaseca}}
}

\keyword{manip}
\keyword{hplot}
\concept{Grouping}
\concept{Edit}
\concept{Menu: Data handling}

\author{
    Vojtech Erban, \email{vojtech.erban@geology.cz}\cr
    Vojtech Janousek, \email{vojtech.janousek@geology.cz}
}

\examples{
    data(sazava)
    accessVar("sazava")

    groupsByDiagram("TASMiddlemostPlut") # Function called "TASMiddlemostPlut"
    groupsByDiagram("^TAS$") # Function called "TAS"
}
