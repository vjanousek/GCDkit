\name{setCex}
\title{Set uniform symbols size}
\alias{setCex}

\description{
Defines the default relative size of plotting symbols.
}

\usage{
setCex(x)
}

\arguments{
\item{x}{numeric; scaling for the plotting symbols}.
}
    
\details{
The coefficient determining the plotting symbols expansion is stored in a 
variable '\code{labels[,"Size"]}', the default is 1.
}

\seealso{
    \code{\link{gcdOptions}}
}

\examples{
setCex(2) # double size
plotDiagram("TAS",FALSE)

setCex(0.5) # half the size
plotDiagram("TAS",FALSE)
}

\keyword{dplot}
\concept{Plot}
\concept{Parameter}
\concept{Menu: GCDkit}

\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}
