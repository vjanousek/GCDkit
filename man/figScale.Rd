\name{figScale}
\title{Plot editing: Scaling text or plotting symbols}

\alias{Scale: symbols}
\alias{Scale: axis labels}
\alias{Scale: title}
\alias{Scale: subtitle}


\alias{figScale}
\alias{figCex}
\alias{figCexLab}
\alias{figCexMain}
\alias{figCexSub}

\description{
These functions enable changing a size of titles, axis labels or plotting symbols 
of Figaro-compatible plots. The size is relative to 1 (the original).
}

\usage{
figCex(x=NULL)

figCexLab(x=NULL)

figCexMain(x=NULL)

figCexSub(x=NULL)
}

\arguments{
\item{x}{numeric: scaling factor.}
}

\seealso{
'\code{\link{figaro}}'    
}

\examples{
plotDiagram("TAS",FALSE) # example of a classification plot
figSub(txt="My TAS diagram")
figCex(2)
figCexMain(1.5)
figCexSub(0.5)
    
spider(WR,selectNorm("Boynton"),0.1,1000,pch=labels$Symbol,col=labels$Colour)
figMain(txt="My REE plot")
figSub(txt="Normalized by Boynton (1989)")
figCex(2)
figCexMain(1.5)
figCexSub(0.5)
}

\keyword{aplot}
\concept{Plot}
\concept{Figaro}
\concept{Menu: Plot editing}

\author{
Colin M. Farrow, \email{colinfarrow537@gmail.com} 

and Vojtech Janousek, \email{vojtech.janousek@geology.cz}
}
