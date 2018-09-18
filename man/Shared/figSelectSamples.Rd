\name{figSelectSamples}
\title{Selecting samples on a Figaro-compatible plot}
\alias{figSelectSamples}

\description{
This function serves to select a subset of samples to be plotted using a Figaro-compatible 
template.
}

\usage{
figSelectSamples(pattern=NULL)
}

\arguments{
  \item{pattern}{search pattern; see below}  
}

\details{
This function serves to replot a Figaro-compatible diagram with only a subset of
the original data. The syntax for the search patterns is the same as for
the function \code{\link{selectSubset}}, used throughout the GCDkit system. 

No permanent changes are being made to the original Figaro template. Hence clicking the 
Ok button (GUI version) or invoking the menu \code{'Plot editing|Redraw/Reset'} 
restores the original plot with all the data selected at the plotting stage.
}

\value{
None.
}

\seealso{
    \code{\link{regex}}, \code{\link{selectSubset}},  \code{\link{selectByLabel}},
    \code{\link{selectAll}},  \code{\link{figaro}} and \code{\link{figRedraw}}
}

\keyword{internal}
\keyword{aplot}
\concept{Plot}
\concept{Figaro}
\concept{Menu: Plot editing}

\examples{
    plotDiagram("LarochePlut",FALSE)
    figSelectSamples("SiO2>60")
    figSelectSamples("1:5")
}

\author{
    Vojtech Janousek, \email{vojtech.janousek@geology.cz}
    and Colin M. Farrow, \email{c.farrow@compserv.gla.ac.uk} 
}


