\name{figZoom}
\title{Plot editing: Zooming}
\alias{Zooming: Zoom in}
\alias{Zooming: Zoom out to original size}
\alias{Zooming: Scale x axis}
\alias{Zooming: Scale y axis}
\alias{figZoom}
\alias{figUnzoom}
\alias{figXlim}
\alias{figYlim}
\alias{figFixLim}
\alias{figZooming}

\description{
These functions zoom in and out Figaro-compatible plots.
}

\usage{
figZoom()

figUnzoom()

figXlim(range=NULL)

figYlim(range=NULL)

figFixLim(no.action.warn=TRUE)
}

\arguments{
    \item{range}{numeric: two limits, minimum and maximum, for the given axis.}
    \item{no.action.warn}{logical: should be a warning shown if there is no action needed?}
}

\details{
    '\code{figZoom}' zooms the specified rectangular area (click bottom left and then top right corner) in a new window. 
    The zoomed area is highlighted in the old window.

    '\code{figUnzoom}' closes the new window with blown up portion of the plotting window and returns to the original window.

    '\code{figXlim}' and '\code{figYlim}' allow to change the plotting limits 
    (as a list of two components, separated by commas).

   '\code{figFixLim}' extends the scales of both axes of a binary plot automatically if necessary to accommodate all 
   the data points. 
}

\seealso{
    '\code{\link{figaro}}'
}
\keyword{aplot}
\concept{Plot}
\concept{Figaro}
\concept{Menu: Plot editing}

\section{Warning}{
If requesting a logarithmic plot, make sure that the axis ranges are positive.
}

\author{
Colin M. Farrow, \email{colinfarrow537@gmail.com} 

and Vojtech Janousek, \email{vojtech.janousek@geology.cz}
}

\examples{
\dontrun{        
# requires a preexisting Figaro-compatible plot 
plot.diagram("Shand",select.samples=FALSE)
figXlim(c(0.6,1.2))
figYlim(c(0.8,3))
}
}
