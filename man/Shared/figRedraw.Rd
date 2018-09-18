\name{figRedraw}
\title{Redrawing/refreshing a Figaro plot}
\alias{figRedraw}
\alias{refreshFig}

\description{
This function redraws/refreshes a Figaro-compatible plot.
}

\usage{
figRedraw(x=x.data, y=y.data, zoom=NULL, bw=FALSE, title=NULL)

refreshFig()
}

\arguments{
  \item{x}{vector of x coordinates}
  \item{y}{vector of y coordinates}
  \item{zoom}{logical; redraw while zooming?}
  \item{bw}{logical; should be the output black and white?}
  \item{title}{character; optional title for the plotting window.}
}

\section{Warning}{
Note that all user-defined components added via '\code{Plot editing: Add}' 
(legend, lines, text, boxes, \ldots) - will be lost.
}

\keyword{device}
\concept{Plot}
\concept{Figaro}
\concept{Menu: Plot editing}

\seealso{
    \link{figaro}  
}

\author{
Colin M. Farrow, \email{colinfarrow537@gmail.com} 

and Vojtech Janousek, \email{vojtech.janousek@geology.cz}
}
