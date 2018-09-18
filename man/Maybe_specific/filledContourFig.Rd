\name{filledContourFig}
\title{Filled contours plot}
\alias{filledContourFig}

\description{
Generates a frequency plot on the basis of the most recently plotted Figaro 
template. 
}

\usage{
filledContourFig(xlab=sheet$demo$call$xlab,ylab=sheet$demo$call$ylab,
xlim=sheet$demo$call$xlim,ylim=sheet$demo$call$ylim,
annotate.fields=FALSE,...)
}

\arguments{
  \item{xlab}{character vector; label for the x axis}
  \item{ylab}{character vector; label for the y axis}
  \item{xlim}{limits for the x axis}
  \item{ylim}{limits for the y axis}
  \item{annotate.fields}{logical; should be the plotted fields labeled by their names?}
  \item{...}{additional plotting parameters}
}

\details{
This is a somewhat modified version of the R function '\code{\link{filled.contour}}' 
that produces a frequency plot on the basis of a Figaro template and superimposes, 
if desired, selected data points. 

First the user is prompted, how many intervals should be each of the axes split 
into. This corresponds to a density of grid, in which are the individual points 
classified into. Then a colour scheme (palette) can be chosen. Lastly, after the 
frequency plot is generated, selected analyses can be plotted ('overplot'). In 
the latter case, standard GCDkit routine is used to selectSamples.
}

\value{
None.
}

\seealso{
'\code{\link{addContours}}'  '\code{\link{selectSubset}}' '\code{\link{figaro}}'
}

\keyword{hplot}
\concept{Plot}
\concept{Figaro}
\concept{Statistics}
\concept{Menu: Plot editing}

\author{
    Vojtech Janousek, \email{vojtech.janousek@geology.cz}
}
