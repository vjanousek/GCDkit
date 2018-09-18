\name{figCol}
\title{Plot editing: Colours}
\alias{Colours: Plotting symbols}
\alias{Colours: title}
\alias{Colours: subtitle}
\alias{Colours: set to B&W}

\alias{figCol}
\alias{figColours}
\alias{figColMain}
\alias{figColSub}
\alias{figBw}


\description{
These functions enable altering colours for titles or all plotting symbols 
in Figaro-compatible plots. 
}

\usage{
figCol(col=NULL)

figColMain(col=NULL)

figColSub(col=NULL)

figBw()
}

\arguments{
\item{col}{colour specification}
}

\details{
The colours can be specified both by their code (see table under \emph{Data handling|Show available colours}) 
or R name (see Examples).

\code{figBw} sets the whole plot (main title and
subtitle, axes and plotting symbols) in black and white, making them ready for 
printing/exporting.
}

\seealso{
  '\code{\link{showColours}}' '\code{\link{colours}}' '\code{\link{figaro}}'
}

\examples{
colours() # prints the list of available colour names

plotDiagram("TAS",FALSE) # example of a classification plot
figSub(txt="My TAS diagram")
figCol(col="green")
figColMain(col="red")
figColSub(col="blue")

figBw()
    
spider(WR,selectNorm("Boynton"),0.1,1000,pch=labels$Symbol,col=labels$Colour)
figMain(txt="My REE plot")
figSub(txt="Normalized by Boynton (1989)")
figCol(col="green")
figColMain(col="red")
figColSub(col="blue")
}

\keyword{aplot}
\keyword{color}
\keyword{iplot}
\concept{Plot}
\concept{Figaro}
\concept{Menu: Plot editing}

\author{
Colin M. Farrow, \email{colinfarrow537@gmail.com}
 
& Vojtech Janousek, \email{vojtech.janousek@geology.cz}
}
