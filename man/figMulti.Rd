\name{figMulti}
\title{Figaro: Multiple plot by groups}
\alias{figMulti}

\description{
    Displays multiple plots, for each of the groups one, based on a most recently 
    plotted Figaro-compatible template. For spiderplots, the colour field denotes the total 
    variation with the whole dataset.
}

\usage{
figMulti(x=x.data,y=y.data,nrow=NULL,ncol=NULL,xlab=sheet$demo$call$xlab,
    ylab=sheet$demo$call$ylab,pch=NULL,col=NULL,
    cex = NULL,plot.symb=NULL,shaded.col="gray",rotate.xlab=TRUE,
    offset=TRUE,centered=FALSE,title=NULL,...)
}

\arguments{
  \item{x, y}{data to be plotted}
  \item{nrow, ncol}{dimensions of the plots' matrix}
  \item{xlab, ylab}{labels for the axes}
  \item{pch}{plotting symbols}
  \item{col}{plotting colours}
  \item{cex}{relative size of the plotting symbols}
  \item{plot.symb}{logical, spiders. Shall be shown also plotting symbols or just lines?}
  \item{shaded.col}{(spiders) Colour for the field portraying the overall variability in the dataset.}
  \item{rotate.xlab}{logical, spiders. Shall be the element names on x axis rotated?}
  \item{offset}{logical, spiders. Shall be the  names for odd and even elements shifted relative to each other?}
  \item{centered}{logical, spiders. Shall be the element names on x axis plotted in between tick marks?}
  \item{title}{optional title for the whole plate. If not provided, it is taken from the title of the Figaro template.}
  \item{...}{any additional graphical parameters}
}

\section{Note}{
    This function uses the plates concept. The individual plots can be selected and their 
    properties/appearance changed as if they were stand alone Figaro-compatible plots. 
    See \code{\link{Plate}}, \code{\link{Plate editing}} and 
    \code{\link{figaro}} for details.
}

\section{Details}{
    The function can handle any Figaro-compatible plots, including binary, ternary or
    spiderplots. For classification plots, it may be advantageous to switch off the field names
    using the function \code{'plateAnnotationsRemove'},  
    (see the figure below as well as the Examples).
    
    \if{html}{\figure{figMulti.png}} 
    \if{latex}{\figure{figMulti.png}{options: width=5.5in}}
} 
    
\seealso{
    \link{figaro},  \link{Plate},  \link{Plate editing}
    \link{binary}, \link{ternary},  \link{spider} 
}

\examples{
# Note that groups should have been defined before running these. 

# switch on the field names (default, valid globally for the whole system)
options("gcd.plot.text"=TRUE)
plotDiagram("TAS",FALSE)
figMulti()

# switch off the field names
options("gcd.plot.text"=FALSE)
plotDiagram("LarochePlut",FALSE)
figMulti(col="black",pch="*",cex=2)
options("gcd.plot.text"=TRUE)

spider(WR,selectNorm("Boynton"),0.1,1000,pch=labels$Symbol,
    col=labels$Colour,cex=labels$cex)
figMulti(plot.symb=TRUE,cex=2)

figMulti(col="red",plot.symb=FALSE,rotate.xlab=FALSE,offset=TRUE)

figMulti(col="red",plot.symb=FALSE,rotate.xlab=FALSE,offset=FALSE,shaded.col="khaki")
}

\author{
    Vojtech Janousek, \email{vojtech.janousek@geology.cz} 
    
    and Colin M. Farrow, \email{colinfarrow537@gmail.com}
}

\keyword{hplot}
\concept{Plot}
\concept{Figaro}
\concept{Menu: Plot editing}
\concept{Grouping}
