\name{figEdit}
\title{Plot editing: Changing titles and axis labels}
\alias{Edit: title}
\alias{Edit: subtitle}
\alias{Edit: x label}
\alias{Edit: y label}

\alias{figEdit}
\alias{figMain}
\alias{figSub}
\alias{figXlab}
\alias{figYlab}
\alias{figAlab}
\alias{figBlab}
\alias{figClab}

\description{
These functions enable altering titles and axis labels of binary (\code{figXlab, figYlab}) 
and ternary (\code{figAlab, figBlab, figClab}), Figaro-compatible plots. 
}

\usage{
figMain(txt=NULL)

figSub(txt=NULL)

figXlab(txt=NULL)

figYlab(txt=NULL)

figAlab(txt=NULL)

figBlab(txt=NULL)

figClab(txt=NULL)
}

\arguments{
\item{txt}{text}
}

\details{
If specified, the parameter \code{txt} will be passed to the function \code{'annotate'} 
to guess the correct reformatting to sub- and superscripts for production of 
"publication quality" plots.
Otherwise, the current value (titles or labels for axes/apices) are edited.  
}

\examples{
    plotDiagram("TAS",FALSE) # example of a classification plot
    figMain(txt="My TAS diagram")
    figSub(txt="test")
    figXlab(txt="Silica")
    figYlab(txt="Total alkalis")
}

\keyword{aplot}
\keyword{iplot}
\concept{Plot}
\concept{Figaro}
\concept{Menu: Plot editing}

\seealso{
  '\code{\link{annotate}}'

  '\code{\link{figaro}}'
}

\author{
Colin M. Farrow, \email{c.farrow@compserv.gla.ac.uk} 

and Vojtech Janousek, \email{vojtech.janousek@geology.cz}
}
