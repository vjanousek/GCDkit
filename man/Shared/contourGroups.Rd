\name{contourGroups}
\alias{contourGroups}
\alias{chullGroups}

\title{
    Outline individual groups in a binary plot 
}
\description{
    The functions outline the individual clusters of data (groups by default) 
    on a binary plot. Implemented methods are the convex hull or 
    contours. This can be useful for a quick appreciation of the data distribution,
    e.g. in classification diagrams. 
}

\usage{
contourGroups(clusters=groups,border=NULL,fill=FALSE,precision=50, ...)

chullGroups(clusters=groups,border=NULL,fill=FALSE,...)
}

\arguments{
  \item{clusters}{grouping information for each of the samples.}
  \item{border}{outline colours.}
  \item{fill}{logical; should be the polygons filled by the border colour?}
  \item{precision}{a number indicating how tight the contours should be.}
  \item{\dots}{additional parameters to the functions \link{contour} and \link{polygon}, respectively.}
}

\details{
    If not specified, the colours are selected as the most frequently occurring one defined 
    among samples within each group. 
    
    For the function \emph{contourGroups}, 
    the shape of the contours drawn can be controlled using the parameter 
    \emph{(precision)}. The higher it is, the smoother contours result. 
    
    \if{html}{\figure{contourGroups.png}} 
    \if{latex}{\figure{contourGroups.png}}

}

\value{
    None.
}

\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}

\seealso{
    \link{chull},  \link{contour}, \link{polygon} 
}

\examples{
data<-loadData("sazava.data",sep="\t")
groupsByLabel("Intrusion")
plotDiagram("PeceTaylor",FALSE,FALSE)
chullGroups()
    
chullGroups(fill=TRUE)
    
plotDiagram("PeceTaylor",FALSE,FALSE)    
contourGroups()
}    

\keyword{aplot}
\keyword{iplot}
\concept{Plot}
\concept{Figaro}
\concept{Menu: Plot editing}
