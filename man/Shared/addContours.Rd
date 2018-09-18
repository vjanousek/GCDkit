\name{Add contours}
\title{Add contours}
\alias{addContours}

\description{
Superposes contour lines to a Figaro-compatible plot.
}

\usage{
addContours(GUI = FALSE, bandwidth = "auto",...)
}

\arguments{
  \item{GUI}{logical; is the function called from GUI (or in a direct mode)?}
  \item{bandwidth}{vector of bandwidths for x and y directions provided to the function \code{kde2d}. See Details.}
  \item{...}{additional parameters passed to the underlying function \code{contour}. Typically plotting parameters.}
}

\details{
    This is, in principle, a front end to the standard R function \code{\link{contour}}. It will work on both the 
    stand-alone Figaro-compatible plot or a plate thereof. 
    
    The  \code{bandwidth} should be a positive number or 'auto', whereby the higher value corresponds to a smoother result.
    The necessary calculations are done by the function \code{\link{kde2d}}. 
}

\value{
    None.
}

\examples{
data(sazava)
accessVar("sazava")
plotDiagram("CoxPlut",FALSE,TRUE)
addContours(col="darkblue",lty="dashed",bandwidth=10)
addContours(col="darkgreen",lty="dotted",bandwidth=5)
    
multiple("SiO2","Al2O3,MgO,CaO,K2O")
plateCex(2)
plateCexLab(1.5)
addContours(col="darkgreen",lty="dashed")   
}

\seealso{
    '\code{\link{filled.contour}}' '\code{\link{kde2d}}'  '\code{\link{par}}' '\code{\link{figaro}}'
}

\keyword{aplot}
\concept{Menu: Plot editing}
\concept{Plot}
\concept{Figaro}

\author{
    Vojtech Erban, \email{vojtech.erban@geology.cz}
    Vojtech Janousek, \email{vojtech.janousek@geology.cz}
}
