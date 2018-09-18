\name{plotPlate}
\alias{plotPlate}
\title{Plot Plate of Diagrams}

\description{
  Plots a plate of diagrams, based on the Figaro style sheets. 
}

\usage{
    plotPlate(diagram,where="WR",...)
}

\arguments{
  \item{diagram}{a valid name of the function that uses the plate concept to plot the given diagram. 
    See Details.}
  \item{where}{name of the data matrix/data frame, columns of which are to be used for plotting.}
  \item{...}{optional parameters for the diagram function call.} 
}

\details{
    The argument '\code{diagram}' may acquire one of following values:
    \preformatted{
        'Maniar' 'Frost' 'PearceGranite' 'Schandl' 
        'Verma' 'Agrawal' 'Cann' 'Wood'  
    }
}

\value{
    none
}

\section{Note}{
    This function uses the plates concept. The individual plots can be selected and their 
    properties/appearance changed as if they were stand alone Figaro-compatible plots. 
    See \code{\link{Plate}}, \code{\link{Plate editing}} and 
    \code{\link{figaro}} for details.
}

\seealso{
    \link{Plate},  \link{Plate editing},  \link{figaro}
}

\examples{
    plotPlate("PearceGranite")
}

\keyword{hplot}
\concept{Plot}
\concept{Plates}
\concept{Menu: Plots}

\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}
