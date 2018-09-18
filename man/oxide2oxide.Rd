\name{oxide2oxide}
\alias{oxide2oxide}
\title{Recalculation of one oxide to a different one}
\description{
Returns a factor needed to multiply concentrations of an element given as an 
oxide (in wt \%) to a different target oxide (of the same element). 
}

\usage{
oxide2oxide(formula1, formula2)
}

\arguments{
    \item{formula1}{character: the oxide which is to be recalculated}
    \item{formula2}{character: the target oxide}
}

\value{
A factor for recalculation.
}

\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}

\keyword{manip}

\seealso{
    \code{\link{oxide2ppm}}, \code{\link{ppm2oxide}}, \code{\link{molecularWeight}}  
}

\examples{
oxide2oxide("FeO","Fe2O3")
oxide2oxide("Mn2O3","MnO")
}
