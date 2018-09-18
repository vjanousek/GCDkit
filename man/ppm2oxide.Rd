\name{ppm2oxide}
\alias{ppm2oxide}
\title{Calculation of wt\% of the given oxide from ppm of atom}
\description{
    Recasts concentrations of a cation (in ppm) to those of the selected oxide (in wt \%). 
}

\usage{
ppm2oxide(formula,where="WR")
}

\arguments{
  \item{formula}{character: the oxide which is to be recalculated}
  \item{where}{character: a name of matrix or dataframe with Te data to be recalculated}
}

\value{
    A numeric matrix with one column containing the recalculated concentrations of 
    the given oxide (in wt \%) for individual samples.
}

\seealso{
    \code{\link{oxide2ppm}}, \code{\link{oxide2oxide}}, \code{\link{molecularWeight}} 
}

\examples{
data(sazava)
accessVar("sazava")
ppm2oxide("K2O")
     
oxide2ppm("FeOt")
oxide2ppm("FeO")+oxide2ppm("Fe2O3")
}
\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}

\keyword{manip}
