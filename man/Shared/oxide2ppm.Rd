\name{oxide2ppm}
\alias{oxide2ppm}
\title{Calculation of ppm of atom from wt\% of an oxide}
\description{
    Recasts concentrations of an oxide (in wt. \%) to that of appropriate cation (in ppm). 
}

\usage{
oxide2ppm(formula,where="WR")
}

\arguments{
  \item{formula}{character: the oxide which is to be recalculated}
  \item{where}{character: a name of matrix or dataframe with the data to be recalculated}
}

\value{
    A numeric matrix with one column containing the recalculated concentrations of 
    the given cation (ppm) for individual samples.
}

\seealso{
    \code{\link{ppm2oxide}}, \code{\link{oxide2oxide}}, \code{\link{molecularWeight}} 
}

\examples{
data(sazava)
accessVar("sazava")
oxide2ppm("K2O")
}
\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}

\keyword{manip}
