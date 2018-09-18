\name{LaRocheCalc}
\alias{LaRocheCalc}
\title{Calculation: De la Roche}

\description{
Recalculates whole-rock data into \eqn{R_{1}-R_{2}}{R1-R2} values of 
\emph{De La Roche et al. (1980)}.
}

\usage{
LaRocheCalc(rock=WR)
}

\arguments{
\item{rock}{a numeric matrix with whole-rock data to be recalculated.}
}

\details{
\eqn{R_{1}-R_{2}}{R1-R2} parameters, as proposed by \emph{De La Roche et al. (1980)}:

R1 = 4 * Si - 11 * (Na + K) - 2 * (Fe[total as bivalent] + Ti), all in millications
R2 = 6 * Ca + 2 * Mg + Al, all in millications
}
            
\value{
  \item{results}{numeric matrix with the two above specified parameters}
}

\references{
De La Roche H, Leterrier J, Grandclaude P, & Marchal M (1980) A classification 
of volcanic and plutonic rocks using \eqn{R_{1}R_{2}}{R1R2}- diagram and major element 
analyses - its relationships with current nomenclature. Chem Geol 29: 183-210
\href{http://dx.doi.org/10.1016/0009-2541(80)90020-0}{doi: 10.1016/0009-2541(80)90020-0} 
}

\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}

\seealso{
\code{\link{LaRoche}}
}

\keyword{manip}
\concept{Norm}
\concept{Calculation}
\concept{Classification}
\concept{Plot}
\concept{Menu: Calculations}
