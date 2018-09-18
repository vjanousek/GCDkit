\name{Batchelor}
\title{Batchelor and Bowden (1985)}
\alias{Batchelor}

\description{
Plots data stored in '\code{WR}' (or its subset) into Batchelor and Bowden's 
\eqn{R_{1}-R_{2}}{R1-R2} diagram.
}

\usage{
Batchelor(ideal=TRUE)
}

\arguments{
\item{ideal}{logical, plot ideal minerals composition?}
}

\details{
Diagram in \eqn{R_{1}-R_{2}}{R1-R2} space, proposed by \emph{De la Roche et al. (1980)}, 
with fields defined by \emph{Batchelor & Bowden (1985)} as characteristic for following 
geotectonic environments:

\emph{Mantle Fractionates}

\emph{Pre-plate Collision}

\emph{Post-collision Uplift}

\emph{Late-orogenic}

\emph{Anorogenic}

\emph{Syn-collision}

\emph{Post-orogenic}

\if{html}{\figure{Batchelor.png}} 
\if{latex}{\figure{Batchelor.png}}
}

\value{
  \item{sheet}{list with Figaro Style Sheet data}
  \item{x.data}{R1 = 4 * Si - 11 * (Na + K) - 2 * (Fe[total as bivalent] + Ti), 
  all in millications; as calculated by the function '\code{\link{LaRoche}}'}
  \item{y.data}{R2 = 6 * Ca + 2 * Mg + Al, all in millications; as calculated 
  by the function '\code{\link{LaRoche}}'}
}
\references{
Batchelor R A & Bowden P (1985) Petrogenetic interpretation of granitoid rock 
series using multicationic parameters. Chem Geol 48: 43-55. 
\href{http://dx.doi.org/10.1016/0009-2541(85)90034-8}{doi: 10.1016/0009-2541(85)90034-8}

De La Roche H, Leterrier J, Grandclaude P, & Marchal M (1980) A classification 
of volcanic and plutonic rocks using \eqn{R_{1}R_{2}}{R1R2}- diagram and major element 
analyses - its relationships with current nomenclature. Chem Geol 29: 183-210. 
\href{http://dx.doi.org/10.1016/0009-2541(80)90020-0}{doi: 10.1016/0009-2541(80)90020-0}

}

\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}

\examples{
#plot the diagram
plotDiagram("Batchelor", FALSE)
}


\seealso{
\link{LaRoche}
\link{figaro}
\link{plotDiagram}
}

\keyword{hplot}
\concept{Menu: Geotectonic}
\concept{Geotectonic}
\concept{Plot}
