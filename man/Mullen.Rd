\name{Mullen}
\title{Mullen (1983) 10MnO-TiO2-10P2O5}
\alias{Mullen}

\description{
    Assigns data for the diagram of \emph{Mullen (1983)} 
    into Figaro template (list '\code{sheet}') and appropriate values 
    into '\code{x.data}' and '\code{y.data}'.  
}

\usage{
Mullen()
}

\details{
    Triangular diagram with apices 10MnO, \eqn{TiO_{2}}{TiO2} and 10\eqn{P_{2}O_{5}}{P2O5}, 
    proposed by \emph{Mullen (1983)}.
    
    \if{html}{\figure{Mullen.png}} 
    \if{latex}{\figure{Mullen.png}{options: width=4.5in}}

    Abbreviations used in diagram represent following geotectonic settings:

    \tabular{ll}{
        CAB \tab \emph{Calc-Alkaline Basalts} \cr
        IAT \tab \emph{Island Arc Tholeiites} \cr
        MORB \tab \emph{Mid-Ocean Ridge Basalts} \cr
        OIA \tab \emph{Ocean Island Andesites} \cr
        OIT \tab \emph{Ocean Island Tholeiites} \cr
    }

}

\value{
  \item{sheet}{list with Figaro Style Sheet data}
  \item{x.data, y.data}{10MnO, \eqn{TiO_{2}}{TiO2} and 10\eqn{P_{2}O_{5}}{P2O5} in wt. \% recalculated to 2D}
}

\references{
Mullen E D (1983) MnO/\eqn{TiO_{2}}{TiO2}/\eqn{P_{2}O_{5}}{P2O5}: a minor element discriminant 
for basaltic rocks of oceanic environments and its implications for petrogenesis. Earth Planet Sci 
Lett 62: 53-62 
\href{http://dx.doi.org/10.1016/0012-821X(83)90070-5}{doi: 10.1016/0012-821X(83)90070-5}
}

\examples{
#plot the diagram
plotDiagram("Mullen",FALSE)
}

\seealso{
\link{figaro}
\link{plotDiagram}
}

\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}

\keyword{hplot}
\concept{Geotectonic}
\concept{Plot}
\concept{Menu: Geotectonic}
