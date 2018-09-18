\name{PearceEtAl}
\title{Pearce et al. (1977) MgO-FeOt-Al2O3}
\alias{PearceEtAl}

\description{
    Assigns data for the MgO-FeOt-\eqn{Al_{2}O_{3}}{Al2O3} triangle proposed by 
    \emph{Pearce et al.(1977)} into Figaro template (list '\code{sheet}') and 
    appropriate values into '\code{x.data}' and '\code{y.data}'. 
    
    \if{html}{\figure{PearceEtAl.png}} 
    \if{latex}{\figure{PearceEtAl.png}{options: width=4.5in}}
}

\usage{
PearceEtAl()
}


\details{
Triangular diagram with apices MgO, FeOt and \eqn{Al_{2}O_{3}}{Al2O3}, proposed by 
\emph{Pearce et al.(1977)}.
The boundaries were defined solely for subalkaline volcanic rocks with \eqn{SiO_{2}}{SiO2} 
between 51-56 wt \%. Following geotectonic positions may be identified using the 
diagram:

\emph{Spreading Center Island} (or inter-plate island) - oceanic islands adjacent to ocean-ridge spreading,
 such as Iceland or Galapagos; the authors 'do not consider this field well established'.\cr
\emph{Orogenic}\cr
\emph{Ocean Ridge and Floor}\cr
\emph{Ocean Island}\cr
\emph{Continental}
}

\value{
  \item{sheet}{list with Figaro Style Sheet data}
  \item{x.data, y.data}{MgO, FeOt and \eqn{Al_{2}O_{3}}{Al2O3} in wt. \% recalculated to two dimensions}
}

\references{
Pearce T H, Gorman B E & Birkett T C (1977) The relationship between major 
element geochemistry and tectonic environment of basic and intermediate volcanic 
rocks. Earth Planet Sci Lett 36: 121-132. 
\href{http://dx.doi.org/10.1016/0012-821X(77)90193-5}{doi: 10.1016/0012-821X(77)90193-5}}

\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}

\examples{
# Plot the diagram
plotDiagram("PearceEtAl",FALSE)
}

\seealso{
\link{figaro}
\link{plotDiagram}
}


\keyword{hplot}
\concept{Geotectonic}
\concept{Plot}
\concept{Menu: Geotectonic}
