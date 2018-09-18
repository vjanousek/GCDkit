\name{Shervais}
\title{Shervais (1982)}
\alias{Shervais}

\description{
    Assigns data for the diagram of \emph{Shervais (1982)} 
    into Figaro template (list '\code{sheet}') and appropriate values 
    into '\code{x.data}' and '\code{y.data}'.  
}

\usage{
Shervais()
}

\details{
    Discrimination diagram for basalts, as proposed by \emph{Shervais (1982)} 
    is based on variability of the Ti/V ratio under different oxygen fugacity. 

    \if{html}{\figure{Shervais.png}} 
    \if{latex}{\figure{Shervais.png}{options: width=4.5in}} 

    Following environments may be distinguished:

    \tabular{ll}{
        ARC \tab \emph{Arc Tholeiites}\cr
        OFB \tab \emph{Ocean Floor Basalts}\cr
    }
}

\references{
Shervais J W (1982) Ti-V plots and the petrogenesis of modern and ophiolitic 
lavas. Earth Planet Sci Lett 59: 101-118. 
\href{http://dx.doi.org/10.1016/0012-821X(82)90120-0}{doi: 10.1016/0012-821X(82)90120-0}}

\author{
  Vojtech Erban, \email{vojtech.erban@geology.cz}\cr 
  & Vojtech Janousek, \email{vojtech.janousek@geology.cz}
}

\examples{
#plot the diagram
plotDiagram("Shervais",FALSE)
}

\seealso{
\link{figaro}
\link{plotDiagram}
}

\keyword{hplot}
\concept{Geotectonic}
\concept{Plot}
\concept{Menu: Geotectonic}
