\name{PearceGranite}
\title{Pearce et al. (1984)}
\alias{PearceGranite}

\description{
Assigns Figaro templates to Pearce's geotectonic diagrams for granitoids 
into  the list '\code{plate}') and appropriate values into 
the list '\code{plate.data}' for subsequent plotting.
}

\usage{
PearceGranite(plot.txt = getOption("gcd.plot.text"))
}

\arguments{
    \item{plot.txt}{logical, annotate fields by their names?}
}

\details{
Suite of four diagrams for discrimination of geotectonic environment of granitoid 
rocks, proposed by \emph{Pearce et al. (1984)}.
It is based on combination of five trace elements (namely  Y, Nb, Rb, Yb and Ta).

\if{html}{\figure{PearceGranite.png}} 
\if{latex}{\figure{PearceGranite.png}{options: width=5.5in}}

Following geotectonic settings may be deduced:

\tabular{ll}{
Abbreviation used \tab Environment \cr
ORG \tab \emph{Ocean Ridge Granites} \cr
VAG \tab \emph{Volcanic Arc Granites} \cr
WPG \tab \emph{Within Plate Granites} \cr
COLG \tab \emph{Collision Granites} \cr
}

}

\section{Note}{
    This function uses the plates concept. The individual plots can be selected and their 
    properties/appearance changed as if they were stand alone Figaro-compatible plots. 
    See \code{\link{Plate}}, 
    \code{\link{Plate editing}} and 
    \code{\link{figaro}} for details.
}

\references{
Pearce J A, Harris N W & Tindle A G (1984) Trace element 
discrimination diagrams for the tectonic interpretation of granitic rocks. 
J Petrology 25: 956-983. 
\href{http://dx.doi.org/10.1093/petrology/25.4.956}{doi:10.1093/petrology/25.4.956}
}

\examples{
plotPlate("PearceGranite")
}

\seealso{
     \link{Plate}, \link{Plate editing},  \link{plotPlate},   \link{figaro}
}

\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}

\keyword{hplot}
\concept{Geotectonic}
\concept{Plot}
\concept{Menu: Geotectonic}
