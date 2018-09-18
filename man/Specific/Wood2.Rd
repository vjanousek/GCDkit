\name{Wood2}
\title{Wood (1980) Th-Hf/3-Nb/16}
\alias{Wood2}

\description{
Plots data stored in '\code{WR}' (or its subset) into Wood's Th-Hf/3-Ta diagram.
}

\usage{
Wood2(idt=idt,plot.txt=plot.text)
}

\arguments{
\item{idt}{logical, identify?}
\item{plot.txt}{logical, annotate fields by their names?}
}

\details{
Triangular diagram with apices Th, Hf/3 and Nb/16 was proposed by \emph{Wood (1980)}.
Following fields are defined:

\tabular{ll}{
IAT \tab \emph{Island-arc Tholeiites}\cr
CAB \tab \emph{Calc-alkaline Basalts}\cr
N-MORB \tab \emph{N-type Mid-ocean Ridge Basalts}\cr
E-MORB \tab \emph{E-type Mid-ocean Ridge Basalts}\cr
WPT \tab \emph{Within-plate Tholeiites}\cr
WPA \tab \emph{Alkaline Within-plate Basalts}\cr
}
}

\value{
  \item{sheet}{list with Figaro Style Sheet data}
  \item{x.data, y.data}{Th, Hf/3 and Ta in ppm recalculated into two dimensions}
}

\references{
Pearce J A (1996) A User's Guide to Basalt Discrimination Diagrams.
In Wyman D A (ed) Trace Element Geochemistry of Volcanic Rocks: 
Applications for Massive Sulphide Exploration. Geological Association of Canada, 
Short Course Notes 12, pp 79-113

Wood D A (1980) The application of a Th-Hf-Ta diagram to problems of
tectonomagmatic classification and to establishing the nature of crustal
contamination of basaltic lavas of the British Tertiary volcanic province.
Earth Planet Sci Lett 50: 11-30
}

\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}

\examples{
#plot the diagram
Wood2()
}

\keyword{hplot}
\concept{Geotectonic}
\concept{Plot}
\concept{Menu: Geotectonic}
