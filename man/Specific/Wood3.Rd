\name{Wood3}
\title{Wood (1980) Th-Zr/117-Nb/16}
\alias{Wood3}

\description{
Plots data stored in '\code{WR}' (or its subset) into Wood's Th-Zr/117-Nb/16 diagram.
}

\usage{
Wood3(idt=ident,plot.txt=plot.text)
}

\arguments{
\item{idt}{logical, identify?}
\item{plot.txt}{logical, annotate fields by their names?}
}

\details{
Triangular diagram with apices Th, Zr/117 and Nb/16, proposed by \emph{Wood (1980)}.
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
  \item{x.data, y.data}{Th, Zr/117 and Nb/16 in ppm recalculated into two dimensions}
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

\examples{
#plot the diagram
Wood3()
}

\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}

\keyword{hplot}
\concept{Geotectonic}
\concept{Plot}
\concept{Menu: Geotectonic}
