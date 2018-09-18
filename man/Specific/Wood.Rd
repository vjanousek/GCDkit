\name{Wood}
\title{Wood (1980)}
\alias{Wood}


\description{
Assigns Figaro templates to Wood's geotectonic diagrams for basaltoids 
into  the list '\code{plate}' and appropriate values into 
the list '\code{plate.data}' for subsequent plotting.
}

\usage{
Wood(ident = getOption("gcd.ident"), 
    plot.txt = getOption("gcd.plot.text"))
}

\arguments{
    \item{ident}{logical, identify?}
    \item{plot.txt}{logical, annotate fields by their names?}
}

\details{
    A series of triangular diagrams with apices Th-Hf/3-Ta, Th-Hf/3-Ta 
    and Th-Zr/117-Nb/16, proposed by \emph{Wood (1980)}.
    
    \if{html}{\figure{Wood.png}} 
    \if{latex}{\figure{Wood.png}{options: width=5.5in}}

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

\section{Note}{
    This function uses the plates concept. The individual plots can be selected and their 
    properties/appearance changed as if they were stand alone Figaro-compatible plots. 
    See \code{\link{Plate}}, \code{\link{Plate editing}} and 
    \code{\link{figaro}} for details.
}

\references{
Pearce J A (1996) A User's Guide to Basalt Discrimination Diagrams.
In Wyman D A (ed) Trace Element Geochemistry of Volcanic Rocks: 
Applications for Massive Sulphide Exploration. Geological Association of Canada, 
Short Course Notes 12, pp 79-113

Wood D A (1980) The application of a Th-Hf-Ta diagram to problems of tectonomagmatic
classification and to establishing the nature of crustal contamination of basaltic
lavas of the British Tertiary volcanic province. Earth Planet Sci Lett 50: 11-30 
\href{http://dx.doi.org/10.1016/0012-821X(80)90116-8}{doi:10.1016/0012-821X(80)90116-8}
}

\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}

\examples{
#plot the diagrams
plotPlate("Wood")
}

\seealso{
     \link{Plate}, \link{Plate editing},  \link{plotPlate},   \link{figaro}
}

\keyword{hplot}
\concept{Geotectonic}
\concept{Plot}
\concept{Menu: Geotectonic}
