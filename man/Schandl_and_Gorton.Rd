\name{Schandl}
\title{Schandl and Gorton (2002)}
\alias{Schandl}

\description{
Plots data stored in '\code{WR}' (or its subset) into the classification 
diagrams after \emph{Schandl and Gorton (2002)}. 

}

\usage{
Schandl(plot.txt = getOption("gcd.plot.text"))
}

\arguments{
    \item{plot.txt}{logical, annotate fields by their names?}
}

\details{
    Suite of four diagrams for geotectonic environment discrimination of felsic volcanic 
    rocks (rhyolites), proposed by \emph{Schandl and Gorton (2002)}. It is based on 
    combination of four presumably little immobile trace elements (namely Ta, Yb, 
    Th, and Hf).
    Diagrams were designed to decipher the geotectonic setting of felsic volcanic 
    suites, specifically those associated with the volcanogenic massive sulphide 
    (VMS) deposits.
    a) Ta/Yb versus Th/Yb diagram from 
    \emph{Gorton and Schandl (2000)}  
    is divided into three fields: Oceanic Arcs, Active Continental 
    Margins (ACM) and Within-Plate Volcanic Zones (WPVZ). 
    The Within-Plate Basalts (WPB) and Mid-Ocean Ridge Basalts (MORB) 
    represent compositions previously determined by \emph{Pearce (1982, 1983)}.
    b) Ta vs. Th diagram demonstrates the Th enrichment of felsic volcanic rocks 
    at post-Archaean VMS deposits (and of some unmineralized Archaean 
    rhyolites) with respect to Ta.
    c) Graph of Ta/Hf vs Th/Hf ratios shows the similar incompatibility between Th 
    and Ta in two different tectonic environments: Active Continental Margins 
    and Within-Plate Volcanic Zones.
    d) Yb vs. Th/Ta diagram with fields for associations of Oceanic Arcs, 
    Active Continental Margins, Within Plate Volcanic Zones and MORB. 
    
    \if{html}{\figure{Schandl.png}} 
    \if{latex}{\figure{Schandl.png}{options: width=5.5in}} 

    Taken together, the following geotectonic settings may be deduced:
    \tabular{ll}{
        \bold{Rock Association} \tab \bold{Abbreviation} \cr
        Oceanic Arcs \tab \cr
        Active Continental Margins \tab ACM \cr
        Within-Plate Volcanic Zones \tab WPVZ
    }
    
    Further abbreviations used on the plots:
    \tabular{ll}{
        \bold{Rock Association} \tab \bold{Abbreviation}\cr
        Mid-Oceanic Ridge Basalts \tab MORB \cr
        Within-Plate Basalts \tab WPB
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
Gorton M P & Schandl E S (2000) From continents to island arcs: 
A geochemical index of tectonic setting for arc-related and within-plate 
felsic to intermediate volcanic rocks. Can Min 38: 1065-1073. 
\href{http://dx.doi.org/10.2113/gscanmin.38.5.1065}{doi: 10.2113/gscanmin.38.5.1065}

Pearce J A (1982) Trace element characteristics of lavas from destructive
plate boundaries. In Thorpe R S (ed) Andesites: Orogenic Andesites and
Related Rocks. John Wiley, Chichester, pp 525-548.

Pearce J A (1983) Role of the sub-continental lithosphere in magma genesis at 
active continental margins. In Hawkesworth C J & Norry M J (eds) Continental
Basalts and Mantle Xenoliths. Shiva, Nantwich. pp 230-249

Schandl E S & Gorton M P (2002) Application of high field strength elements 
to discriminate tectonic settings in VMS environments. Economic Geology 97: 629-642. 
\href{http://dx.doi.org/10.2113/97.3.629}{doi: 10.2113/97.3.629}
}

\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}

\seealso{
     \link{Plate}, \link{Plate editing},  \link{plotPlate},   \link{figaro}
}

\examples{
#plot the diagrams
plotPlate("Schandl")
}

\keyword{hplot}
\concept{Geotectonic}
\concept{Plot}
\concept{Menu: Geotectonic}
