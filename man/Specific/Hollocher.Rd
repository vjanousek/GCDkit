\encoding{latin1} 
\name{Hollocher}
\title{Hollocher et al. (2012) La/Yb vs. Nb/La or Th/Nb}
\alias{Hollocher1}
\alias{Hollocher2}
\alias{Hollocher}

\description{
    Assigns data for La/Yb vs. Nb/La or La/Yb vs. Th/Nb binary diagrams
    into Figaro template (list '\code{sheet}') and appropriate values
    into '\code{x.data}' and '\code{y.data}'.
}

\usage{
Hollocher1()
Hollocher2()
}

\arguments{
None.
}

\details{
    The two binary plots, La/Yb vs. Nb/La and La/Yb vs. Th/Nb, of 
    \emph{Hollocher et al. (2012)} serve for geotectonic discrimination of  
     basalts or basaltic amphibolite units. These diagrams can distinguish 
     between the MORB, enriched ocean island basalts, and the near continuum 
     defined by oceanic, continental, and alkaline arcs. 
     
     However, the authors have noted that basalts from back-arc basins have a wide range 
     of compositions caused by basalt source region variation between depleted 
     N-MORB, ocean-sland (hot spot), and subduction component-enriched 
     (sub-volcanic arc) mantle end members. See also their Fig. 16.
    
    \if{html}{\figure{Hollocher1.png}}
    \if{latex}{\figure{Hollocher1.png}{options: width=4.5in}}
    
    \if{html}{\figure{Hollocher2.png}}
    \if{latex}{\figure{Hollocher2.png}{options: width=4.5in}}
}

\value{
  \item{sheet}{list with Figaro Style Sheet data}
  \item{x.data}{x coordinates}
  \item{y.data}{y coordinates}
}

\references{
    Hollocher K, Robinson P, Walsh E, Roberts D (2012) Geochemistry of amphibolite-facies volcanics and gabbros 
    of the Storen Nappe in extensions west and southwest of Trondheim, western gneiss region, Norway: 
    a key to correlations and paleotectonic settings. Amer J Sci 312: 357-416
    \href{http://dx.doi.org/10.2475/04.2012.01}{doi: 10.2475/04.2012.01} 

    Coordinates and graph layout are taken from website of 
    \href{http://minerva.union.edu/hollochk/c_petrology/discrim.html}{Kurt Hollocher}.}

\author{
    Vojtech Janousek, \email{vojtech.janousek@geology.cz}
}

\examples{ 
plotDiagram("Hollocher1",FALSE,TRUE)
plotDiagram("Hollocher2",FALSE,TRUE)
}

\seealso{
    \link{figaro} \link{plotDiagram}
}

\keyword{hplot} \concept{Geotectonic} \concept{Plot} \concept{Menu: Geotectonic}
