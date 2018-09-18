\encoding{latin1} 
\name{Cabanis}
\title{Cabanis + Lecolle (1989) La/10-Y/15-Nb/8}
\alias{Cabanis}

\description{
    Assigns data for a La/10-Y/15-Nb/8 ternary diagram
    into Figaro template (list '\code{sheet}') and appropriate values
    into '\code{x.data}' and '\code{y.data}'.
}

\usage{
Cabanis()
}

\arguments{
None.
}

\details{
    The ternary plot La/10-Y/15-Nb/8 designed by \emph{Cabanis and Lecolle (1989)}
    serves for distinguishing magmas that have originated (1) at orogenic, compressive, 
    destructive plate boundaries (calc-alkaline, closer to the La apex and tholeiitic, closer to the Y apex); 
    (2) in anorogenic, distensive inter-plate domains (including NMORB/EMORB and alkaline rocks); and, in between, 
    (3) in either compressive or distensive, intra-continental, late- to post-
    orogenic zones. See the original paper for details. 
    
    The diagram can also serve for recognition of magmas contaminated by continental crust 
    or resulting from magma mixing.

    \if{html}{\figure{Cabanis.png}}
    \if{latex}{\figure{Cabanis.png}{options: width=4.5in}}
}

\value{
  \item{sheet}{list with Figaro Style Sheet data}
  \item{x.data}{x coordinates}
  \item{y.data}{y coordinates}
}

\references{
    Cabanis B, Lecolle M (1989) Le diagramme La/10-Y/15-Nb/8: un outil pour la discrimination des séries volcaniques et 
    la mise en évidence des processus de mélange et/ou de contamination crustale. 
    CR Acad Sci IIA 309: 2023-2029

    Coordinates and graph layout are taken from website of 
    \href{http://minerva.union.edu/hollochk/c_petrology/discrim.html}{Kurt Hollocher}. 
}

\author{
    Vojtech Janousek, \email{vojtech.janousek@geology.cz}
}

\examples{ 
plotDiagram("Cabanis",FALSE,TRUE)
}

\seealso{
    \link{figaro} \link{plotDiagram}
}

\keyword{hplot} \concept{Geotectonic} \concept{Plot} \concept{Menu: Geotectonic}
