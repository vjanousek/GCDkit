\name{Meschede}
\title{Meschede (1986) Zr/4-2Nb-Y}
\alias{Meschede}

\description{
Assigns data for a \emph{Meschede's (1986)} triangular 
diagram into Figaro template (list '\code{sheet}') and appropriate values 
into '\code{x.data}' and '\code{y.data}'.
}

\usage{
Meschede()
}

\details{
    Triangular diagram with apices Zr/4, 2Nb and Y, proposed by 
    \emph{Meschede (1986)}. The plot serves primarily for 
    tectonic discrimination of tholeiitic basalts.

    \if{html}{\figure{Meschede.png}} 
    \if{latex}{\figure{Meschede.png}{options: width=4.5in}}

    Abbreviations used in diagram 
    represent following geotectonic settings:
    \tabular{ll}{
        AI-AII \tab \emph{Within-Plate Alkaline Basalts} \cr
        AII-C \tab \emph{Within-Plate Tholeiites} \cr
        B \tab \emph{P-type Mid-Ocean Ridge Basalts} \cr
        D \tab \emph{N-type Mid-Ocean Ridge Basalts} \cr
        C-D \tab \emph{Volcanic Arc Basalts} \cr
    }
}

\value{
  \item{sheet}{list with Figaro Style Sheet data}
  \item{x.data, y.data}{Zr/4, 2Nb and Y values recalculated into two dimensions}
}

\references{
Meschede M (1986) A method of discriminating between different types of mid-ocean 
ridge basalts and continental tholeiites with the Nb-Zr-Y diagram. Chem Geol 56: 
207-218 \href{http://dx.doi.org/10.1016/0009-2541(86)90004-5}{doi: 10.1016/0009-2541(86)90004-5}
}

\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}

\examples{
#plot the diagram
plotDiagram("Meschede",FALSE)
}

\seealso{
\link{figaro}
\link{plotDiagram}
}

\keyword{hplot}
\concept{Geotectonic}
\concept{Plot}
\concept{Menu: Geotectonic}
