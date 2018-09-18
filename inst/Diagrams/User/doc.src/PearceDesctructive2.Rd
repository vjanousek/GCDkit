\encoding{latin1} 
\name{PearceDestructive2}
\title{Ta/Yb - Th/Yb (Pearce 1982)}
\alias{PearceDestructive2}

\description{
    Assigns data for Ta/Yb vs. Th/Yb 
    binary diagram into Figaro template 
    (list '\code{sheet}') and appropriate values
    into '\code{x.data}' and '\code{y.data}'.
}

\usage{
PearceDestructive2()
}

\arguments{
None.
}

\details{
    The binary plot Ta/Yb vs. Th/Yb of \emph{Pearce (1982)} serves for geotectonic 
    discrimination of volcanic-arc basalts. In addition, it can distinguish 
    between the tholeiitic, calc-alkaline and shoshonitic types. 
    
    \if{html}{\figure{PearceDestructive2.png}}
    \if{latex}{\figure{PearceDestructive2.png}{options: width=4.5in}}
}

\value{
  \item{sheet}{list with Figaro Style Sheet data}
  \item{x.data}{x coordinates}
  \item{y.data}{y coordinates}
}

\references{
    Pearce JA (1982) Trace element characteristics of lavas from destructive plate boundaries. 
    In: Thorpe RS (eds) Andesites; Orogenic Andesites and Related Rocks. 
    John Wiley & Sons, Chichester, pp 525-548

    Coordinates and graph layout are taken from website of 
    \href{http://minerva.union.edu/hollochk/c_petrology/discrim.html}{Kurt Hollocher}.}

\author{
    Vojtech Janousek, \email{vojtech.janousek@geology.cz}
}

\examples{
# plot the diagram
# assuming a dataset is loaded, of course!
\dontrun{
plotDiagram("PearceDestructive2",FALSE,TRUE)
}
}

\seealso{
\link{figaro} \link{plotDiagram}
}

\keyword{hplot} 
\concept{Geotectonic} 
\concept{Plot} 
\concept{Menu: Geotectonic}
