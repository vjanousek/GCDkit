\encoding{latin1} 
\name{Ross}
\title{Ross + Bedard (2009) Zr/Y-Th/Yb}
\alias{Ross}

\description{
    Assigns data for a Zr/Y vs. Th/Yb binary diagram
    into Figaro template (list '\code{sheet}') and appropriate values
    into '\code{x.data}' and '\code{y.data}'.
}

\usage{
Ross()
}

\arguments{
None.
}

\details{
    The binary plot Zr/Y vs. Th/Yb designed by \emph{Ross and Bédard (2009)}
    for classification of ancient subalkaline volcanic rocks into
    tholeiitic or calc-alkaline series. 
    In these cases, the conventional \link{AFM} diagram tends to be of limited 
    use due to the potential mobility of alkalis. 
    
    \if{html}{\figure{Ross.png}}
    \if{latex}{\figure{Ross.png}{options: width=4.5in}}
}

\value{
  \item{sheet}{list with Figaro Style Sheet data}
  \item{x.data}{x coordinates}
  \item{y.data}{y coordinates}
}

\references{
    Ross PS, Bédard LP (2009) Magmatic affinity of modern and ancient subalkaline volcanic rocks determined from 
    trace-element discriminant diagrams. Can J Earth Sci 46: 823-839
    \href{http://dx.doi.org/10.1139/E09-054}{doi: 10.1139/E09-054} 

    Coordinates and graph layout are taken from website of 
    \href{http://minerva.union.edu/hollochk/c_petrology/discrim.html}{Kurt Hollocher}.}

\author{
    Vojtech Janousek, \email{vojtech.janousek@geology.cz}
}

\examples{ 
plotDiagram("Ross",FALSE,TRUE)
}

\seealso{
    \link{figaro} \link{plotDiagram} \link{AFM}
}

\keyword{hplot} \concept{Geotectonic} \concept{Plot} \concept{Menu: Geotectonic}
