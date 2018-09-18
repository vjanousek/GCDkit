\name{Pearce and Norry}
\title{Pearce and Norry (1979)}
\alias{Norry}

\description{
    Assigns data for the diagram of \emph{Pearce & Norry (1979)} 
    into Figaro template (list '\code{sheet}') and appropriate values 
    into '\code{x.data}' and '\code{y.data}'.
}

\usage{
Norry()
}

\details{
    Diagram proposed by \emph{Pearce & Norry (1979)} for geotectonic discrimination
    between basaltic rocks from distinct geotectonic positions:

    \emph{Within-plate Basalts}\cr
    \emph{Island-arc basalts}\cr
    \emph{Mid-ocean Ridge Basalts}

    \if{html}{\figure{Norry.png}} 
    \if{latex}{\figure{Norry.png}{options: width=4.5in}}
}

\value{
  \item{sheet}{list with Figaro Style Sheet data}
  \item{x.data}{Zr ppm}
  \item{y.data}{Zr/Y by weight}
}

\references{
Pearce J A & Norry M J (1979) Petrogenetic implications of Ti, Zr, Y, and Nb 
variations in volcanic rocks. Contrib Mineral Petrol 69: 33-47. 
\href{http://dx.doi.org/10.1007/BF00375192}{doi: 10.1007/BF00375192}
}

\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}

\examples{
#plot the diagram
plotDiagram("Norry",FALSE)
}

\seealso{
\link{figaro}
\link{plotDiagram}
}

\keyword{hplot}
\concept{Geotectonic}
\concept{Plot}
\concept{Menu: Geotectonic}
