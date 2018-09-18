\name{Pearce1982}
\title{Pearce (1982)}
\alias{Pearce1982}

\description{
    Assigns data for the diagram of \emph{Pearce (1982)} 
    into Figaro template (list '\code{sheet}') and appropriate values 
    into '\code{x.data}' and '\code{y.data}'.
    
    \if{html}{\figure{Pearce1982.png}} 
    \if{latex}{\figure{Pearce1982.png}{options: width=4in}} 
}

\usage{
Pearce1982()
}

\details{
Diagram proposed by \emph{Pearce (1982)} for geotectonic discrimination
between lavas from distinct geotectonic positions:

\emph{Within-plate lavas}\cr
\emph{Island-arc lavas}\cr
\emph{Mid-ocean Ridge Basalts}
}

\value{
  \item{sheet}{list with Figaro Style Sheet data}
  \item{x.data}{Zr ppm}
  \item{y.data}{Ti ppm}
}

\references{
Pearce, J A (1982) Trace element characteristics of lavas from destructive plate boundaries. 
In: R S Thorpe (ed) Andesites: Orogenic Andesites and Related Rocks.
John Wiley & Sons, Chichester, pp 525-548, ISBN 0 471 28034 8
}

\author{Jean-Francois Moyen, \email{jfmoyen@gmail.com}}

\examples{
#plot the diagram
plotDiagram("Pearce1982",FALSE)
}

\seealso{
\link{figaro}
\link{plotDiagram}
}

\keyword{hplot}
\concept{Geotectonic}
\concept{Plot}
\concept{Menu: Geotectonic}
