\name{Pearce and Cann}
\title{Pearce and Cann (1973)}
\alias{Cann}

\description{
Plots data stored in '\code{WR}' (or its subset) into Pearce and Cann's diagrams.
}

\usage{
Cann(plot.txt = getOption("gcd.plot.text"))
}

\arguments{
\item{plot.txt}{logical, annotate fields by their names?}
}

\details{
Set of two triangular and one binary diagram, proposed by \emph{Pearce & Cann (1973)}.

    \if{html}{\figure{Cann.png}} 
    \if{latex}{\figure{Cann.png}{options: width=5.5in}} 
    
Following abbreviations are used:

\tabular{ll}{
IAT \tab \emph{Low-K Tholeiites} \cr
MORB \tab \emph{Ocean Floor Basalts} \cr
CAB \tab \emph{Island Arc Basalts} \cr
WPB \tab \emph{Within Plate Basalts} \cr
}


}

\section{Note}{
    This function uses the plates concept. The individual plots can be selected and their 
    properties/appearance changed as if they were stand alone Figaro-compatible plots. 
    See \code{\link{Plate}}, \code{\link{Plate editing}} and 
    \code{\link{figaro}} for details.
}

\references{
Pearce J A & Cann J R (1973) Tectonic setting of basic volcanic rocks determined 
using trace element analyses. Earth Planet Sci Lett 19: 290-300. 
\href{http://dx.doi.org/10.1016/0012-821X(73)90129-5}{doi: 10.1016/0012-821X(73)90129-5}
}

\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}

\seealso{
     \link{Plate}, \link{Plate editing},  \link{plotPlate},   \link{figaro}
}

\examples{
#plot the diagrams
plotPlate("Cann")
}

\keyword{hplot}
\concept{Geotectonic}
\concept{Plot}
\concept{Menu: Geotectonic}
