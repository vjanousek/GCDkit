\name{Villaseca}
\alias{Villaseca}
\title{B-A plot (modified by Villaseca et al. 1998)}

\description{
The B-A diagram as proposed by \emph{Debon and Le Fort (1983)} 
with classification fields for various types of peraluminous rocks designed by
\emph{Villaseca et al. (1998)}. 
}

\usage{
Villaseca()
}

\details{
Plots modified B-A diagram (designed originally by \emph{Debon and Le Fort 1983})
with fields for various peraluminous rock types after
\emph{Villaseca et al. (1998)}.
Assigns data for the B-A diagram into Figaro template 
(list '\code{sheet}') and appropriate values into '\code{x.data}' 
and '\code{y.data}'.  

    \if{html}{\figure{Villaseca.png}} 
    \if{latex}{\figure{Villaseca.png}{options: width=4.5in}} 
    
The following fields are defined:
\tabular{ll}{
l-P\tab low peraluminous \cr
m-P\tab moderately peraluminous \cr
h-P\tab highly peraluminous \cr
f-P\tab felsic peraluminous \cr
metaluminous\tab \cr
}

Rocks with composition falling beyond defined boundaries are labeled 
\emph{'undefined'} by the 'classify' function.

Parameters for the diagram are calculated by the function '\code{DebonCalc}'.
All of them are based on millications (1000 gram-atoms per 100 grams).

A = Al - (K + Na + 2 Ca)

B = Fe + Mg + Ti

For details, see \emph{Debon & Le Fort (1983)} or \emph{(1988)}.
}



\value{
  \item{sheet}{list with Figaro Style Sheet data}
  \item{x.data}{B value. See details.}
  \item{y.data}{A value. See details.}
}

\references{
  Debon F & Le Fort P (1983) A chemical-mineralogical classification of common
  plutonic rocks and associations. Trans Roy Soc Edinb; Earth Sci 73: 135-149

  Debon F & Le Fort P (1988) A cationic classification of common plutonic rocks
  and their magmatic associations: principles, method, applications. Bull.
  Mineral 111: 493-511
  
  Villaseca C, Barbero L, Herreros V (1998) A re-examination of the 
  typology of peraluminous granite types in intracontinental orogenic belts. 
  Trans Roy Soc Edinb, Earth Sci 89: 113-119
}

\author{
 Vojtech Janousek, \email{vojtech.janousek@geology.cz}
}

\seealso{
  \link{classify}
  \link{figaro}
  \link{plotDiagram}
  \link{DebonCalc}
  \link{Debon}
}

\examples{
#plot the diagram
plotDiagram("Villaseca",FALSE)
}

\keyword{hplot}
\concept{Plot}
\concept{Grouping}
\concept{Edit}
\concept{Menu: Data handling}
\concept{Menu: Classification}
