\name{DebonCalc}
\title{Calculation: Debon and Le Fort}
\alias{DebonCalc}

\description{
Recalculates whole-rock analyses to Debon & Le Fort's values.
}

\usage{
DebonCalc(millicats=milli)
}

\arguments{
\item{millicats}{a numeric matrix with millications to be used for calculation.}
}

\details{

All the parameters are based on millications (1000 gram-atoms per 100 grams).


P = K - (Na + Ca); represents the proportion of K-feldspar to plagioclase
 
Q = Si / 3 - (K + Na + 2 * Ca / 3); corresponds to quartz content

A = Al - (K + Na + 2 Ca); expresses alumina saturation

B = Fe + Mg + Ti; proportional to the amount of dark minerals

F = 555 - (Q + B); represents the proportion of feldspars
    

(Na+K)

K/(Na+K)
 
Mg/(Fe+Mg); serves to distinguish between ferriferous and magnesian associations


Q(quartz)

B(dark m.)
 
F(feldsp)
 
The Q - B - F ternary plot expresses the balance of dark minerals, feldspars and 
quartz.

For details, see \emph{Debon & Le Fort (1983)} or \emph{(1988)}.
}
\value{
  \item{results}{numerical matrix with the above specified results}
}


\references{
Debon F & Le Fort P (1983) A chemical-mineralogical classification of common 
plutonic rocks and associations. Trans Roy Soc Edinb; Earth Sci 73: 135-149

Debon F & Le Fort P (1988) A cationic classification of common plutonic rocks 
and their magmatic associations: principles, method, applications. Bull Mineral 
111: 493-511
}

\seealso{
\code{\link{Debon}}
}

\keyword{manip}
\keyword{internal}
\concept{Norm}
\concept{Calculation}
\concept{Classification}
\concept{Plot}
\concept{Menu: Calculations}

\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}
