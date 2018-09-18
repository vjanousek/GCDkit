\name{millications}
\title{Millications}
\alias{millications}

\description{
Returns millications.
}

\usage{
millications(x,print=FALSE,save=FALSE)
}

\arguments{
  \item{x}{matrix or vector with major-element data}  
  \item{print}{logical: print the result?}
  \item{save}{logical: should be the results assigned globally?}
}

\details{
The millications are used for many plots of the French school, e.g. 
\emph{De la Roche et al. (1980)} or \emph{Debon & Le Fort (1983, 1988)}. 
The calculated values are \code{Si, Ti, Al, Fe3, Fe2, Fe, Mn, Mg, Ca, Na, K, P}.
\deqn{Element_{i} = 1000\frac{Oxide_{i}(wt. \%)}{MW(Oxide_{i}))}*{x(Element_{i})}}{% 
    Element[i] = 1000 (Oxide[i] (wt. \%)) / (MW(Oxide[i]) * (x(Element[i]) )}
Where: MW = molecularWeight of the Oxide[i], x = number of atoms of Element[i] in its formula
}

\value{
Numeric matrix (or vector)  with the millications. If '\code{save=TRUE}', 
'\code{results}' and '\code{milli}' are assigned globally.
}

\references{
De La Roche H, Leterrier J, Grandclaude P, & Marchal M (1980) A classification 
of volcanic and plutonic rocks using R1R2- diagram and major element analyses - 
its relationships with current nomenclature. Chem Geol 29: 183-210

Debon F & Le Fort P (1988) A cationic classification of common plutonic rocks and their magmatic associations: 
principles, method, applications. Bull Mineral 111: 493-510

Debon F & Le Fort P (1983) A chemical-mineralogical classification of common plutonic rocks and associations. 
Trans Roy Soc Edinb, Earth Sci 73: 135-149
}

\keyword{manip}
\concept{Calculation}
\concept{Edit}
\concept{Norm}
\concept{Menu: Calculations}

\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}
