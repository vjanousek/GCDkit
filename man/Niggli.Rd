\name{Niggli}
\title{Niggli's values}
\alias{Niggli}

\description{
Calculates cationic parameters of \emph{Niggli (1948)}.
}

\usage{
Niggli(WR, precision = getOption("gcd.digits"))
}

\arguments{
\item{WR}{a numerical matrix; the whole-rock data to be normalized.}
\item{precision}{precision of the result.}
}
\details{
The calculated parameters are:

\code{si, al, fm, c, alk, k, mg, ti, p, c/fm, qz}

}
\references{
Niggli P (1948) Gesteine und Minerallagerstatten. Birkhauser, Basel, p. 1-540
}

\value{
A numeric matrix '\code{results}'. 
}

\keyword{manip}
\concept{Norm}
\concept{Calculation}
\concept{Menu: Calculations}

\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}
