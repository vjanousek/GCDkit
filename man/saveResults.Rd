\name{saveResults}
\title{Save results}
\alias{saveResults}

\description{
Saves the most recently calculated results to a text file. 
}

\usage{
saveResults(what = results, sep = "\t", digits = 2)
}

\arguments{
\item{what}{a variable to be saved, can be either a vector, a matrix or a list.}
\item{sep}{separator; default is a tab-delimited file.}
\item{digits}{precision of the results to be saved.}
}

\details{
Saves the variable '\code{results}' returned by most of the calculation algorithms 
to a tab-delimited ASCII file.
}

\value{
None.
}

\keyword{file}
\concept{File}
\concept{Calculation}
\concept{Menu: Calculations}
\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}
