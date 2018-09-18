\name{r2clipboard}
\title{Copy results to clipboard}
\alias{r2clipboard}

\description{
Copies the most recently calculated results to a clipboard. 
}


\usage{
r2clipboard(what=results)
}

\arguments{
\item{what}{a variable to be copied, can be either a vector, a matrix, a list or 
a table.}
}

\details{
Copies the variable '\code{results}' returned by most of the calculation 
algorithms to the Windows clipboard.
}

\value{
None.
}

\keyword{file}
\concept{File}
\concept{Calculation}
\concept{Menu: Calculations}
\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}
