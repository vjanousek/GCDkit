\name{filterOut}
\alias{filterOut}
\title{Extracting specified columns of a data frame/matrix}
\description{
This function returns the specified data columns from a matrix/data frame. Removes
samples for which too few data are available.
}

\usage{
filterOut(where, what, n = length(what))
}

\arguments{
  \item{where}{name of the matrix/data frame}
  \item{what}{character vector with names of the columns to be extracted}
  \item{n}{how many empty items are allowed per sample (see details)}
}

\details{
Finds all columns in '\code{where}' whose name appears in '\code{what}'. The 
function eliminates all rows that have at least '\code{n} NAs (by default only 
completely empty rows). 
}

\value{
Matrix with the extracted elements.
}

\examples{
filterOut(WR,c("SiO2","A/CNK","FeOt"))
round(filterOut(WR,REE,n=1),2)
}

\author{
    Vojtech Janousek, \email{vojtech.janousek@geology.cz}
}

\keyword{internal}
