\name{addOn}
\alias{addOn}
\title{Replacing missing data by values}

\description{
Add missing columns ("what") to a matrix "where", fill with "value".  
}

\usage{
addOn(what,value=NA,where=WR)
}

\arguments{
  \item{what}{character or numeric, specifies the column in the matrix \code{where}}
  \item{value}{numeric vector with the replacement}
  \item{where}{a matrix in which the replacement is to be done}
}

\details{
The function serves two purposes. If a column with the name '\code{what}' does not
exist in the numeric data matrix \code{where}, it appends a new column and fills 
it with '\code{value}'. 

On the other hand, if the column '\code{what}' does exist, the function attempts 
to replace only the empty (NA) values by the specified values '(\code{value})'.
}

\value{
Modifies the matrix \code{WR}.
}

\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}

\keyword{internal}
