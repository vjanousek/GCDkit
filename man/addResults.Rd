\name{addResults}
\title{Appending results to data}
\alias{addResults}

\description{
Appends the most recently calculated results to the data stored in memory.
}

\usage{
addResults(what="results", save=TRUE, overwrite=TRUE, GUI=FALSE)
}

\arguments{
    \item{what}{character; the name of variable to be appended.}
    \item{save}{logical; Append to the data matrix '\code{WR}'?}
    \item{overwrite}{logical; overwrite any matching items in the matrix '\code{WR}'?}
    \item{GUI}{logical; Is the function called within the GUI environment?}
}

\details{
This function appends the variable '\code{results}' (a matrix or vector)
returned by most of the calculation algorithms to a the numeric data stored
in the matrix '\code{WR}'.

In case that any items of the same name are already present in the matrix '\code{WR}', 
the user is asked whether they should be overwritten (GUI). 
In batch mode, they can be ovewritten silently if '\code{overwrite=TRUE}'.  
}

\value{
Modifies the matrix '\code{WR}'.
}

\keyword{manip}
\concept{Calculation}
\concept{Edit}
\concept{Menu: Calculations}

\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}
