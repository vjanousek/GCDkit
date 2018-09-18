\name{recast}
\title{Recast to given sum}
\alias{normalize2total}
\alias{recast}

\description{
Recasts the selected data to a fixed sum.
}

\usage{
recast(total = 100)

normalize2total(what = NULL, total = 100)
}

\arguments{
\item{what}{numeric matrix or character vector with a list of column names to be normalized, separated by commas.}
\item{total}{a sum the data should be normalized to.}
}

\details{
Both functions return the selected elements/oxides (columns in the data matrix 
'\code{WR}') normalized to the required sum. The function '\code{recast}' is front-end 
to '\code{normalize2total}'. If '\code{what}' is a comma delimited list, the 
corresponding columns from the data matrix '\code{WR}' are selected. 
If '\code{what}' is empty, the user is prompted to supply the list of required 
column names via the function '\code{\link{selectColumnsLabels}}'.
}

\value{
\item{results}{numerical vector/matrix with the results}
}

\examples{
normalize2total(major,1)
recast() # to select the sum and elements interactively
}

\keyword{manip}
\concept{Calculation}
\concept{Edit}
\concept{Menu: Calculations}
\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}
