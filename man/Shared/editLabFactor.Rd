\name{editLabFactor}
\title{Edit label as factor}
\alias{editLabFactor}

\description{
Global replacement each of the discrete values (levels) for a selected label.
}

\usage{
editLabFactor()
}

\details{ 
The label to be edited is selected using the function 
'\code{\link{selectColumnLabel}}'.

Then the function invokes a spreadsheet-like interface that enables the user to 
overwrite directly any of the discrete values for the a given label, in the R 
jargon called \emph{levels}. When all the desired changes have been performed, 
close button is to be clicked.
}

\value{
Returns the corrected version of the data frame \code{labels}.
}

\seealso{
'\code{\link{data.entry}}'
}

\keyword{manip}
\concept{Edit}
\concept{Menu: Data handling}
\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}
