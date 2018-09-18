\name{deleteSingle}
\title{Delete label or variable}
\alias{deleteSingle}

\description{
Deletes a single numeric variable or a label.
}

\usage{
deleteSingle()
}

\details{
The variables to be deleted is selected using the function 
'\code{\link{selectColumnLabel}}'. In any case, a confirmation is required 
before a variable is deleted from the system. Note that some variables are
required by the system and cannot be deleted.
}

\value{
Returns the corrected version of the data frame '\code{labels}' or numeric matrix 
'\code{WR}'.
}

\keyword{manip}
\concept{Edit}
\concept{Menu: Data handling}
\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}
