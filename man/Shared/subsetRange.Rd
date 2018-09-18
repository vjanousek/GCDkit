\name{Subset by range}
\title{Select subset by range}
\alias{subsetRange}

\description{
Selecting subsets of the data stored in memory by their range.
}

\details{
The menu item '\code{Select subset by range}' is connected to the function 
\code{\link{selectSubset}}. The search pattern is treated as a selection of 
sample sequence numbers (effectively a list separated by commas that may also 
contain ranges expressed by colons). The current data will be replaced by its 
newly chosen subset.
}

\value{
Overwrites the data frame '\code{labels}' and numeric matrix '\code{WR}' by 
subset that fulfills the search criteria. 
}

\examples{
\dontrun{
Search pattern = 1:5
# First to fifth samples in the data set

Search pattern = 1,10
# First and tenth samples

Search pattern = 1:5, 10:11, 25  
# Samples number 1, 2, ...5, 10, 11, 25
}
}

\keyword{manip}
\keyword{database}
\concept{Subsetting}
\concept{Edit}
\concept{Menu: Data handling}

\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}
