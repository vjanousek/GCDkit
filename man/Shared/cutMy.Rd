\name{cutMy}
\title{Groups by numerical variable}
\alias{cutMy}

\description{
Grouping the data according to the interval of a single numerical variable it 
falls into.
}

\usage{
cutMy(where=NULL,int=NULL,int.lab=NULL,na.lab="Unclassified")
}

\arguments{
\item{where}{Numeric data column in '\code{WR}' - the basis of the classification.}
\item{int}{Boundaries of intervals.}
\item{int.lab}{Labels for the intervals}
\item{na.lab}{Labels for samples that cannot be classified}
}

\details{
The numeric data column is selected using the function 
'\code{\link{selectColumnLabel}}'.
 
After this is done, the user is prompted to enter a comma-delimited list or at 
least one break point defining the intervals. The default includes the mean, 
that will be automatically supplemented by minimum and maximum (i.e. two 
intervals). 

Then the names of the individual groups are to be specified; values out of range 
are automatically labeled as 
\code{'Unclassified'}. 
The vector containing the information on the current groups can be appended to 
the data frame '\code{labels}'. 
}

\value{
  \item{groups}{character vector: the grouping information}
  \item{grouping}{If the new column was appended the data frame \code{labels}, 
  sequence number of this column; if not appended, though, this variable is set to -100.}
}

\seealso{
\code{\link{cut}}
}

\keyword{manip}
\concept{Edit}
\concept{Grouping}
\concept{Classification}
\concept{Menu: Data handling}
\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}
