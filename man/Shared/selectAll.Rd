\name{selectAll}
\title{Select whole dataset}
\alias{selectAll}

\description{
Restores data for all samples as they were loaded from a data file.
}

\usage{
selectAll(GUI=FALSE)
}

\arguments{
\item{GUI}{logical; was the function called from the GUI?}.
}

\details{
When a datafile is loaded into GCDkit using the \code{\link{loadData}} 
function, the data and their backup copy are stored in the memory.
 
The subsets of the current dataset can be chosen using the functions 
\code{\link{selectByLabel}} and \code{\link{selectSubset}} (menus 
\code{'Select subset by sample name}\cr 
\code{or label', 'Select subset by range',
'Select subset by Boolean'}) 
and the current data will be replaced by their  newly chosen subset. 

The backup copy is kept intact ever since the \code{\link{loadData}} function 
has been invoked and can be uploaded any time in place of the current data set 
using the function '\code{selectAll}'. 
Note that all changes made e.g. to plotting symbols, grouping, newly calculated 
variables etc. will be lost.  
}

\value{
None.
}

\keyword{manip}
\concept{Edit}
\concept{Subsetting}
\concept{Menu: Data handling}
\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}
