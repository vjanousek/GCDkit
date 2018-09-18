\name{selectByLabel}
\title{Select subset by sample name or label}
\alias{selectByLabel}

\description{
Selecting subsets of the data stored in memory by searching sample names or a 
single label.
}

\usage{
selectByLabel()
}

\details{
This function enables the user to query a single textual column, a label,
chosen using the function '\code{\link{selectColumnLabel}}'. 
The current data will be replaced by its newly chosen subset. 
These enquiries employ \code{\link{regular expressions}}. 
}

\value{
Overwrites the data frame '\code{labels}' and numeric matrix '\code{WR}' by 
subset that fulfills the search criteria. 
}

\examples{
\dontrun{
# Subset by label
The searched field corresponds to localities with the following levels: 
Mull, Rum, Skye, Coll, Colonsay, Hoy, Westray, 
Sanday, Stronsay, Tiree, Islay

Search pattern = ol       
Coll, Colonsay

Search pattern = n.a      
Colonsay, Sanday, Stronsay

Search pattern = ^S       
Skye, Sanday, Stronsay

Search pattern = e$       
Skye, Tiree

Search pattern = [ds]ay   
Colonsay, Sanday, Stronsay

Search pattern = [p-s]ay
Colonsay, Westray, Stronsay

Search pattern = ol|oy    
Coll, Colonsay, Hoy

Search pattern = l\{2\}   
Mull, Coll


# Subset by sample name
The sample names are: Bl-1, Bl-3, Koz-1, Koz-2, 
Koz-5, Koz-11, KozD-1, Ri-1.

Search pattern = oz-[1-3]    
Koz-1, Koz-2, Koz-11

Search pattern = oz-|Bl- 
Bl-1, Bl-2, Bl-3, Koz-1, Koz-2, Koz-5, Koz-11
}
}

\keyword{manip}
\concept{Edit}
\concept{Subsetting}
\concept{Menu: Data handling}

\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}
