\name{joinGroups}
\title{Merge groups}
\alias{joinGroups}

\description{
Enables merging several groups into a single one.
}

\usage{
joinGroups()
}

\arguments{
None.
}

\details{
This function is the most useful to merge several groups, defined e.g. on the 
basis of a classification plot. A simple spreadsheet is invoked with two columns, 
the first (\code{'Old'}) containing the old levels of groups and the second, 
\code{'New'}, which can be edited. Finally, groups with identical names will be 
merged into a single one. 

Optionally, the vector containing the information on the current groups can be 
appended to the data frame '\code{labels}'.
}

\value{
  \item{groups}{character vector: the grouping information}
  \item{grouping}{Sequential number of the column with grouping information in \code{labels} (if
  appended) or simply set to -100.}
}

\keyword{manip}
\concept{Grouping}
\concept{Edit}
\concept{Menu: Data handling}

\author{
    Vojtech Janousek, \email{vojtech.janousek@geology.cz}
}
