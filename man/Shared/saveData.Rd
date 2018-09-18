\name{saveData}
\title{Save data file}
\alias{saveData}

\description{
Saves modified data set into a specified datafile.
}

\usage{
saveData(sep="\t")
}


\arguments{
\item{sep}{delimiter separating individual items in the data file.}
}

\details{
Labels (stored in data frame '\code{labels}') and  numeric data (in numeric matrix 
'\code{WR}') for the currently selected subset are glued together and saved under 
the specified filename. The format is such that the data can be retrieved again 
into GCDkit using the \code{\link{loadData}} command. Note that no mg numbers 
are currently saved.
}

\value{
None.
}

\seealso{
  '\code{\link{loadData}}' '\code{\link{mergeData}}'
  '\code{\link{showColours}}' '\code{\link{colours}}' '\code{\link{showSymbols}}'
'\code{\link{read.table}}'
}

\keyword{file}
\concept{File}
\concept{Menu: GCDkit}
\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}
