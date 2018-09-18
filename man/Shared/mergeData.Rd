\name{mergeData}
\title{Appending data to a current data set}
\alias{mergeDataRows}
\alias{mergeDataCols}
\alias{mergeData}

\description{
These functions append new data to the analyses currently stored in the memory 
of the GCDkit. 
}


\usage{
mergeDataRows()
mergeDataCols(all.rows=NULL)
}

\arguments{
  \item{all.rows}{logical; should be  all samples preserved, even those missing in one of the datasets ?}
}

\details{
The function '\code{mergeDataRows}' appends new samples (i.e. new rows). 
The structures of both datafiles are, as much as possible, matched against each 
other, and, if necessary, new empty columns are introduced to the original data 
file, if they are missing. If any duplicated sample names are found, they are 
replaced by sequence numbers and a new  column '\code{old.ID}' is appended to the 
labels. Also appended is a column named '\code{file}' containing the name of the 
file the particular sample originated from.

'\code{mergeDataCols}' adds new data (i.e. new data columns) to the samples 
stored in the memory. If desired ('\code{all.rows}' is '\code{TRUE}'),  
included are also samples that occur solely in one of the files.

For the guidelines on correct formatting of the data files see \code{\link{loadData}}.
}

\value{
  \item{WR}{numeric matrix: all numeric data}
  \item{labels}{data frame: all at least partly character fields; \code{labels$Symbol} 
  contains plotting symbols and \code{labels$Colour} the plotting colours}

The function prints a short summary about the loaded file.
}

\seealso{
  '\code{\link{loadData}}' '\code{\link{saveData}}' '\code{\link{merge}}'
}

\keyword{file}
\concept{File}
\concept{Menu: GCDkit}

\author{
    Vojtech Janousek, \email{vojtech.janousek@geology.cz}
} 
