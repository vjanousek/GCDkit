\name{Export to DBF}
\title{Export to DBF}
\alias{dbfExport}

\description{
This function serves for exporting the specified data into DBF (dBase III) format 
(using the function '\code{\link{write.dbf}}' of the package '\code{foreign}').
}

\usage{
dbfExport(what=cbind(labels,WR), transpose=FALSE)
}

\arguments{
  \item{what}{a matrix or data frame}
  \item{transpose}{logical; transpose the data frame?}
}

\details{
The function \code{dbfExport} outputs the specified data. Note that it
cannot handle lists.
}

\value{
  None.
}

\keyword{file}
\concept{File}
\concept{Print}
\concept{Menu: Data handling}
\concept{Menu: Calculations}

\author{
Vojtech Janousek, \email{vojtech.janousek@geology.cz}
}

\examples{
dbfExport(results) # Saves the last calculated results
}

\seealso{
'\code{\link{write.dbf}}' '\code{\link{excelExport}}'  '\code{\link{accessExport}}' 

}
