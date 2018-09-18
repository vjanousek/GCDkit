\name{Export to Access}
\title{Export to Access}
\alias{accessExport}

\description{
This function serves for exporting the specified data into MDB (MS Access) format 
(via the ODBC interface).
}

\usage{
accessExport(what=cbind(labels, WR), tablename=NULL,
transpose=FALSE,dec.places=NULL)
}

\arguments{
  \item{what}{a matrix, data frame or a list}
  \item{tablename}{name of the data table}
  \item{transpose}{logical; transpose the data?}
  \item{dec.places}{numeric; number of decimal places}
}

\details{
The function \code{accessExport} outputs the specified data via Microsoft's ODBC 
interface, taking an advantage of the library \code{'RODBC'}. Unlike for the 
function '\code{\link{excelExport}}', ODBC makes possible opening a new file. 

If the argument \code{'what'} is a matrix or data frame, the name of the table
can be specified using the optional parameter \code{'tablename'}.

For a list, several tables are created, their number and names corresponding to 
the items present. 
}

\value{
  None.
}

\section{Warning}{
This function is not available on 64-bit systems! 
}

\keyword{file}
\concept{File}
\concept{Print}
\concept{Menu: Data handling}
\concept{Menu: Calculations}

\author{
The RODBC package was written by Brian Ripley.

Vojtech Janousek, \email{vojtech.janousek@geology.cz}
}

\examples{
accessExport(results) # Saves the last calculated results
}

\seealso{
'\code{\link{excelExport}}'  '\code{\link{dbfExport}}' 
}
