\name{Export to Excel}
\title{Export to Excel}
\alias{excelExport}
\alias{excel2007Export}

\description{
This function serves for exporting the specified data into XLS or XLSX (MS Excel) formats 
(via the ODBC interface).
}

\usage{
excelExport(what=cbind(labels, WR), tablename =NULL,
transpose=FALSE, dec.places=NULL)

excel2007Export(what=cbind(labels, WR), tablename =NULL,
transpose=FALSE, dec.places=NULL)
}

\arguments{
  \item{what}{a matrix, data frame or a list}
  \item{tablename}{name of the data sheet}
  \item{transpose}{logical; transpose the data?}
  \item{dec.places}{numeric; number of decimal places}
}

\details{
The functions \code{excelExport} and \code{excel2007Export} output the specified 
data via Microsoft's ODBC interface, taking an advantage of the library \code{'RODBC'}. 

If the argument \code{'what'} is a matrix or data frame, the name of the sheet
can be specified using the optional parameter \code{'tablename'}.

For a list, several sheets are attached, their number and names corresponding to 
the items present. 
}

\value{
  None.
}

\section{Warning}{
These functions are not available on 64-bit systems!

Unfortunately the way the ODBC is programmed by Microsoft does not make opening 
a new Excel file possible. Thus only adding new sheet(s) to a pre-existing 
spreadsheet file is feasible.    
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
excelExport(results) # Saves the last calculated results in XLS format

excel2007Export(results) # Saves the last calculated results in XLSX (or XLS) format
}

\seealso{
'\code{\link{accessExport}}'  '\code{\link{dbfExport}}' 
}
