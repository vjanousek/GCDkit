\name{selectColumnLabel}
\alias{selectColumnLabel}

\title{Selecting a single variable in GCDkit}

\description{
This is an auxiliary function invoked by many others to select a single variable.
}

\usage{
selectColumnLabel(where = colnames(labels), 
message = "Select the variable\nor press ENTER to pick from a list", 
default = "", sample.names = FALSE, silent = FALSE, print = TRUE, 
empty.ok = TRUE)
}

\arguments{
  \item{where}{names of data columns to choose from}
  \item{message}{prompt}
  \item{default}{comma delimited list of default names}
  \item{sample.names}{logical; should be the sample names listed}
  \item{silent}{logical, echo on/off}
  \item{print}{logical, echo on/off}
  \item{empty.ok}{is empty selection ok?}
}

\details{
The easiest way for specification of the variable is to type directly the name 
of the numerical column in the data matrix '\code{WR}' (e.g., 'SiO2') or its 
sequence number (2 for the second column). However, it is not necessary to enter 
the name in its entirety. Only a substring that appears somewhere in the column 
name or other forms of \code{\link{regular.expressions}} can be specified. 

If the result is ambiguous, the correct variable has to be selected by mouse from 
the list of the multiple matches. Ultimately, empty response invokes list of all 
variables available in the memory.
}

\value{
A numeric index of the selected column.
}

\keyword{manip}
\keyword{database}
\concept{Subsetting}
\concept{Edit}
\concept{Menu: Data handling}

\seealso{
\code{\link{selectColumnsLabels}}
}

\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}
