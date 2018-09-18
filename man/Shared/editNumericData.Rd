\name{Edit numeric data}
\title{Edit numeric data}
\alias{editData}

\description{
    Simultaneous editing of all numeric data using a spreadsheet-like interface.
}

\usage{
    editData(x=WR)
}

\arguments{
  \item{x}{data frame/numeric matrix to be edited; default is '\code{WR}', i.e. numeric data}
}


\details{ 
    The function invokes a spreadsheet-like interface that enables the user to edit 
    the numeric data for individual samples. When all the desired changes have been 
    performed, close button is to be clicked. 

    The system then performs some recalculations as if the data set was loaded from 
    the disc afresh (calling '\code{Gcdkit.r}').
}

\value{
    Returns the corrected version of the numeric matrix '\code{WR}'.
}

\seealso{
    '\code{\link{data.entry}}'
}

\keyword{manip}
\concept{Edit}
\concept{Menu: Data handling}
\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}
