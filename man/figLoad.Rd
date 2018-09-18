\name{figLoad}
\title{Loading a Figaro plot}
\alias{figLoad}

\description{
Loads a Figaro-compatible plot (both the template and the data) stored in a file. 
}

\usage{
figLoad()
}

\arguments{
None.
}

\details{
The default suffix for the saved diagrams is '\code{fgr}'. Note that only the data 
needed for the plotting ('\code{x.data}', '\code{y.data}') are stored in
the 'fgr' files. Thus the data set currently in memory (e.g., variables '\code{WR}', 
'\code{labels}', \ldots) is unaffected by the function '\code{figLoad}'.
}

\seealso{
  \link{figSave}
  \link{figaro}  
}

\keyword{hplot}
\keyword{file}
\concept{Plot}
\concept{Figaro}
\concept{File}
\concept{Menu: Plot editing}

\author{
Colin Farrow, \email{colinfarrow537@gmail.com} 

and Vojtech Janousek, \email{vojtech.janousek@geology.cz}
}
