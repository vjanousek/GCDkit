\name{selectByDiagram}
\title{Selecting subset by diagram}
\alias{selectByDiagram}

\description{
This function enables selecting samples that plot into certain field(s) 
of the given classification diagram. 
}

\usage{
selectByDiagram(diagram =  select.list(claslist[, "menu"]))
}

\arguments{
  \item{diagram}{one of the valid diagram names that appear in '\code{.claslist()}'}
}

\details{
The diagram can be chosen from a list (the default) or specified directly as an 
argument. Clicking onto a field toggles its inclusion/exclusion - the currently 
selected fields are cyan.
}

\value{
  None.
}

\author{
Vojtech Janousek, \email{vojtech.janousek@geology.cz} 
& Vojtech Erban, \email{vojtech.erban@geology.cz} 
}

\seealso{
'\code{\link{selectByLabel}}', '\code{\link{selectSubset}}', 
'\code{\link{selectAll}}' and '\code{\link{classify}}'.
}

\examples{
.claslist() # names of existing diagrams
selectByDiagram("TAS")
}


\keyword{manip}
\keyword{iplot}
\concept{Edit}
\concept{Subsetting}
\concept{Plot}
\concept{Figaro}
\concept{Classification}
\concept{Menu: Data handling}
