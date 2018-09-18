\name{groupsByLabel}
\title{Groups by label}
\alias{groupsByLabel}

\description{
    Grouping the data according to the levels of a single label.
}

\usage{
groupsByLabel(lab=NULL)
}

\arguments{
  \item{lab}{name or sequence number of the label}
}

\details{
    Sets the groups on the selected column within the data frame '\code{labels}'.
    If not specified at the function call, the appropriate label is selected 
    by the function '\code{\link{selectColumnLabel}}'. 
}

\value{
  \item{groups}{character vector: the grouping information}
  \item{grouping}{the sequence number of the column in the data frame '\code{labels}' 
  used for grouping}
}

\seealso{
    \link{classify}  \link{groupsByCluster} \link{groupsByDiagram}  
}

\examples{
data(sazava)
accessVar("sazava")
groupsByLabel("Intrusion")
}
 
\keyword{manip}
\concept{Grouping}
\concept{Edit}
\concept{Menu: Data handling}
\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}
