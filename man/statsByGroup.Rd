\name{statsByGroup}
\title{Statistics by groups}
\alias{statsByGroup}

\description{
Calculates simple descriptive statistics for individual columns of the given 
data matrix; optionally this can be done for each of the groups separately. 
}

\usage{
statsByGroup(data = WR, groups = groups)
}

\arguments{
\item{data}{numeric data matrix.}
\item{groups}{a vector, in which is specified, for each sample, a group it belongs to.}
}

\details{
The function returns a list containing the calculated statistical parameters 
respecting the desired grouping. The statistical summary involves number of 
observations, missing values, mean, standard deviation, minimum, 25\% quartile, 
median (= 50\% quartile), 75\% quartile and maximum. This is a core function 
invoked both by \code{\link{summarySingle}} and \code{\link{summarySingleByGroup}}.
}

\examples{
statsByGroup(WR)

statsByGroup(WR[,LILE])
}


\value{
\item{results}{a list with the results for individual groups}
}

\seealso{
\itemize{
\code{\link{summarySingle}}

\code{\link{statistics}}

\code{\link{summaryAll}}

\code{\link{summaryByGroup}}
}
}

\keyword{univar}
\concept{Calculation}
\concept{Grouping}
\concept{Menu: Calculations: Statistics}
\concept{Statistics}

\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}
