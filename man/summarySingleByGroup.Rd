\name{summarySingleByGroup}
\title{Statistics: Single variable by groups}
\alias{summarySingleByGroup}

\description{
Prints statistical summary for a single variable and the whole dataset, divided 
by groups.
}

\usage{
summarySingleByGroup(xlab="")
}

\arguments{
  \item{xlab}{variable name}
}


\details{
The statistical summary involves number of observations, missing values, mean,
standard deviation, minimum, 25\% quartile, median (= 50\% quartile), 75\% 
quartile and maximum. The function also plots a summary boxplot and histogram.

If no variable is specified as an argument 
'\code{xlab}', the user can enter 
it using the function \cr'\code{\link{selectColumnLabel}}'. 
In the specification 
of the variable can be used also arithmetic expressions, see 
\code{\link{calcCore}} for the correct syntax.
}

\value{
\item{results}{numeric matrix with the results}
}

\examples{
summarySingleByGroup("(Na2O+K2O)/Al2O3")
}

\seealso{
\code{\link{boxplot}}
\code{\link{summarySingle}}
\code{\link{statistics}}
\code{\link{summaryAll}}
\code{\link{summaryByGroup}}
}

\keyword{univar}
\keyword{hplot}
\concept{Calculation}
\concept{Plot}
\concept{Menu: Calculations: Statistics}
\concept{Statistics}
\concept{Grouping}

\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}
