\name{summarySingle}
\title{Statistics: Single variable all/selection}
\alias{summarySingle}

\description{
Prints statistical summary for a single variable and the current dataset (or its part).
}

\usage{
summarySingle(xlab="")
}

\arguments{
  \item{xlab}{variable name}
}

\details{
The statistical summary involves number of observations, missing values, mean,
standard deviation, minimum, 25\% quartile, median (=50\% quartile), 75\% quartile 
and maximum. The function also plots a summary boxplot and histogram.

In addition the statistical distribution of the given variable is shown as a 
boxplot, a box-percentile plot and two variants of histograms.

    \if{html}{\figure{summarySingle.png}} 
    \if{latex}{\figure{summarySingle.png}{options: width=5.5in}} 
    
If no variable is specified as an argument '\code{xlab}', the user can enter 
it using the function 
'\code{\link{selectColumnLabel}}'. In the specification 
of the variable can be used also arithmetic expressions, see 
\code{\link{calcCore}} for the correct syntax.

The samples can be selected based on combination of three searching mechanisms 
(by sample name/label, range or a Boolean condition) - see 
\code{\link{selectSamples}} for details. 
}

\value{
\item{results}{numeric matrix/vector with the results}
}

\examples{
summarySingle("(Na2O+K2O)/Al2O3")
}

\seealso{
\code{\link{boxplot}}

\code{\link{bpplot2}}

\code{\link{statistics}}

\code{\link{summarySingleByGroup}}
\code{\link{summaryAll}}
\code{\link{summaryByGroup}}
}

\keyword{univar}
\keyword{hplot}
\concept{Calculation}
\concept{Plot}
\concept{Menu: Calculations: Statistics}
\concept{Statistics}

\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}
