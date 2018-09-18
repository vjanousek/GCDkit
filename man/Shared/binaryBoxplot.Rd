\name{binaryBoxplot}
\title{Binary boxplot}
\alias{binaryBoxplot}

\description{
A binary plot combined with boxplots for both variables.
}

\usage{
binaryBoxplot(xaxis="",yaxis="")   
}

\arguments{
\item{xaxis, yaxis}{specification of the variables. Formulae are OK.}
}

\details{
Unless specified in the call, the variables 
to be plotted are selected 
using the function 
'\code{\link{selectColumnLabel}}.

In the specification of the variables can be used also arithmetic 
expressions, see \code{\link{calcCore}} for the correct syntax.

The samples can be selected based on combination of three searching 
mechanisms (by sample name/label, range or a Boolean condition) - see 
\code{\link{selectSubset}} for details. 

The function is not Figaro-compatible.
}

\value{
None.
}

\examples{
binaryBoxplot("SiO2/10","Na2O+K2O")
}

\seealso{
  \link{plot}
  \link{boxplot}
}

\keyword{hplot}
\concept{Plot}
\concept{Menu: Plots}

\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}
