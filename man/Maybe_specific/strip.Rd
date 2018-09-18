\name{strip}
\alias{strip}
\title{Statistics: Stripplot by groups}

\description{
Stripplot for selected samples and variable, respecting the grouping.
}

\usage{
strip(xlab = "", ...)
}

\arguments{
  \item{xlab}{variable name}
  \item{...}{additional parameters to \code{stripplot}}
}

\details{
Stripplot shows 1D scatter plots for each of the groups, with some artificial 
noise (jitter) added to make the individual points better visible. Stripplots 
are a good alternative to boxplots when sample sizes are small.

If no variable is specified as an argument 
'\code{xlab}', the user can enter 
it using the function '\code{\link{selectColumnLabel}}'. 

In the specification of the variable can be used also 
arithmetic expressions, see 
\code{\link{calcCore}} 
for the correct syntax.
}


\seealso{
\code{\link{stripplot}}
}

\examples{
strip("(Na2O+K2O)/Al2O3")
}

\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}

\keyword{hplot}
\keyword{univar}
\concept{Plot}
\concept{Menu: Calculations: Statistics}
\concept{Statistics}
\concept{Grouping}
