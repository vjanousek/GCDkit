\name{statsByGroupPlot}
\title{Statistics: Plot summary by element and group}
\alias{statsByGroupPlot}

\description{
Plots crosses in a binary diagram denoting means and standard deviations for 
individual groups.
}

\usage{
statsByGroupPlot()
}

\details{
Displays a binary diagram of two elements/oxides in which are plotted averages 
for the individual groups with whiskers corresponding to their standard 
deviations.

The variables are entered via the function '\code{\link{selectColumnLabel}}'. 
In the specification of the variables can be used also arithmetic expressions, 
see \code{\link{calcCore}} for the correct syntax.
}

\value{
\item{results}{a matrix with the results for individual groups and selected two 
elements/oxides}
}

\keyword{hplot}
\keyword{univar}
\concept{Plot}
\concept{Grouping}
\concept{Menu: Calculations: Statistics}
\concept{Statistics}

\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}
