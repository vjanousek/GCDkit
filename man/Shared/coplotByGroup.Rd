\name{coplotByGroup}
\title{Coplot by groups}
\alias{coplotByGroup}

\description{
Plots a series of binary plots, for each of the groups separately. 
}

\usage{
coplotByGroup(xaxis = "",yaxis = "",show.leg = "")
}

\arguments{
\item{xaxis}{Name of the data column to be used as x axis.}
\item{yaxis}{Name of the data column to be used as y axis.}
\item{show.leg}{Logical: are the levels of the conditioning variable 
    ('\code{groups}') to be shown?}
}

\details{
For examination of large datasets split into user-defined subsets serves in R 
function \code{\link{coplot}}. 
It produces a set of binary diagrams with the data filtered out according to the 
values of the third (conditioning) variable. In case of the function 
'\code{coplotByGroup}' it is done by groups.

If no parameters \code{'xlab', 'ylab'} and '\code{show.leg}' are given, the user 
is prompted to specify them.

The variables to be plotted are selected using the function 
'\code{\link{selectColumnLabel}}.

See manual entry for '\code{\link{coplot}}' for further details.
}

\section{Warning}{
Please note that no formulae can be handled at this stage.

This function IS NOT Figaro-compatible.
}

\value{
None.
}

\seealso{
'\code{\link{coplot}}'
}

\examples{
coplotByGroup("SiO2","Na2O",show.leg=TRUE)
}

\keyword{hplot}
\concept{Plot}
\concept{Grouping}
\concept{Menu: Plots}

\author{
    Vojtech Janousek, \email{vojtech.janousek@geology.cz} 
    & Vojtech Erban, \email{vojtech.erban@geology.cz}
}
