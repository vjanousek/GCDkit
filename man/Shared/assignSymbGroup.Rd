\name{assignSymbGroup}
\title{Symbols/colours by groups}
\alias{assignSymbGroup}

\description{
Lets the user to assign plotting symbols and colours according to the levels 
of the defined groups.
}

\usage{
assignSymbGroup()
}

\arguments{
None.
}

\value{
Sets '\code{leg.col}' and '\code{leg.pch}' to zero, '\code{labels$Symbol}' 
contains the codes of desired plotting symbols, '\code{labels$Colour}' of plotting colours.
}


\seealso{
To display the current legend use \code{\link{showLegend}}. 
Symbols by a single label can be assigned by \code{\link{assignSymbLab}}, 
colours using \code{\link{assignColLab}}. Uniform colours and symbols are 
obtained by \code{\link{assign1symb}} and \code{\link{assign1col}}.
Table of available plotting symbols is displayed by \code{\link{showSymbols}} 
and colours by \code{\link{showColours}}.
}


\keyword{aplot}
\concept{Edit}
\concept{Plot}
\concept{Grouping}
\concept{Menu: Data handling}
\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}

