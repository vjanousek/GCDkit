\name{assign1symb}
\title{Uniform symbols}
\alias{assign1symb}

\description{
Assigns the same plotting symbol to all samples.
}

\usage{
assign1symb(pch=-1)
}

\arguments{
\item{pch}{numeric; code of the plotting symbol.}
}

\details{
This function sets the same plotting symbol to all the data points. If 
'\code{pch}' = -1 (the default), the user is prompted to specify its code.
}

\seealso{
To display the current legend use \code{\link{showLegend}}. 
Symbols and colours by a single label can be assigned by \code{\link{assignSymbLab}} 
and \code{\link{assignColLab}} respectively, symbols and colours by groups 
simultaneously by \code{\link{assignSymbGroup}}. Uniform colours are obtained 
by \code{\link{assign1col}}. Table of available plotting symbols is displayed by 
\code{\link{showSymbols}} and colours by \code{\link{showColours}}.
}

\value{
Sets '\code{labels$Symbol}' to code of the selected plotting symbol.
}

\keyword{aplot}
\concept{Edit}
\concept{Plot}
\concept{Menu: Data handling}
\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}
