\name{assign1col}
\title{Uniform colours}
\alias{assign1col}

\description{
Assigns the same plotting colour to all samples.
}

\usage{
assign1col(col=-1)
}

\arguments{
\item{col}{numeric; code of the colour.}
}

\details{
This function sets the same colour to all of the plotting symbols. If '\code{col}' 
= -1 (the default), the user is prompted to specify its code.
}

\seealso{
To display the current legend use \code{\link{showLegend}}. 
Symbols and colours by a single label can be assigned by \code{\link{assignSymbLab}} 
and \code{\link{assignColLab}} respectively, symbols and colours by groups 
simultaneously by \code{\link{assignSymbGroup}}. Uniform symbols are obtained 
by \code{\link{assign1symb}}. Table of available plotting symbols is displayed 
by \code{\link{showSymbols}} and colours by \code{\link{showColours}}.
}


\value{
Sets '\code{labels$Colour}' to code of the selected plotting colour.
}

\keyword{color}
\concept{Edit}
\concept{Plot}
\concept{Menu: Data handling}
\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}
