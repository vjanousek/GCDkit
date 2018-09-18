\name{assignSymbLett}
\title{Symbols by label - initial letters}
\alias{assignSymbLett}

\description{
    Assigns plotting symbols to initial letters of the respective levels of the chosen label.
}
\usage{
assignSymbLett(lab = NULL, display.legend = FALSE)
}

\arguments{
    \item{lab}{specification of the variable to be used for symbols assignment. See Details.}
    \item{display.legend}{logical; should be the legend displayed? Batch mode only.}
}

\details{
    If called from in interactive mode (from GUI), the variable (sample names or label) can be 
    selected using the function '\code{\link{selectColumnLabel}}'.
    
    In batch mode, '\code{lab}' can be an integer (a sequence number of the 
    column in the '\code{labels}'). 
    Alternatively, it can contain the full name of a column in '\code{labels}'. 
    See examples.
}

\value{
    Sets '\code{leg.pch}' to a sequence number of column in '\code{labels}' that is 
    to be used to build the legend; '\code{labels$Symbol}' contains the  plotting 
    symbols, which correspond to initial letters for the levels of the specified label.
}


\seealso{
    To display the current legend use \code{\link{showLegend}}. 
    Symbols by a single label can be assigned by \code{\link{assignSymbLab}}, 
    colours by \code{\link{assignColLab}}, symbols and colours by groups simultaneously 
    by \code{\link{assignSymbGroup}}. Uniform colours or symbols are achieved by 
    \code{\link{assign1symb}} and \code{\link{assign1col}}. Table of available plotting 
    symbols is displayed by \code{\link{showSymbols}} and colours by \code{\link{showColours}}.
}

\keyword{aplot}
\concept{Edit}
\concept{Plot}
\concept{Grouping}
\concept{Menu: Data handling}
\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}

\examples{
data(sazava)
accessVar("sazava")
    
assignSymbLett()              # Interactive mode
    
assignSymbLett(2,display.legend=TRUE)           # Second column in labels 
assignSymbLett("Locality",display.legend=TRUE)  # The same (here Locality)
}

