\name{assignSymbLab}
\title{Symbols by label}
\alias{assignSymbLab}

\description{
    Assigns plotting symbols according to the levels of the chosen label or, alternatively, sample names.
}

\usage{
assignSymbLab(lab = NULL, symbols = NULL, display.legend = FALSE)
}

\arguments{
    \item{lab}{specification of the variable to be used for symbols assignment. See Details.}
    \item{symbols}{a vector with codes of plotting symbols to be assigned. Batch mode only.}
    \item{display.legend}{logical; should be the legend displayed? Batch mode only.}
}


\details{
    If called from in interactive mode (from GUI), the variable (sample names or label) can be 
    selected using the function '\code{\link{selectColumnLabel}}'.
    
    In batch mode, '\code{lab}' can be an integer (1 for sample names, 
    or a sequence number of the column in the '\code{labels}' plus 1). 
    Alternatively, it can contain the full name of a column in '\code{labels}'. See examples.
    
    If in batch mode, '\code{symbols}' have to be specified for the
    correct plotting symbols assignment.
}

\value{
    Sets '\code{leg.pch}' to a sequence number of column in '\code{labels}' that is 
    to be used to build the legend or -1 if sample numbers are to be used;
    '\code{labels$Symbol}' contains the codes for desired plotting symbols.
}


\seealso{
    To display the current legend use \code{\link{showLegend}}. 
    
    Using the function \code{\link{assignSymbLett}}, initial letters 
    of the respective levels of the chosen label can be assigned to 
    the plotting symbols.
    
    Colours by a single label can be assigned by \code{\link{assignColLab}}, 
    symbols and colours by groups simultaneously by \code{\link{assignSymbGroup}}. 
    Uniform colours and symbols are obtained by \code{\link{assign1symb}} and 
    \code{\link{assign1col}}. Table of available plotting symbols is displayed by 
    \code{\link{showSymbols}} and colours by \code{\link{showColours}}.

    Selecting a label: \code{\link{selectColumnLabel}}.
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
    
assignSymbLab()              # Interactive mode
    
# Sample names, standard GCDkit colours palette 
assignSymbLab(1,symbols=1:nrow(WR),display.legend=TRUE)             
    
assignSymbLab(2,symbols=c("+","*","@"),display.legend=TRUE)         # First column in labels
assignSymbLab("Intrusion",symbols=c(12,15,17),display.legend=TRUE)  # Ditto (here Intrusion)
}
