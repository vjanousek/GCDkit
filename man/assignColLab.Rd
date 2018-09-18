\name{assignColLab}
\title{Colours by label}
\alias{assignColLab}

\description{
    Assigns plotting colours according to the levels of the chosen label or, alternatively, sample names.
}

\usage{
assignColLab(lab = NULL, pal = NULL, colours = NULL, display.legend = FALSE)
}

\arguments{
    \item{lab}{specification of the variable to be used for colours assignment. See Details.}
    \item{pal}{character; name of the palette to be used when no colours are specified directly. Batch mode only.}
    \item{colours}{a vector with codes of colours to be assigned. Batch mode only.}
    \item{display.legend}{logical; should be the legend displayed? Batch mode only.}
}

\details{
    If called from in interactive mode (from GUI), the variable (sample names or label) 
    can be selected using the function '\code{\link{selectColumnLabel}}'.
    
    In batch mode, '\code{lab}' can be an integer (1 for sample names, 
    or a sequence number of the column in the '\code{labels}' plus 1). 
    Alternatively, it can contain the full name of a column in '\code{labels}'. See examples.
    
    If in batch mode, either '\code{colours}' or '\code{palette}' have to be specified for the
    correct colour assignment.
}

\value{
    Sets '\code{leg.col}' to a sequence number of column in '\code{labels}' that is 
    to be used to build the legend or -1 if sample numbers are to be used; 
    '\code{labels$Colour}' contains the codes of the desired plotting colours.
}


\seealso{
    To display the current legend use \code{\link{showLegend}}. 
    Symbols by a single label can be assigned by \code{\link{assignSymbLab}}, 
    symbols and colours by groups simultaneously by \code{\link{assignSymbGroup}}. 
    Uniform colours and symbols are obtained by \code{\link{assign1symb}} and 
    \code{\link{assign1col}}.
    Table of available plotting symbols is displayed by \code{\link{showSymbols}} 
    and colours by \code{\link{showColours}}.

    Selecting a label: \code{\link{selectColumnLabel}}.
    
    Selecting a palette: \code{\link{selectPalette}}.
}


\keyword{color}
\concept{Edit}
\concept{Plot}
\concept{Menu: Data handling}
\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}

\examples{
data(sazava)
accessVar("sazava")
    
assignColLab()              # Interactive mode
    
# Sample names, standard GCDkit colours palette 
assignColLab(1,colours=palette.gcdkit,display.legend=TRUE)        
    
# Standard palettes
assignColLab(3,pal="jet.colours",display.legend=TRUE)          # Second column in labels 
assignColLab("Locality",pal="jet.colours",display.legend=TRUE) # Ditto (here Locality)

# User defined palette
my.palette<-colorRampPalette(c("black", "darkgreen", "red"),space = "rgb")
assignColLab("Locality",pal="my.palette",display.legend=TRUE)
}
