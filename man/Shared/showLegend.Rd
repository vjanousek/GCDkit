\name{showLegend}
\title{Display legend}
\alias{showLegend}

\description{
    Displays a graphical legend(s) with assignment of plotting symbols and colours 
    used by majority of the diagrams.
}

\usage{
showLegend(pch=labels$Symbol,col=labels$Colour,new.plot=TRUE,alt.leg=FALSE,
just.colours=FALSE,GUI=FALSE)
}

\arguments{
    \item{pch}{numeric or character: plotting symbols.}
    \item{col}{numeric: code for their colour.}
    \item{new.plot}{logical: shall be opened a new plotting window for the legend?}
    \item{alt.leg}{logical; should be the alternative (continuous) legend shown? See details.}
    \item{just.colours}{logical; in cases when two legends would be created, should be only that 
            for plotting colours shown?}
    \item{GUI}{logical; Is teh function called from GUI (and not batch mode)?}
}

\details{
    The internal variables '\code{leg.col}' and '\code{leg.pch}' are set to zero, if 
    the current assignment is on the basis of '\code{groups}'. Otherwise they contain 
    the sequential number(s) of column(s) in the data frame '\code{labels}' whose 
    levels are to be used to build the legend(s). 
    
    If both variables differ, two legends are created, for plotting symbols and colours separately. 
    This is done unless '\code{just.colours}' is set, when only legend for colours is displayed. 
    
    If both variables equal zero, the current grouping information is used.
    
    If a complete colour scale is used for plotting symbols, for instance that created 
    by the \code{\link{assignColVar}} function, an alternative (continuous) 
    legend can be drawn.   
}

\value{
    None.
}

\examples{
    showLegend()
}

\seealso{
    Symbols and colours by a single label can be assigned by functions 
    \code{\link{assignSymbLab}} and \code{\link{assignColLab}} respectively, 
    symbols and colours by groups 
    simultaneously by \code{\link{assignSymbGroup}}. 
    Symbols can be colour-coded according to a 
    variable using the function \code{\link{assignColVar}}. 
    Uniform symbols are obtained by \code{\link{assign1symb}}, uniform colours by 
    \code{\link{assign1col}}. Table of available plotting symbols is displayed by 
    \code{\link{showSymbols}} and colours by \code{\link{showColours}}.   
}

\keyword{iplot}
\concept{Plot}
\concept{Menu: Data handling}

\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}
