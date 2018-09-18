\name{assignColVar}
\title{Colours by a variable}
\alias{assignColVar}


\description{
    Assigns plotting colours according to the values of the variable.
}

\usage{
assignColVar(what=NULL,pal="heat.colours",save=TRUE,n=15,quant=0,eq.classes=FALSE,alt.leg=FALSE)
}

\arguments{
    \item{what}{variable name or a formula; if NULL a dialogue is displayed}  
    \item{pal}{character; name of a palette}
    \item{save}{logical;should the newly picked colours be assigned to '\code{labels}'?}
    \item{n}{desired approximate number of colours to be assigned.}
    \item{quant}{numeric, 0-50; quantile to be potentially used to get rid of outliers. See details.}
    \item{eq.classes}{logical; should classes contain equal number of values?}
    \item{alt.leg}{logical; should be the alternative (continuous) legend shown? See Examples.}
}

\details{
    For selection of the variable is employed the function '\code{\link{selectColumnLabel}}'.
    The user can specify either existing data column in the '\code{WR}' or a formula.
    The colours can be optionally (default behaviour) assigned globally, so that all the plots
    will use these from this point on.
    If not specified upon function call, the palette is picked using \code{\link{selectPalette}}. 
    The possible values are: 
    \code{'grays','reds','blues','greens','cyans','violets','yellows','cm.colors'},\cr
    \code{'heat.colors', 'terrain.colors','topo.colors', 'rainbow'} 
    and \code{'jet.colors'}.\cr

    Also, user-defined palette functions are supported now. See Examples. 

    The analyses with no data available for the colours assignment will remain black. 
    
    If \code{quant} differs from the default value of zero, the data are trimmed to an 
    interval \code{(quant, 100-quant)}-th quantile of the dataset and all values out of it
    plotted in gray. 
    
    Setting \code{eq.classes=TRUE} allows to have classes with equal number of values 
    (as opposed to equal intervals). This option is best suited for very skewed datasets 
    (lots of points with similar values, some outliers). 
}

\value{
    A list of two components, \code{col} and \code{leg}. The former are the plotting colours, 
    the latter contains information needed to build a legend.
    If \code{save = TRUE}, '\code{labels$Colour}' will acquire the codes of 
    desired plotting colours.
}


\seealso{
    \code{\link{quantile}} 
    Colours by a single variable can be assigned by \code{\link{assignColLab}}, 
    symbols and colours by groups simultaneously by \code{\link{assignSymbGroup}}. 
    Uniform colours are obtained by \code{\link{assign1col}}.
    Table of available plotting colours is obtained by \code{\link{showColours}}.
}

\examples{
    assignColVar("Na2O/K2O","greens")
    plotDiagram("PeceTaylor",FALSE,FALSE)
    
    my.palette<-colorRampPalette(c("black", "darkgreen", "red"),space = "rgb")
    assignColVar("SiO2","my.palette")
    plotDiagram("PeceTaylor",FALSE,FALSE)
    
    assignColVar("SiO2","my.palette",n=7,quant=5)
    plotDiagram("PeceTaylor",FALSE,FALSE)
    showLegend()
    showLegend(alt.leg=TRUE)
}

\keyword{color}
\concept{Edit}
\concept{Plot}
\concept{Menu: Data handling}
\author{
    Vojtech Janousek, \email{vojtech.janousek@geology.cz}
    
    Jean-Francois Moyen, \email{jfmoyen@gmail.com}
}
