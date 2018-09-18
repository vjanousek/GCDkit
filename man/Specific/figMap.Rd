\name{figMap}
\alias{figMap}

\title{Spatial distribution maps}

\section{Plugin}{ArcGIS}

\description{
    Plots a map of a spatial distribution of the given geochemical parameter, underlain by 
    the selected layers of an ArcView map. 
}

\usage{
    figMap(obj = GCDmap, var = NULL, main = "A map", palette = "grays", pch = 19, 
    cex = 1, layers = NULL, legend = TRUE, 
    proj.string = CRS("+proj=longlat +datum=WGS84"))
}

\arguments{
  \item{obj}{object with shapefile definition of the map}
  
  \item{var}{variable nam; formulae are ok}
  
  \item{main}{Main title of the map.}
  
  \item{palette}{Name of palette to be used for the plotting symbols.}
  
  \item{pch}{plotting symbol(s)} 

  \item{cex}{size of plotting symbols}
  
  \item{layers}{map layers to be drawn}
  
  \item{legend}{logical, should be the legend shown?}
  
  \item{proj.string}{projection string for the geochemical data}
}

\details{
First of all, the ArcView map is constructed. Its definition is given in an object 
'\code{GCDmap}', storing all previously loaded shapefile(s) as well as some further 
properties of the map, such as legend and compass rose. 

The (selected) map layers (individual shape files) are drawn, one on the top of another.\cr
If '\code{layers="select"}', the layers can be chosen from the list of the available ones.
If '\code{layers}' is empty (default), all layers are shown. 

For selection of the variable is employed the function '\code{\link{selectColumnLabel}}'.
The user can specify either existing data column in the '\code{WR}' or a formula.

If '\code{palette="select"}', the palette name is to be picked using 
\code{\link{selectPalette}}. 
The possible values are: \code{'grays','reds','blues','greens','cyans','violets','yellows','cm.colors'},
\code{'heat.colors', 'terrain.colors','topo.colors'}, \code{'rainbow'} 
and \code{'jet.colors'}.

The specified geochemical data are plotted at coordinates \code{[XX,YY]} stored in 
namesake columns of the \code{'WR'} matrix. They are converted to spatial points 
using the projection specified by \code{'proj.string'}.\cr
If necessary, they are then re-projected using the projection string of the first 
(master) layer of the map object. 

So far, colours of individual data points cannot be specified individually. Instead they
are based on statistical distribution of the variable plotted, using various shades of 
the '\code{palette}'. This approach is analogous to the function '\code{assignColVar}'.
 }

\value{
    None. 
    So far assigns plotting symbols and colours globally, i.e. alters the data frame 
    \code{'labels'}. If this is not desired, one should consider the function 
    '\code{\link{ArcMapDraw}}'.
}

\references{
    None.
}

\seealso{
    \link{ArcMapDraw}
    \link{assignColVar}
    \link{figaro}
}

\keyword{hplot}
\concept{Maps}
\concept{GIS}
\concept{Plot}
\concept{Menu: Plugins}
\concept{Plugin}

\author{
    Vojtech Janousek, \email{vojtech.janousek@geology.cz}
    This code relies heavily on \code{sp} package that 
    was written by Edzer Pebesma, Roger Bivand and others.  
}
