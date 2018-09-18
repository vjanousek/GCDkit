\name{ArcMapSetup}
\alias{ArcMapSetup}

\title{Drawing Arc GIS shapefiles}

\description{
This function provides a rudimentary support for drawing Arc GIS-compatible shape files (.shp).
}

\usage{
    ArcMapSetup(object, layers = NULL, map.col = NULL, map.palette = "heat.colours", labels.txt = FALSE, 
    col.txt = "black", cex.txt = 0.5, axes = TRUE, longlat = TRUE, xlab = "Longitude", ylab = "Latitude")
}


\arguments{
   \item{object}{name of the object to be drawn, normally \code{GCDmap}.}
   \item{layers}{names of layers to be drawn.}
   \item{map.col}{a vector with colors specified for each of the polygons.}
   \item{map.palette}{name of a palette to fill the individual polygons by a random colour.}
   \item{labels.txt}{logical; label the individual polygons?}
   \item{col.txt}{colour of these textual labels.}
   \item{cex.txt}{relative size of these textual labels.}
   \item{axes}{logical; should be the axes drawn?}
   \item{longlat}{logical; should be long-lat grid added?}
   \item{xlab}{label for the x axis.}
   \item{ylab}{label for the y axis.}
}

\details{
By default, the  \link{loadData} function of the \emph{GCDkit} system loads a shape (*.shp)
file into a list object called \code{GCDmap}. Each layer represents one item. 

If required, the longitude-latitude grid is also drawn using the function \link{llgridlines}.
}

\value{
None. It just modifies properties of a Figaro object (a map).
}


\examples{
    # Example of a public-domain World map
    shp.file<-"world_country_admin_boundary_shapefile_with_fips_codes.shp"
    setwd(earthchem.dir)
    loadData(shp.file)
    figRedraw()
    
    ArcMapSetup(GCDmap,map.palette="heat.colors",labels.txt=TRUE,col.txt="darkblue",cex.txt=0.8,axes=TRUE,longlat=FALSE)
    figRedraw()
    
    #Scaling (not precise clipping, as it needs to preserve the aspect ratio)
    figXlim(c(-77,-50))
    figYlim(c(0,30))
    
    # Other Figaro functions should be finally working, too
    figMain("Caribbean and adjacent South America")
    figColMain("darkred")
}

\references{
    None.
}

\seealso{
    \link{sp}
    \link{readOGR}
    \link{llgridlines}
    \link{loadData}
    \link{assignColVar}
    \link{figaro}
    \href{ http://proj.maptools.org/}{ http://proj.maptools.org}. 
   
}

\keyword{hplot}
\concept{Maps}
\concept{GIS}
\concept{Plot}

\author{
    Vojtech Janousek, \email{vojtech.janousek@geology.cz}
    
    This code relies heavily on \code{rgdal} and \code{sp} packages that 
    were written by Roger Bivand, Edzer Pebesma and their co-workers.  
}
