\name{overplotDataset}
\alias{overplotDataset}
\alias{underplotDataset}
\title{
    Adding another dataset to the current plot
}
\description{
    This function allows overplotting new data points stored in the memory onto any type of 
    single Figaro-compatible plots (or their plates). This can be done either into foreground 
    or into background. 
}

\usage{   
    overplotDataset(reference.dataset=NULL, underplotting=FALSE, transp=0, 
      pch=NULL, col=NULL, cex=NULL, ...)
    underplotDataset(reference.dataset=NULL, transp=0,...)
}

\arguments{
  \item{reference.dataset}{object name (given as a character string or unquoted); 
       the dataset to be added to the current diagram. See Details.}
  \item{underplotting}{logical; should be the reference dataset added at the background?}
  \item{transp}{numeric, 0-1; transparency of the background dataset (in underplotting).}
  \item{pch}{plotting symbol(s) for the foreground dataset.}
  \item{col}{plotting colour(s) for the foreground dataset.}
  \item{cex}{numeric; relative size of the plotting symbol(s) for the foreground dataset.}
  \item{...}{additional parameters to the underlying plotting function(s). See Details.}
}

\details{
    These are front-ends to the functions '\code{figOverplot}' and '\code{figOverplotDiagram}',  
    invoked as appropriate. However, the functions '\code{overplotDataset}' and '\code{underplotDataset}'  
    work correctly also on plates. 
    
    Also \code{underplotDataset} is just a convenience function, calling 
    \code{overplotDataset} with the parameter \code{underplotting=TRUE}.
    
    Most typically, \code{reference.dataset} is a (quoted) name of a dataset stored in memory. 
    Alternatively, a (unquoted) name of a global variable can be specified.
    
    Plotting parameters '\code{pch}', '\code{col}' and '\code{cex}' are available only 
    for overplotting.
    
    On the other hand, transparency can be set only in underplotting. 
    See '\link{setTransparency}' for further info. 
    
    Argument '\code{...}' can supply additional parameters to the original plotting functions 
    (e.g., '\link{TAS}')  invoked by '\link{plotDiagram}' or  '\link{plateExtract}'.  
}

\section{Note}{
    This function is a front-end, truly a 'mother of all' specialized and less versatile 
    overplotting functions such as '\code{figOverplot}', '\code{figOverplotDiagram}' or
    '\code{figAddReservoirs(just.draw=TRUE)}'. Please use '\code{overplotDataset}' instead, unless
    permanent addition to the plot is required. For such cases, 
    '\code{figAddReservoirs}' '\code{plateAddReservoirs}' in their default form, i.e. 
    with argument \code{just.draw=FALSE}, are the functions of choice.  
}

\section{Warning}{
    NB that the points for the overplotted dataset do not from a part of the template, 
    and thus will vanish upon redrawing, zooming \ldots.    
}

\value{
    (Invisibly) name of the reference dataset.
}

\seealso{
  '\link{figOverplot}' '\link{figOverplotDiagram}' '\link{figAddReservoirs}' '\link{plateAddReservoirs}'
  
  '\link{peekDataset}' '\link{pokeDataset}' '\link{purgeDatasets}'  
  
  '\link{setTransparency}' '\link{plotDiagram}'  '\link{plateExtract}'
}

\examples{
# Loading two testing datasets
data(sazava)
accessVar("sazava")
    
data(blatna)
accessVar("blatna")
setCex(1.5)
pokeDataset("blatna",overwrite.warn=FALSE) # Store a version with larger symbols
    
# Single plots
peekDataset("blatna")
plotDiagram("DebonPQ",FALSE,TRUE)
figRemove()
overplotDataset("sazava",cex=2,col="darkred",pch=15)
    
plotDiagram("DebonPQ",FALSE,TRUE)
figRemove()
underplotDataset("sazava",transp=0.5)
    
plateExtract("PearceGranite",2,main=" ")
overplotDataset("sazava")
    
# Spiderplots
peekDataset("blatna")
spider(WR,"Boynton",1,1000,cex=0,join=TRUE,offset=TRUE,
  centered=FALSE,xrotate=FALSE,xaxs="r")
overplotDataset("sazava")
    
spider(WR,"Boynton",0.1,1000,field=TRUE,fill.col=TRUE,shaded.col="gray") 
# Blatna as gray field
overplotDataset("sazava")
    
# A simple plate
peekDataset("blatna")
multiple("SiO2",y="TiO2,Al2O3,FeOt,MgO,CaO,Na2O,K2O,P2O5",nrow=3,ncol=3,main="")
plateCex(1.8)
plateCexLab(1.3)
overplotDataset("sazava")

# A plate of classification diagrams
peekDataset("blatna")
multiplePerPage(4,nrow=2,ncol=2,title="A classification plate")
plotDiagram("DebonPQ",FALSE,FALSE,main=" ")
plotDiagram("DebonBA",FALSE,FALSE,main=" ")
plotDiagram("LarocheVolc",FALSE,FALSE,main=" ") 
plotDiagram("Meschede",FALSE,FALSE,main=" ")
plateLabelSlots(text=letters,cex=1.5,pos="topleft")
plateCexLab(1.2)
plateCol("black")

plateRedraw()
overplotDataset("sazava") 

plateRedraw()
overplotDataset("sazava",cex=2,col=2,pch=15)

plateRedraw()
underplotDataset("sazava",transp=0.5)
}

\keyword{aplot}
\keyword{iplot}
\concept{Plot}
\concept{Figaro}
\concept{Switching datasets}
\concept{Menu: GCDkit}

\author{
 Vojtech Janousek, \email{vojtech.janousek@geology.cz}
}
