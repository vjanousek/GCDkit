\name{figOverplotDiagram}
\alias{figOverplotDiagram}
\title{
    Overplotting data onto classification or geotectonic plots
}
\description{
    This function allows overplotting new data points onto single Figaro-compatible templates defined for 
    classification or geotectonic plots (binary or ternary, designed as stand alone or extracted from plates). 
}

\usage{
figOverplotDiagram(overplot.dataset, bg.dataset=NULL, diagram=NULL,
    which=NULL, xlim=NULL, ylim=NULL, pch="*", col="darkred", 
    cex=1, labs=NULL, type="p", lwd=1, lty="solid", 
    transp=0, just.draw=TRUE,...)
}


\arguments{
  \item{overplot.dataset}{(obligatory) name of the main (foreground) dataset stored in memory, 
       or global variable name.}
  \item{bg.dataset}{(optional) name of the background dataset stored in memory.}
  \item{diagram}{character; existing diagram name.}
  \item{which}{which plot is to be extracted (if belonging to a plate)?}
  \item{xlim}{new limits of the x axis.}
  \item{ylim}{new limits of the y axis.}
  \item{pch}{plotting symbol(s) for the foreground dataset.}
  \item{col}{plotting colour(s) for the foreground dataset.}
  \item{cex}{numeric; relative size of the plotting symbol(s) for the foreground dataset.}
  \item{labs}{text; optional  labels for the overplotted data.}
  \item{type}{character; see '\link{points}'.}
  \item{lwd, lty}{parameters for connecting line, if drawn; see '\link{par}'.}
  \item{transp}{numeric; transparency for the background set, 0-1.}
  \item{just.draw}{logical; NOT FUNCTIONAL, kept just for compatibility sake.}
  \item{...}{additional parameters to the underlying plotting function(s). See Details.
  }
}

\details{
    The function '\code{figOverplotDiagram}' can be employed in two ways. 
    
    If quoted names of two datasets in memory are provided 
    ('\code{bg.dataset}' and '\code{overplot.dataset}'), 
    a new plot is created, whereby the background dataset is plotted using either the function 
    '\code{plotDiagram}' (for stand-alone plots) or '\code{plateExtract}' (for one of diagrams
    extracted from a plate). 
    
    If only a single name of dataset is given, then the data are overplotted onto the current 
    (preexisting) diagram. 
    
    Optional plotting parameters '\code{pch}', '\code{col}', '\code{cex}', '\code{type}', 
    '\code{lwd}' and '\code{lty}' can be defined for the overplotted (foreground) dataset. 
    
    Argument '\code{...}' can supply additional parameters to the original plotting functions 
    (e.g., '\link{TAS}') invoked by '\link{plotDiagram}' or  '\link{plateExtract}'. 
}

\section{Warning}{
    This function serves to add extra components/annotations 
    immediately before the graph (a spiderplot, simple binary or ternary 
    plot) is printed or exported. 
    Note that the points for the overplotted dataset are not part of the template, 
    and thus will vanish upon redrawing, zooming \ldots.
}

\value{
None.
}

\seealso{
  \link{figOverplot}  \link{figAddReservoirs}  \link{overplotDataset}
  
  \link{underplotDataset} \link{plotDiagram}  \link{plateExtract}
  
  \link{figaro}  \link{par}
}

\examples{
data(sazava)
accessVar("sazava")
    
data(blatna) 
accessVar("blatna")
setCex(2)
pokeDataset("blatna", overwrite.warn=FALSE)
    
## Two datasets
# stand alone plot
figOverplotDiagram("sazava","blatna","DebonBA",pch=15,col="darkred",
  cex=2,transp=0.5)

# plateExtract
figOverplotDiagram("sazava","blatna","PearceGranite",col="darkred",
  cex=2,transp=0.5,which=2) 

## Overplotting on existing plot - plotDiagram
peekDataset("blatna")
plotDiagram("DebonPQ",FALSE,TRUE)
figCex(2)
figRemove()
figOverplotDiagram("sazava",pch=15,col="darkred",cex=2,transp=0.6)
    
# Overplotting of existing plot - plateExtract
peekDataset("blatna")
plateExtract("PearceGranite",which=2)
figXlim(c(1,100))
figYlim(c(1,300))
figCex(2)
figOverplotDiagram("sazava",pch=15,col="darkred",cex=2,transp=0.6)
}


\keyword{aplot}
\keyword{color}
\keyword{iplot}
\concept{Plot}
\concept{Figaro}
\concept{Menu: Plot editing}

\author{
    Vojtech Janousek, \email{vojtech.janousek@geology.cz}
}
