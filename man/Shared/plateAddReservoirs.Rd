\name{plateAddReservoirs}
\title{Plate editing: plateAddReservoirs}
\alias{plateAddReservoirs}

\description{
    This function enables adding selected data from typical geochemical 
    reservoirs (e.g., Upper Continental Crust, MORB \ldots), ideal mineral compositions, 
    results of petrogenetic modelling or just another dataset used for comparison 
    to a plate of Figaro-compatible plots. 
}

\usage{
plateAddReservoirs(autoscale=FALSE, var.name="reservoirs.data", sample.names=NULL,
    reserv.condition=NULL, labs=NULL, pch="*", col="darkblue", cex=1, type="p",
    just.draw=FALSE,...)
}

\arguments{
  \item{autoscale}{logical; should be the scaling changed so that all the plotted data fit in?}
  \item{var.name}{text; either '\code{reservoirs.data}', '\code{idealmins.data}' or a name of a global 
       variable. See Details.}
  \item{sample.names}{character vector; names of reservoirs, ideal minerals or samples to be plotted.}
  \item{reserv.condition}{text; regular expression specifying names of reservoirs, ideal minerals or 
       samples to be plotted.}  
  \item{labs}{text; optional labels for the individual reservoirs.}
  \item{pch}{plotting symbols.}
  \item{col}{plotting colours.}
  \item{cex}{numeric; relative size of the plotting symbols.}
  \item{type}{character; plot type; see \link{plot.default}.}
  \item{just.draw}{logical; if \code{FALSE}, the overplotted bit is added permanently, 
       i.e. the Figaro template is also affected.}
  \item{...}{additional parameters to the plotting function. See \link{figOverplot}.}   
}



\details{
    The function '\code{plateAddReservoirs}' overplots compositions of selected geochemical reservoirs
    (from the file '\code{reservoirs.data}', see \code{\link{selectNorm}} for the file
    structure as well as relevant references) or ideal minerals 
    (from the file '\code{idealmins.data}') onto a current plate. 

    Alternatively, if the name of a numeric matrix or dataframe in the global environment is 
    provided via the argument '\code{var.name}', data 
    from this object are used (see Examples). The selection of samples is governed  
    either by '\code{sample.names}' or by '\code{reserv.condition}' parameters.
      
    Optional argument '\code{labs}' can provide alternative,
    perhaps abbreviated textual labels to the points plotted. 
    
    Please note that this function is so far available 
    for spiderplots, binary and ternary plots only and no special indexes, e.g.
    for Debon and Le Fort's plots, are calculated.
    
    By default, the overplotted information is added permanently but this behaviour is 
    controlled by the argument \code{just.draw}. 
}

\value{
    A list of numeric matrices with the overplotted analyses 
    from the reference dataset.
}

\section{Warning}{
    If \code{just.draw=FALSE}, the points for the reference dataset do not 
    become a part of the template, and thus will vanish upon redrawing, zooming 
    \ldots. See Examples.
}

\seealso{
  \link{figAddReservoirs}  \link{selectNorm}
  \link{overplotDataset}
  \link{figOverplot} \link{figOverplotDiagram}
}

\examples{
data(blatna)
accessVar("blatna")

# Simple binary plots    
multiplePerPage(2,ncol=2,nrow=1,title="Testing plateAddReservoirs",dummy=FALSE)
screen(1)
binary("Ba","Sr",new=FALSE,log="xy")
screen(2)
binary("Ba","Rb/Sr",new=FALSE)
plateCex(2)
plateCexLab(1.5)

# Temporary overplotting with the selected reservoirs
# Sun & McDonough 1989 mantle reservoirs, Taylor & McLennan 1995 Upper/Lower Crust
reserv<-c("(MORB|EMORB|OIB) McDonough","Upper Crust Taylor 1995","Lower Crust Taylor 1995") 
reserv.names<-c("NMORB","EMORB","OIB","UCC","LCC") 

plateAddReservoirs(TRUE,"reservoirs.data",reserv.condition=reserv,
  labs=reserv.names,cex=1.2,col="darkblue",just.draw=TRUE) 
plateRedraw()

# Permanent overplotting with a modelled trend
# Calculate Rayleigh-type fractionation trend and store in a global variable
ff<-seq(1,0.1,-0.1) # F, amount of melt left
x<-80*ff^(1.2-1)    # cL for three elements, arbitrary D of 1.2, 2.0 and 1.3
y<-550*ff^(2.0-1)
z<-1000*ff^(1.3-1)
my.trend<-cbind(x,y,z)
colnames(my.trend)<-c("Rb","Sr","Ba")
rownames(my.trend)<-ff

plateAddReservoirs(TRUE,var.name="my.trend",type="o",col="darkgreen",just.draw=FALSE)
plateRedraw()

# Spider plots  
ee<-spider(WR,"NMORB immobile",0.1,1000,pch=1:14,col=1:14,legend=TRUE)
groupsByLabel("Suite")
figMulti(nrow=1,ncol=3,plot.symb=TRUE)

reserv<-c("OIB .* McDonough","Lower Crust Taylor 1995") 
reserv.names<-c("OIB","LCC") 
plateAddReservoirs(FALSE,"reservoirs.data",reserv.condition=reserv,
  labs=reserv.names,cex=1.2,col="darkgreen")
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
