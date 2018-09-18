\name{figOverplot}
\alias{figOverplot}
\title{Overplotting data onto pre-existing binary, ternary or spider plots}

\description{
    This function allows overplotting new data points onto Figaro-compatible binary or ternary plots, 
    or patterns onto spiderplots. 
    It is most useful in adding selected data from typical geochemical 
    reservoirs (e.g., Upper Continental Crust, MORB \ldots), ideal mineral compositions, 
    results of petrogenetic modelling or just another dataset used for comparison 
    (any of these will be henceforth referred to as a reference dataset).  
}

\usage{
figOverplot(var.name, mat=NULL, sample.names=NULL, condition=NULL, 
    labs=NULL, autoscale=FALSE, pch="*", col="darkred", cex=1, 
    type="p", just.draw = FALSE,overplotDataset = FALSE,...)
}

\arguments{
  \item{var.name}{either '\code{reservoirs.data}', '\code{idealmins.data}' or a quoted name of a global variable.}
  \item{mat}{matrix with data for all reservoirs available for overplotting. Meant mainly for internal use of the 
       \emph{GCDkit} system.}
  \item{sample.names}{character vector; list of names of desired reservoirs, ideal minerals or samples 
       in the reference dataset to be overplotted.}
  \item{condition}{text; regular expression specifying names of desired reservoirs, ideal minerals or samples 
        in the reference dataset.}  
  \item{labs}{text; optional (typically abbreviated) labels for the overplotted data from the reference dataset.}
  \item{autoscale}{logical; should be the scaling changed so that all the plotted analyses fit in?}
  \item{pch}{plotting symbol(s) for the reference dataset.}
  \item{col}{plotting colour(s) for the reference dataset.}
  \item{cex}{numeric; relative size of the plotting symbol(s) for the reference dataset.}
  \item{type}{character; plot type; see \link{plot.default}. For obvious reasons, not implemented for spiderplots.}
  \item{just.draw}{logical; if \code{FALSE}, the overplotted bit is added permanently, 
       i.e. the Figaro template is also affected.}
  \item{overplotDataset}{logical; for internal use by the system only.}
  \item{...}{additional parameters to the underlying plotting function(s). See Details. }
}

\details{ 
    If called directly, the function is employed to overplot data from a reference dataset, 
    either real-world data or a numeric matrix spanning, for instance, from petrogenetic 
    modelling. The data originate from a two-dimensional variable 
    in the global environment, whose name is provided via the obligatory argument
    '\code{var.name}'. 
    
    Argument '\code{mat}' is meant for internal use by the system and does not need to 
    be specified by the user as the data frame/matrix \code{mat} is generated automatically 
    by the function '\code{figOverplot}'. 
    
    In both cases, the selection from the numeric matrix or dataframe '\code{mat}' is 
    based on a list of desired '\code{sample.names}' or on a regular expression yielding 
    their subset ('\code{condition}'). Of course, from this selection, only analyses 
    with  data sufficient to be plotted on the current diagram are used.
      
    If neither '\code{sample.names}' nor '\code{condition}' is provided, all samples are shown.   

    For plotting are used functions '\link{points}', '\link{triplotadd}' and '\link{spider}' 
    for binary plots, ternary plots and spiderplots, respectively. 
    Argument '\code{...}' can supply additional parameters to these low-level plotting 
    functions. 
    
    Optional parameter '\code{labs}' can specify alternative, typically abbreviated textual 
    labels to the points plotted.
    
    Logical argument '\code{autoscale}' determines whether the plot should be rescaled 
    to accommodate both the original data points and the reference dataset. 
    Clearly, it does not make sense for a ternary plot. 
    
    By default, the overplotted information is added permanently but this behaviour is 
    controlled by the argument \code{just.draw}.
}

\value{
    A numeric matrix with the overplotted analyses from the reference dataset.
}

\section{Note}{
    Within the \emph{GCDkit} system, this function is invoked by '\link{figAddReservoirs}' 
    to overplot selected compositions from typical geochemical reservoirs
    (system file '\code{reservoirs.data}') or chemistries of ideal minerals 
    (system file '\code{idealmins.data}'). 
}

\section{Warning}{
    If \code{just.draw=FALSE}, the points for the reference dataset do not 
    become a part of the template, and thus will vanish upon redrawing, zooming 
    \ldots. See Examples.
}

\seealso{
  \link{figAddReservoirs}  \link{plateAddReservoirs}  
  
  \link{figOverplotDiagram}  \link{overplotDataset}
  
  \link{points} \link{triplotadd} \link{spider}
  
  \link{figaro}  \link{selectNorm}  \link{par}
}

\examples{
data(sazava)
accessVar("sazava")
setCex(1.5)
pokeDataset("sazava", overwrite.warn=FALSE)
    
# Calculate Rayleigh-type fractionation trend
ff<-seq(1,0.1,-0.1) # F, amount of melt left
x<-80*ff^(1.2-1)    # cL for three elements, arbitrary D of 1.2, 2.0 and 1.3
y<-550*ff^(2.0-1)
z<-1000*ff^(1.3-1)
my.trend<-cbind(x,y,z)
colnames(my.trend)<-c("Rb","Sr","Ba")
rownames(my.trend)<-ff

# By default, the overplotted information is added permanently    
binary("Rb","Sr",log="xy")
figOverplot(var.name="my.trend",pch="+",col="blue",autoscale=TRUE,type="o",
  labs=rownames(my.trend))
figXlim(c(30,100))

# But this is controlled by the argument just.draw    
binary("Rb","Sr",log="xy")
figOverplot(var.name="my.trend",pch="+",col="darkred",autoscale=TRUE,type="o",
  labs=rownames(my.trend),just.draw=TRUE)
# Any function redrawing the plotting window will wipe the added trend out
figXlim(c(30,100))
    
ternary("10*Rb","2*Sr","Ba/2")
figOverplot(var.name="my.trend",pch="+",col="blue",type="o",
  labs=rownames(my.trend))
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
