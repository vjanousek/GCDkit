\name{figAdd}
\title{Plot editing: Add}
\alias{figTicks}
\alias{figGrid}
\alias{figLegend}
\alias{figAddReservoirs}
\alias{figAdd}
\alias{figAddFit}
\alias{figAddCurve}
\alias{figAddText}
\alias{figAddArrow}
\alias{figAddBox}

\description{
These functions enable adding new components to Figaro-compatible plots. 
}

\usage{
figTicks(major=-0.5, minor=0.25, xmjr=NULL, xmin=NULL, ymjr=NULL, ymin=NULL)

figGrid(x.int=NULL, y.int=NULL, lty="dotted", col="gray30",GUI=FALSE)    
    
figLegend()

figAddReservoirs(autoscale=FALSE, var.name=NULL, sample.names=NULL,
reserv.condition=NULL, labs=NULL, pch="*", col="darkred", cex=1, type="p",...)

figAddText()

figAddArrow()

figAddBox()

figAddFit()

figAddCurve(equation=NULL)
}

\arguments{
  \item{major}{length of the major tick marks.}
  \item{minor}{length of the minor tick marks.}
  \item{xmjr, ymjr}{intervals for the major tick marks.}
  \item{xmin, ymin}{intervals for the minor tick marks.}
  \item{x.int}{intervals for the grid, x axis component.}
  \item{y.int}{intervals for the grid, y axis component.}
  \item{GUI}{logical; is the function called from GUI?}
  \item{lty}{grid line type.}
  \item{col}{plotting colour.}
  \item{autoscale}{logical; should be the scaling changed so that all the plotted data fit in?}
  \item{var.name}{text; either '\code{reservoirs.data}', '\code{idealmins.data}'or a name of a 
       global variable. See Details.}
  \item{sample.names}{character vector; names of reservoirs, ideal minerals or samples to be plotted.}
  \item{reserv.condition}{text; regular expression specifying reservoirs compositions of which are to be plotted.}  
  \item{labs}{text; optional abbreviated labels for the individual reservoirs}
  \item{pch}{plotting symbol.}
  \item{cex}{numeric; relative size of the plotting symbol.}
  \item{type}{character; plot type; see \link{plot.default}.}
  \item{...}{additional parameters to the plotting function. See \link{figOverplot}.}
  \item{equation}{text; equation expressed as a function of x; see \link{curve}.}   
}


\details{
    '\code{figTicks}' adds major and minor tick marks for the x and y axes. Their 
    length is specified as a fraction of the height of a line of text. Negative 
    numbers imply outward and positive inward pointing ticks. The user is prompted for
    four numbers separated by commas, xmjr, xmin, ymjr, ymin. These specify the intervals 
    of major and minor ticks for x and y axes, respectively. 
    Not implemented to logarithmic plots and spiderplots yet. 

    '\code{figGrid}' adds grid lines for x and/or y axes.    
    
    '\code{figLegend}' adds legend(s) on specified location.

    '\code{figAddReservoirs}' overplots compositions of selected geochemical reservoirs
    (taken from the file '\code{reservoirs.data}', see \code{\link{selectNorm}} 
    for the file structure as well as relevant references) or ideal minerals 
    (from the file '\code{idealmins.data}'). 
    Alternatively, if the name of a numeric matrix or dataframe in the global 
    environment is provided via the argument '\code{var.name}', the selection of data 
    from this object is used (see Examples). The selection is specified by either 
    '\code{sample.names}' or by '\code{reserv.condition}' parameters.  
    Optional argument '\code{labs}' can specify alternative,
    perhaps abbreviated textual labels to the points plotted. 
    
    Please note that the function '\code{figAddReservoirs}' is available 
    so far for simple spiderplots, binary and ternary plots only. Technically, 
    the function invokes '\link{figOverplot}' setting
    \code{just.draw=FALSE}, and thus the overplotted dataset is added permanently.  
    If \code{just.draw=FALSE}, the points for the reference dataset do not 
    become a part of the template, and thus will vanish upon redrawing, zooming 
    \ldots. See Examples.    

    '\code{figAddText}' adds text on specified location. The parameters are the 
    text style ('\code{n}' = normal, '\code{b}' = bold, '\code{i}' = italic and 
    '\code{bi}' = bold italic), colour and relative size.

    '\code{figAddArrow}' adds arrow on specified location. The parameters are 
    colour and line style ('\code{solid}', '\code{dashed}', '\code{dotted}' and 
    '\code{dotdash}'). 

    '\code{figAddBox}' adds box on specified location (click bottom left and then 
    top right corner). 

    '\code{figAddFit}' adds either a single least-squares fit to all data, or 
    several fit lines, for each of the groups separately. The parameters are colour 
    and line style ('\code{solid}', '\code{dashed}', '\code{dotted}' and 
    '\code{dotdash}'). The equation of each fit line is plotted at the user-defined
    location.

    '\code{figAddCurve}' adds a curve, specified as a function of variable '\code{x}'. 
    The parameters are colour and line style ('\code{solid}', '\code{dashed}', 
    '\code{dotted}' and '\code{dotdash}'). 

    The colours can be specified both by their code (see table under menu 
    '\code{Data handling|Show 
    available colours}') or R name (see Examples).

    The additional two menu items, available for binary and ternary plots, allow
    adding contours or convex hulls outlining individual groups of data. See 
    \link{contourGroups} and \link{chullGroups}.
}

\value{
    For '\code{figAddReservoirs}', a numeric matrix with the overplotted analyses 
    from the reference dataset.
}

\section{Warning}{
    Most of these functions serve to adding some extra components/annotations immediately
    before the graph is printed/exported. Note that, except for '\code{figAddReservoirs}', 
    all user-defined components  added via '\code{Plot editing: Add}' 
    will be lost upon redrawing, zooming \ldots.
}

\seealso{
  \link{par}  \link{showColours} \link{colours}
  \link{figaro}  \link{selectNorm} 
  
  \link{contourGroups}  \link{chullGroups}
  
  
  \link{figOverplot}  \link{figOverplotDiagram} \link{overplotDataset} \link{curve}
}

\examples{
## figTicks and figGrid
data(blatna)
accessVar("blatna")
setCex(1.5)

binary("Zr/Nb","Ba/La")
figTicks(major=-0.5, minor=0.25,10,1,10,1)
figGrid(,5,col="darkblue") # just y axis (second parameter)

figRedraw()
figGrid(2,5,col="darkblue")

## figAddReservoirs
data(blatna)
accessVar("blatna")
setCex(1.5)

# binary    
binary("Zr/Nb","Ba/La")
# Sun & McDonough mantle reservoirs, Taylor & McLennan 1995 Upper and Lower Crust
reserv<-c("(MORB|OIB) McDonough","Upper Crust Taylor 1995","Lower Crust Taylor 1995") 
reserv.names<-c("NMORB","EMORB","OIB","UCC","LCC") 
figAddReservoirs(TRUE,"reservoirs.data",reserv.condition=reserv,labs=reserv.names)


# ternary    
ternary("SiO2/10","MgO","FeOt")
figAddReservoirs(var.name="idealmins.data",sample.names=c("Or","Bt","Ph")) 
    
# spider
spider(WR,"NMORB..Sun",field=TRUE,colour="gray",field.colour=TRUE,ymin=0.1,ymax=100)
figAddReservoirs(var.name="reservoirs.data",reserv.condition="Continental Crust",
    autoscale=TRUE,col=c("red","black","darkblue"),pch=1:3)
    
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
figAddReservoirs(var.name="my.trend",pch="+",col="blue",autoscale=TRUE,type="o",
  labs=rownames(my.trend))
figXlim(c(10,500))

# But this is controlled by the argument just.draw    
binary("Rb","Sr",log="xy")
figAddReservoirs(var.name="my.trend",pch="+",col="red",autoscale=TRUE,type="o",
  labs=rownames(my.trend),just.draw=TRUE)
figRedraw() 
}

\keyword{aplot}
\keyword{color}
\keyword{iplot}
\concept{Plot}
\concept{Figaro}
\concept{Menu: Plot editing}

\author{
Colin M. Farrow, \email{colinfarrow537@gmail.com} 

Vojtech Janousek, \email{vojtech.janousek@geology.cz}
}
