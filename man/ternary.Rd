\name{ternary}
\title{Ternary plot}
\alias{triplot}
\alias{triplotadd}
\alias{ternary}

\description{
These functions plot/add data to a ternary plot.
}

\usage{
ternary(x = NULL, y = NULL, z = NULL, samples = rownames(WR), 
    new = TRUE, grid = FALSE, ticks = TRUE, ...)

triplot(aa, bb, cc, alab, blab, clab, title = "", grid.int = 0, 
tick.int = 0, label.axes = FALSE, line = FALSE, 
pch = labels[names(aa), "Symbol"], 
col = labels[names(aa), "Colour"],
cex = labels[names(aa),"Size"],
identify = getOption("gcd.ident"),
new = TRUE,...)

triplotadd(aa, bb, cc,
pch=labels[names(aa),"Symbol"],
col=labels[names(aa),"Colour"],
cex = labels[names(aa),"Size"], 
labs=NULL, identify = FALSE, lines = FALSE, lty = "solid", type="p")
}

\arguments{
\item{x}{character; specification of the plotting variable for the 
    bottom left apex (formulae OK).}
\item{y}{character; specification of the plotting variable for the 
    top apex (formulae OK).}
\item{z}{character; specification of the plotting variables for the 
    bottom right apex (formulae OK).}
\item{grid}{logical; should be grid plotted?}
\item{ticks}{logical; should be ticks plotted?}
\item{samples}{character or numeric vector; specification of the samples to be plotted.}
\item{new}{logical; should be opened a new plotting window?}
\item{...}{Further parameters to the functions '\code{ternary}' and '\code{triplot}'.}    
\item{aa}{a numerical vector, bottom left apex.}
\item{bb}{a numerical vector, top apex.}
\item{cc}{a numerical vector, bottom right apex.}
\item{alab,blab,clab}{labels for the apices.}
\item{title}{title for the whole diagram.}
\item{grid.int}{interval of grid lines (0-1); if set to zero (default value), no grid is drawn.}
\item{tick.int}{interval of ticks on axes (0-1); if set to zero (default value), no ticks are drawn.}
\item{label.axes}{logical; if set to TRUE, axes are labeled by percentages of the components.}
\item{line, lines}{logical; if set to TRUE, lines are drawn instead of plotting points.}
\item{lty}{line type.}
\item{pch}{plotting symbols.}
\item{col}{plotting colours.}
\item{cex}{relative size of plotting symbols.}
\item{identify}{logical; should be samples identified?}
\item{labs}{character; optional text to label the points.}
\item{type}{character; plot type; see \link{plot.default}.}
}

\details{
The function '\code{ternary}' is the user interface to '\code{triplot}'. The 
latter sets up the axes, labels the apices, plots the data and, if desired, 
enables the user to identify the data points interactively. 

If '\code{new=TRUE}', new plot window is opened.

    \if{html}{\figure{ternary.png}} 
    \if{latex}{\figure{ternary.png}{options: width=4.5in}}
    
The values for '\code{label.axes}' are chosen according to 
'\code{tick.int}' or '\code{grid.int}'; 
if these are not available, labels are drawn by 10\%.

'\code{triplotadd} 'adds data points/lines to pre-existing ternary plot. 

The variables to be plotted are selected using the function '\code{\link{selectColumnLabel}}.

In the specification of the apices can be used also arithmetic expressions, 
see \code{\link{calcCore}} for the correct syntax. 

The functions are Figaro-compatible.
}

\value{
A numeric matrix with coordinates of the data points recast to a sum of 1.
}

\examples{
ternary("Ba","Rb*10","Sr",col="red",pch="+")

ternary("SiO2/10","2*FeOt","K2O*5",samples=1:10,grid=TRUE)   
    
triplot(WR[,"SiO2"]/10,WR[,"Na2O"]+WR[,"K2O"],WR[,"MgO"],"SiO2","A","MgO",
tick.int=0.1)

triplot(WR[,"Rb"]*10,WR[,"Sr"],WR[,"Ba"],"Rb","Sr","Ba",tick.int=0.05,
grid.int=0.1,pch="+",col="darkblue",label.axes=TRUE)
}

\seealso{
  \link{plot}
}

\keyword{hplot}
\concept{Plot}
\concept{Menu: Plots}

\author{
    Jakub Smid \email{smid@prfdec.natur.cuni.cz} 
    & Vojtech Janousek, \email{vojtech.janousek@geology.cz}
}
