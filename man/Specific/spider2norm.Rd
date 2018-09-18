\name{spider2norm}

\title{Spider plot(s): Selected samples, double normalized}
\alias{spider_double_norm.r}
\alias{spider2norm}


\description{
    Plots a double normalized spiderplot. Trace-element data are first normalized by the given standard,
    as usual (see \code{\link{spider}}). Then the normalized concentrations are multiplied by a factor 
    needed to adjust the normalized content of the selected element in each analysis to a desired value
    (such as unity). The goal is to eliminate effects of fractional crystallization 
    \emph{(Thompson et al. 1983, Pearce et al. 2005, Pearce and Stern 2006)}.
}

\usage{
spider2norm(rock=WR,norm=NULL,norm2=NULL,ymin=0,ymax=0,which=rep(TRUE,nrow(rock)),
    legend=FALSE,pch=labels$Symbol,col=labels$Colour,plot=TRUE,join=TRUE,
    shaded.col="gray",density=-1,angle=0,xaxs="r",fill.col=FALSE,field=FALSE,
    add=FALSE,...)
}

\arguments{
\item{rock}{a numeric matrix; the whole-rock data from which will be filtered out those to be normalized.}
\item{norm}{a character string specifying the model.}
\item{norm2}{name of the variable for the second normalization.}
\item{ymin, ymax}{y range of the diagram.}
\item{which}{specification of the samples to be plotted.}
\item{legend}{logical; if TRUE, room for legend is reserved.}
\item{pch}{a vector specifying the plotting symbols.}
\item{col}{a numeric vector; colour of the plotting symbols and connecting lines.}
\item{plot}{logical; if set to FALSE, individual patterns are not plotted.}
\item{join}{logical; if TRUE, the NAs are extrapolated so that the patterns are unbroken.}
\item{shaded.col}{numeric: colour for the cross-hatched fill.}
\item{density}{numeric: density of the fill pattern (fraction of the whole plotting range).}
\item{angle}{numeric: angle of the fill pattern (in degrees).}
\item{xaxs}{style of the xaxis: see '\code{help(par)} for details.}
\item{fill.col}{colour for solid fill}
\item{field}{logical; if TRUE, a shaded field denoting the overall data span is plotted}
\item{add}{logical; if TRUE, a new plot is started (otherwise overplot).}
\item{...}{further graphical parameters: see '\code{help(par)} for details.}
}

\details{
    The  parameter '\emph{norm}' is an optional search pattern to 
    query  the available normalizing model names. It can contain a 
    substring or even a regular expression. For choosing the correct 
    normalization values serves the auxiliary function 
    \code{\link{selectNorm}}.The function fails 
    if no matches are found or the search is ambiguous. 
    See \code{\link{selectNorm}}
    for details.

    The samples to be plotted can be selected based on combination of three 
    searching mechanisms (by sample name/label, range or a Boolean condition) - see 
    \code{\link{selectSamples}} for details. 

    Then the user is prompted whether to use the currently assigned plotting symbols. 
    If desired so, the symbols and colours can be specified in a simple spreadsheet-
    like interface. 

    Likewise the scale of the y axis can be specified interactively.
}

\value{
    \item{results}{numeric matrix with normalized concentrations}.
}

\references{
Pearce J A, Stern R J (2006) Origin of back-arc basin magmas: 
Trace element and isotope perspectives. Back-Arc Spreading Systems: 
Geological, Biological, Chemical, and Physical Interactions. 
Geophysical Monograph Series 166. American Geophysical Union, pp 63-86

Pearce J A, Stern R J, Bloomer S H, Fryer P (2005) 
Geochemical mapping of the Mariana arc-basin system: 
implications for the nature and distribution of subduction components. 
Geochem Geophys Geosyst 6: doi: 10.1029/2004GC000895
\href{http://dx.doi.org/10.1029/2004GC000895}{doi: 10.1029/2004GC000895} 

Thompson R N, Morrison M A, Dickin A P, Hendry G L (1983) 
Continental flood basalts... Arachnids rule OK? 
In: Hawkesworth C J, Norry M J (eds) Continental Basalts and Mantle Xenoliths. 
Shiva, Nantwich, pp 158-185
}

\examples{
ee<-spider2norm(WR,"Boynton","Yb",0.1,1000,pch="*",col="red",cex=2) 
# the ee<- construction redirects the textual output

ee<-spider2norm(WR,"Boynton","Yb",field=TRUE,density=0.05,angle=60,col="red",0.1,1000)

ee<-spider2norm(WR,"Boynton","Yb",field=TRUE,fill.col=TRUE,shaded.col="khaki",0.1,1000)

# Shade the background field portraying the overall variation 
ee<-spider2norm(WR,"Boynton","Lu",0.1,1000,pch=labels$Symbol,col=labels$Colour,cex=labels$Size)
ee<-spider2norm(WR,"Boynton","Lu",field=TRUE,density=0.02,angle=45,col="gray",add=TRUE)

# Shade the background field portraying the overall variation 
ee<-spider2norm(WR,"Boynton","Lu",0.1,1000,pch=labels$Symbol,col=labels$Colour,cex=labels$Size)
ee<-spider2norm(WR,"Boynton","Lu",field=TRUE,fill.col=TRUE,shaded.col="gray",add=TRUE)

# Possible styles for x axis
multiplePerPage(8,nrow=2,ncol=4,"Possible x axis styles", dummy=FALSE)
ee<-spider2norm(WR, "Boynton","Yb", 0.1, 1000, pch=labels$Symbol, 
    col=labels$Colour, cex=labels$Size, offset=F, xrotate=F, centered=F,new=F)
ee<-spider2norm(WR, "Boynton","Yb", 0.1, 1000, pch=labels$Symbol, 
    col=labels$Colour, cex=labels$Size, offset=F, xrotate=T, centered=F,new=F)
ee<-spider2norm(WR, "Boynton","Yb", 0.1, 1000, pch=labels$Symbol, 
    col=labels$Colour, cex=labels$Size, offset=F, xrotate=F, centered=T,new=F)
ee<-spider2norm(WR, "Boynton","Yb", 0.1, 1000, pch=labels$Symbol, 
    col=labels$Colour, cex=labels$Size, offset=F, xrotate=T, centered=T,new=F)
ee<-spider2norm(WR, "Boynton","Yb", 0.1, 1000, pch=labels$Symbol, 
    col=labels$Colour, cex=labels$Size, offset=T, xrotate=F, centered=F,new=F)
ee<-spider2norm(WR, "Boynton","Yb", 0.1, 1000, pch=labels$Symbol, 
    col=labels$Colour, cex=labels$Size, offset=T, xrotate=T, centered=F,new=F)
ee<-spider2norm(WR, "Boynton","Yb", 0.1, 1000, pch=labels$Symbol, 
    col=labels$Colour, cex=labels$Size, offset=T, xrotate=F, centered=T,new=F)
ee<-spider2norm(WR, "Boynton","Yb", 0.1, 1000, pch=labels$Symbol, 
    col=labels$Colour, cex=labels$Size, offset=T, xrotate=T, centered=T,new=F)

spider2norm(WR,"Boynton","Yb",plot=FALSE) # Calculation only
}


\seealso{
For the syntax of the setup file with normalizing values and adding new 
normalization schemes \cr
see \code{\link{selectNorm}}; for further variants of spiderplots, see
\code{\link{spider}}, \code{\link{spiderByGroupPatterns}} 
and \code{\link{spiderByGroupFields}}.
}

\author{
Vojtech Janousek, \email{vojtech.janousek@geology.cz}
}

\keyword{hplot}
\concept{Plot}
\concept{Spiderplot}
\concept{Menu: Plots}

