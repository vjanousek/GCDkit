\name{spider}

\title{Spider plot(s): Selected samples}
\alias{spiderplot.r}
\alias{spider}
\alias{spider.individual}
\alias{spider.contour}

\description{
Normalization of trace-element data by the given standard and spiderplot plotting.
}

\usage{
spider.individual(new=TRUE)

spider.contour(chondrit = selectNorm(),what=NULL,
colour.palette = "heat.colors", ymin = 0, ymax = 0,
cex = 1,join = TRUE,pch = 15,
main = "",sub = "",offset = TRUE,centered = FALSE,
xrotate = FALSE, xaxs = "r", new = TRUE, legend = TRUE)

spider(rock, chondrit = selectNorm(), ymin = 0, 
ymax = 0, cex = NULL, plot = TRUE, join = TRUE, 
field = FALSE, legend = FALSE, add = FALSE, 
pch = NULL, col = NULL, shaded.col = "gray", 
density = 0.02, angle = 0, main = "", sub = "", 
offset = FALSE, centered = FALSE, xrotate = FALSE,
xaxs = "r", fill.col = TRUE, log = "y", new = TRUE, ...)
}

\arguments{
\item{new}{logical; if true, new plotting window is opened.}
\item{chondrit}{a numeric matrix with one row; the normalizing values.}
\item{what}{variable name or formula.}
\item{colour.palette}{variable name or formula.}
\item{rock}{a numeric matrix; the whole-rock data from which will be filtered out those to be normalized.}
\item{ymin, ymax}{y range of the diagram.}
\item{cex}{magnification of the plotting symbols.}
\item{plot}{logical; if set to FALSE, individual patterns are not plotted.}
\item{join}{logical; if TRUE, the NAs are extrapolated so that the patterns are unbroken.}
\item{field}{logical; if TRUE, a shaded field denoting the overall data span is plotted}
\item{legend}{logical; if TRUE, room for legend is reserved.}
\item{add}{logical; if FALSE, a new plot is started (otherwise overplot).}
\item{pch}{a vector specifying the plotting symbols.}
\item{col}{a numeric vector; colour of the plotting symbols and connecting lines.}
\item{fill.col}{logical; should be the field of overall variability filled by solid colour?}
\item{shaded.col}{numeric: colour for the cross-hatched or solid fill.}
\item{density}{numeric: density of the fill pattern (fraction of the whole plotting range).}
\item{angle}{numeric: angle of the fill pattern (in degrees).}
\item{main}{character: the main title for the plot.}
\item{sub}{character: the subtitle for the plot.}
\item{xrotate}{logical; shall be the element names on x axis rotated?}
\item{offset}{logical; shall be the  names for odd and even elements shifted relative to each other?}
\item{centered}{logical; shall be the element names on x axis plotted in between tick marks?}
\item{xaxs}{style of the xaxis: see '\code{help(par)} for details.}
\item{log}{which of the axes should be logarithmic?}
\item{...}{further graphical parameters: see '\code{help(par)} for details.}
}

\note{
    If not specified, the parameters \code{pch}, \code{col}  and \code{cex} are set up by default to 0 (circle),
    black and 1 numeric matrix, respectively. 
    The only exception occurs when the plotting object is \code{WR}, when the missing 
    plotting parameters are sought in the dataframe \code{labels}, i.e. among the standard plotting properties. 
}

\details{
    This is a quite flexible function, a true \emph{Mother of All Spiderplots}, that 
    can be used in a number of ways. It is employed by functions of the GCDkit system 
    for normalization and plotting individual patterns for selected samples 
    ('\code{spiderplot.r}') or each of the groups \cr
    ('\code{spider by group individual.r}'). In '\code{spiderplot.r}' is stored a 
    user interface to '\code{spider}' for 
    plotting individual patterns. 
    \if{html}{\figure{spider3.png}} 
    \if{latex}{\figure{spider3.png}{options: width=5.5in}} 

    Function '\code{spider}' can also serve for plotting the overall compositional ranges 
    (shown as cross-hatched fields or, optionally, semitransparent filled polygons) 
    in a manner similar to function 
    '\code{spider by group.r}'.
    \if{html}{\figure{spider4.png}} 
    \if{latex}{\figure{spider4.png}{options: width=5.5in}} 

    In '\code{spiderplot_contour.r}' is stored a user interface to '\code{spider}' for 
    plotting individual patterns, in which the plotting symbols is uniform and colour
    reflects distribution of an independent variable, such as silica contents.
    The variable (or formula) can be specified using the parameter '\code{what}', the
    colour scheme by '\code{colour.palette}'. The legal colour schemes are: 
    '\code{"grays","reds","blues"},\cr
    \code{"greens","cyans","violets","yellows","cm.colors","heat.colors","terrain.colors"},\cr
    \code{"topo.colors","rainbow", "jet.colors"}'. 
    \if{html}{\figure{spider1.png}} 
    \if{latex}{\figure{spider1.png}{options: width=5.5in}} 

    The samples to be plotted can be selected based on combination of three 
    searching mechanisms (by sample name/label, range or a Boolean condition) - see 
    \code{\link{selectSamples}} for details. 
    For choosing the correct normalization values serves the auxiliary function 
    \code{\link{selectNorm}}.
    Then the user is prompted whether to use the currently assigned plotting symbols. 
    If desired so, the symbols and colours can be specified in a simple spreadsheet-
    like interface. 
    Likewise the scale of the y axis can be specified.
    The exact appearance of the labels to the x axis can be fine tuned by the arguments 
    '\code{rotate.xlab}', '\code{offset}' and '\code{centered}'.  See examples. 
    \if{html}{\figure{spider9.png}} 
    \if{latex}{\figure{spider9.png}{options: width=5.5in}}

    If '\code{plot=FALSE}', not plotting is done, and only the normalized 
    values are returned. 
}

\value{
    \item{results}{numeric matrix with normalized concentrations.}
}

\seealso{
    For the syntax of the setup file with normalizing values and adding new 
    normalization schemes \cr
    see \code{\link{selectNorm}}; for further applications of 
    '\code{spider}' see \code{\link{spider2norm}}, 
    \code{\link{spiderByGroupPatterns}} 
    and \code{\link{spiderByGroupFields}}.
}


\examples{
ee<-spider.contour("Boynton","SiO2","reds",pch="*",cex=2,ymin=0.01,ymax=1000)

ee<-spider(WR,"Boynton",0.1,1000,pch="*",col="red",cex=2) 
# the ee<- construction redirects the textual output

ee<-spider(WR[1:14,],"Boynton",1,500,pch=1:14,col=1:14,legend=TRUE)

ee<-spider(WR,"Boynton",field=T,density=0.02,angle=60,col="darkred",fill.col=F,0.1,1000)

ee<-spider(WR,"Boynton",field=TRUE,fill.col=TRUE,shaded.col="khaki",0.1,1000)

# Shade the background field portraying the overall variation 
    
# Shade the background field portraying the overall variation 
ee<-spider(WR,"Boynton",0.1,1000,pch=labels$Symbol,col=labels$Colour,cex=labels$Size)
ee<-spider(WR,"Boynton",field=TRUE,fill.col=TRUE,shaded.col="gray",add=TRUE)

ee<-spider(WR,"Boynton",0.1,1000,pch=labels$Symbol,col=labels$Colour,cex=labels$Size)
ee<-spider(WR,"Boynton",field=TRUE,density=0.02,angle=45,col="gray",fill.col=FALSE,add=TRUE)

# Custom normalization scheme
chon<-c(0.4,4,50,0.8,0.7,10,35,9,340,9,70,8.0)
chon<-matrix(chon,nrow=1)
colnames(chon)<-c("K2O","Rb","Ba","Th","Ta","Nb","Ce","Hf","Zr","Sm","Y","Yb")
rownames(chon)<-"ORG (Pearce et al. 1984)"
spider(WR,chon,ymin=0.01,col="navy",ymax=1000)


# Possible styles for x axis
multiplePerPage(8,nrow=2,ncol=4,"Possible x axis styles", dummy=FALSE)
ee<-spider(WR, "Boynton", 0.1, 1000, pch=labels$Symbol, 
    col=labels$Colour, cex=labels$Size, offset=F, xrotate=F, centered=F, 
    main="offset=F, xrotate=F, centered=F",new=F)
ee<-spider(WR, "Boynton", 0.1, 1000, pch=labels$Symbol, 
    col=labels$Colour, cex=labels$Size, offset=F, xrotate=T, centered=F, 
    main="offset=F, xrotate=T, centered=F",new=F)
ee<-spider(WR, "Boynton", 0.1, 1000, pch=labels$Symbol, 
    col=labels$Colour, cex=labels$Size, offset=F, xrotate=F, centered=T, 
    main="offset=F, xrotate=F, centered=T",new=F)
ee<-spider(WR, "Boynton", 0.1, 1000, pch=labels$Symbol, 
    col=labels$Colour, cex=labels$Size, offset=F, xrotate=T, centered=T, 
    main="offset=F, xrotate=T, centered=T",new=F)
ee<-spider(WR, "Boynton", 0.1, 1000, pch=labels$Symbol, 
    col=labels$Colour, cex=labels$Size, offset=T, xrotate=F, centered=F, 
    main="offset=T, xrotate=F, centered=F",new=F)
ee<-spider(WR, "Boynton", 0.1, 1000, pch=labels$Symbol, 
    col=labels$Colour, cex=labels$Size, offset=T, xrotate=T, centered=F, 
    main="offset=T, xrotate=T, centered=F",new=F)
ee<-spider(WR, "Boynton", 0.1, 1000, pch=labels$Symbol, 
    col=labels$Colour, cex=labels$Size, offset=T, xrotate=F, centered=T, 
    main="offset=T, xrotate=F, centered=T",new=F)
ee<-spider(WR, "Boynton", 0.1, 1000, pch=labels$Symbol, 
    col=labels$Colour, cex=labels$Size, offset=T, xrotate=T, centered=T, 
    main="offset=T, xrotate=T, centered=T",new=F)

spider(WR,"Boynton",plot=FALSE) # Calculation only
}

\author{
Vojtech Janousek, \email{vojtech.janousek@geology.cz};
 
Vojtech Erban, \email{vojtech.erban@geology.cz}, 
contributed the algorithm hatching closed polygons
}


\keyword{hplot}
\concept{Plot}
\concept{Spiderplot}
\concept{Menu: Plots}
