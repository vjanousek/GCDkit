\name{Plate editing}
\alias{Plate editing}
\alias{plateCex}
\alias{plateCexLab}
\alias{plateCexMain}
\alias{plateAnnotationsRemove}
\alias{platePch}
\alias{plateCol}
\alias{plateBW}
\alias{plateXLim}
\alias{plateYLim}
\alias{plate0YLim}
\alias{plateExpand}
\alias{plateExtract}

\title{Editing the plate properties/all its plots simultaneously}

\description{
    A collection of functions to modify the  properties of a plate (or all 
    its diagrams) simultaneously.
}

\usage{
plateXLim(xlim=NULL)
    
plateYLim(ylim=NULL)
    
plate0YLim()
    
plateCex(n=NULL)

plateCexLab(n=NULL)
    
plateCexMain(n=NULL)

plateAnnotationsRemove()

platePch(pch=NULL)

plateCol(col=NULL)

plateBW()
    
plateExpand(scr=NULL)
    
plateExtract(diagram,which=NULL,main=NULL,calc.only=FALSE,...)
}


\arguments{
  \item{xlim}{scaling for the x axis}
  \item{ylim}{scaling for the y axis}
  \item{n}{relative size (use n = 1 for normal one).}   
  \item{pch}{plotting symbol specification, either as string or a numeric code (\link{showSymbols}).}  
  \item{col}{colour specification, either by its English name, or by a numeric code (\link{showColours}).}
  \item{scr}{number of screen to be expanded.}
  \item{diagram}{name of the function plotting a plate.}
  \item{which}{sequential number of plot in its definition.}
  \item{main}{optional alternative main title to the diagram.}
  \item{calc.only}{logical; should be performed only calculations, without plotting?}
  \item{...}{additional parameters to the diagram (plate) plotting function.}
}

\details{
    The functions serve to change properties of all particular diagrams forming the given plate. 
    They can be used to set up the uniform size of plotting symbols (\code{'plateCex'}), 
    main title (\code{'plateCexMain'})   
    or of the axes' labels (\code{'plateCexLab'}), remove the annotation 
    of classification fields 
    (\code{'plateAnnotationsRemove'}), 
    uniform plotting symbol 
    (\code{'platePch'}) and/or colour \cr
    (\code{'plateCol'}) to all plots, or set 
    them into black and white (\code{'plateBW'}). If the same variable is plotted 
    as x or y axis in all diagrams forming the plate (e.g., on Harker plots), it can be
    scaled by means of the functions \code{'plateXLim'} and \code{'plateYLim'}.
    Using the command \code{'plate0YLim'} it is possible to set the origin of all 
    non-logarithmic y axes to zero.
    
    The function \code{'plateExpand'} displays a zoomed up version of the selected 
    diagram in a separate window. 
    
    The function \code{'plateExtract'} extracts a Figaro definition of a single plot
    from a plate plotted by the function \code{'diagram'}. If \code{'calc.only'} is \code{'FALSE'},
    the diagram is plotted, either into a separate window or into current slot, if the active plot is a
    plate.    
}


\value{
    None.
}

\seealso{
  \link{Plate}, \link{plotPlate},  \link{figaro}, \link{figScale},  \link{figCol},  
  \link{showSymbols},  \link{showColours}
}

\examples{
data<-loadData("sazava.data",sep="\t")
showSymbols()
showColours()
multiplePerPage(which=c("binary(\"K2O/Na2O\",
   \"Rb\",new=FALSE)","DebonPQ","AFM","PeceTaylor","Shand"))
   
plateCex(0.5)
   
plateCex(2)
   
platePch(11)
   
platePch("+")
   
plateCol(11)
   
plateCol("red")
   
plateBW()
   
multiple("SiO2",major)
plateXLim(c(50,70))
   
groupsByLabel("Intrusion")
spider(WR,selectNorm("Boynton"),0.1,1000,pch=labels$Symbol,col=labels$Colour)
figMulti(plot.symb=TRUE)
plateYLim(c(1,100))
   
graphicsOff()
plotDiagram("DebonBA",FALSE,FALSE)
figMulti()
plate0YLim()
   
plateExpand(2)
   
plateExtract("PearceGranite",2) # Second plot of Pearce et al. (1984), i.e. Y-Nb
}

\keyword{hplot}
\concept{Plot}
\concept{Plates}
\concept{Menu: Plots}

\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}

\references{
Pearce J A, Harris N W & Tindle A G (1984) Trace element 
discrimination diagrams for the tectonic interpretation of granitic rocks. 
J Petrology 25: 956-983. 
\href{http://dx.doi.org/10.1093/petrology/25.4.956}{doi:10.1093/petrology/25.4.956 }
}
