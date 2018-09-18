\name{trendTicks}
\alias{trendTicks}
\title{
    Petrogenetic trends
}

\description{
    Adding a trend with arrow and tick marks to a pre-existing GCDkit plot. 
}

\usage{
trendTicks(equation, x, xmin = par("usr")[1], xmax = par("usr")[2], 
    tick = abs(par("tcl")), text = FALSE, col = "blue", lty = "solid", 
    lwd = 1, arrow = FALSE, autoscale = TRUE)
}
    
\arguments{
    \item{equation}{character or expression; a valid formula expressed as a function of \code{x}.}
    \item{x}{numeric; x values where the ticks are to be drawn.}
    \item{xmin}{numeric; beginning of the trend.}
    \item{xmax}{numeric; end of the trend.}
    \item{tick}{numeric; length of a tick as a fraction of the height of a line of text.}
    \item{text}{logical; should be the tick marks annotated by text?}
    \item{col}{text or numeric; plotting colour specification.}
    \item{lty}{text or numeric; the line type.}
    \item{lwd}{numeric; the line width, a positive number, defaulting to 1.}
    \item{arrow}{logical; should be also an arrow head shown?}
    \item{autoscale}{logical; should the plot be autosized in order to accommodate the whole trend
        as well as all data points?}
}
    
\details{
    Using the function \code{\link{curve}}, the function \code{trendTicks} adds to an existing GCDkit plot 
    a linear or curved trend with tick marks and (optionally) arrow head. It is required that the trend 
    is defined as a function of \code{x}. The slope of the individual tick marks is then determined using 
    a derivation of the main function at the respective points. 

    \if{html}{\figure{trendTicks.png}} 
    \if{latex}{\figure{trendTicks.png}{options: width=4.5in}}
}

\value{
\item{}{a list with two components, \code{x} and \code{y}, with coordinates of the tick marks.}
}



\examples{
binary("Ba","Sr",xmin=200,xmax=2000,ymin=10,ymax=400)
equation<-"x/8+200"
x<-seq(2000,500,by=-100)
trendTicks(equation,x,min(x),max(x),col="darkred",lty="solid",lwd=2,arrow=TRUE,text=FALSE)
    
plot(1,1,type="n",xlim=c(0.01,1),ylim=c(0,1),xlab="Rb",ylab="Sr",log="x")
equation<-"6*x/8"
x<-seq(0.01,1,by=0.1)
trendTicks(equation,x,min(x),max(x),col=2,lwd=2,arrow=FALSE,text=FALSE,autoscale=FALSE)
}

\section{Warning}{
    Autoscaling will work only with Figaro compatible plots!
}

\keyword{manip}
\concept{Modelling}
\concept{Menu: Modelling}

\seealso{
     \link{par}
}

\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}
