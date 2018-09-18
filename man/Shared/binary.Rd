\name{binary}
\title{Binary plot}
\alias{plotWithLimits}
\alias{binary}

\description{
These functions display data as a binary plot.
}

\usage{
binary(x=NULL,y=NULL,log="",samples=rownames(WR), 
    new=TRUE, ...)

plotWithLimits(x.data, y.data, 
    digits.x=NULL, digits.y=NULL,log = "",new = TRUE,  
    xmin=.round.min.down(x.data,dec.places=digits.x,expand=TRUE),
    xmax=.round.max.up(x.data,dec.places=digits.x,expand=TRUE),
    ymin=.round.min.down(y.data,dec.places=digits.y,expand=TRUE),
    ymax=.round.max.up(y.data,dec.places=digits.y,expand=TRUE),
    xlab = "", ylab = "", fousy = "", 
    IDlabels=getOption("gcd.ident"), fit = FALSE, main = "", 
    pch = labels[names(x.data), "Symbol"],
    col = labels[names(x.data), "Colour"],
    cex=labels[names(x.data),"Size"],title=NULL,xaxs="i",yaxs="i",interactive=FALSE) 
}

\arguments{
\item{x,y}{character; specification of the plotting variables (formulae OK).}
\item{log}{a vector \code{'', 'x', 'y'} or \code{'xy'} specifying which of the 
  axes are to be logarithmic}
\item{samples}{character or numeric vector; specification of the samples to be plotted.}
\item{new}{logical; should be opened a new plotting window?}
\item{...}{Further parameters to the function '\code{plotWithLimits}'.}
\item{x.data}{a numerical vector with the x data.}
\item{y.data}{a numerical vector with the y data.}
\item{digits.x}{Precision to which should be rounded the x axis labels.}
\item{digits.y}{Precision to which should be rounded the y axis labels.}
\item{xmin, xmax}{limits of the x axis.}
\item{ymin, ymax}{limits of the y axis.}
\item{xlab, ylab}{labels for the x and y axes, respectively.}
\item{fousy}{numeric vector: if specified, vertical error bars are plotted at 
  each data point.}
\item{IDlabels}{labels that are to be used to identify the individual data points}
\item{fit}{logical, should be the data fitted by a least squares line?}
\item{main}{main title for the plot.}
\item{pch}{plotting symbols.}
\item{col}{plotting colours.}
\item{cex}{relative size of the plotting symbols.}
\item{title}{title for the plotting window.}
\item{xaxs, yaxs}{type of the x and y axes.}
\item{interactive}{logical; for internal use by our French colleagues.}
}

\details{
The function '\code{plots.with.limits}' sets up the axes, labels them, plots the 
data and, if desired, enables the user to identify the data points interactively. 

'\code{binary}' is the user interface to '\code{plotWithLimits}'.

The variables to be plotted are selected using the function 
'\code{\link{selectColumnLabel}}'. In the specification of the variables can 
be used also arithmetic expressions, see \code{\link{calcCore}} for the correct 
syntax.

The samples can be selected based on combination of three searching 
mechanisms (by sample name/label, range or a Boolean condition) - see 
\code{\link{selectSubset}} for details. 

The functions are Figaro-compatible.
}

\value{
None.
}

\examples{
binary("K2O/Na2O","Rb")

binary("Rb/Sr","Ba/Rb",log="xy",samples=1:10,col="red",pch="+",main="My plot")

plotWithLimits(WR[,"SiO2"]/10,WR[,"Na2O"]+WR[,"K2O"],xlab="SiO2/10",
ylab="alkalis")

plotWithLimits(WR[,"Rb"],WR[,"Sr"],xlab="Rb",ylab="Sr",log="xy")

plotWithLimits(WR[,"SiO2"],WR[,"Rb"],fousy=WR[,"Rb"]*0.05,xlab="SiO2",
ylab="Rb",fit=TRUE)
}

\seealso{
  \link{plot}
}

\keyword{hplot}
\concept{Plot}
\concept{Menu: Plots}

\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}
