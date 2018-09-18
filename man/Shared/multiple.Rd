\name{Multiple plots}
\alias{multiple}
\alias{multipleMjr}
\alias{multipleTrc}

\title{Multiple binary plots}

\description{
    These functions plot multiple binary plots with a common x axis, such as Harker 
    plots.
}

\usage{

multiple(x,y=paste(colnames(WR),sep=","),
    samples=rownames(WR),pch=labels$Symbol,
    col=labels$Colour,xmin=NULL,xmax=NULL,GUI=FALSE,nrow=NULL,ncol=NULL,...)

multipleMjr(x = "", 
    y = "SiO2,TiO2,Al2O3,FeOt,MgO,CaO,Na2O,K2O,P2O5", 
    pch = labels$Symbol, col = labels$Colour, ...) 
multipleTrc(x = "", 
    y = "Rb,Sr,Ba,Cr,Ni,La,Ce,Y,Zr,mg#,A/CNK,K2O/Na2O", 
    pch = labels$Symbol, col = labels$Colour, ...) 
}

\arguments{
\item{x}{a character vector, name of the common x axis. Formulae are OK.}
\item{y}{a character vector, names of oxides/elements to be plotted as y axes separated by 
    commas. Formulae are OK.}
\item{nrow, ncol}{dimensions of the plots' matrix}
\item{samples}{character or numeric vector; specification of the samples to be plotted.}     
\item{pch}{plotting symbols.}
\item{col}{plotting colours.}
\item{xmin, xmax}{minimum and maximum for the x axis.}
\item{GUI}{logical; is the call being made from within GCDkit GUI or not?}
\item{...}{further graphical parameters: see '\code{help(par)} for details.}
}

\details{
If x axis occurs among the arguments to be plotted as y axes, 
it is skipped.
 
Functions '\code{multipleMjr}' and 
'\code{multipleTrc}' 
are entry points supplying the default lists for major- and 
trace elements. 

Even though as a default is assumed a list of major 
\code{(SiO2, TiO2, Al2O3, FeOt, MnO, MgO, CaO,} 
\code{Na2O, K2O)} 
or trace 
\code{(Rb, Sr, Ba, Cr, Ni, La, Ce, Y, 
Zr and mg#)} 
elements, the variable(s) to be displayed can be specified.

The easiest way is to type directly the names of the  columns, separated by commas. 
Alternatively can be used their sequence numbers or ranges. Also built-in lists 
can be employed, such as 'LILE', 'REE', 'major' and 'HFSE' or their combinations 
with the column names. 

These lists are simple character vectors, and additional ones can be built by the 
user (see Examples). Note that currently only a single, stand-alone, user-defined 
list can be employed as a search criterion.

\if{html}{\figure{multiple.png}} 
\if{latex}{\figure{multiple.png}{options: width=5.5in}}

In the specification of the x axis or any of the y axes can be used also arithmetic 
expressions, see \code{\link{calcCore}} for the correct syntax.

Lastly, the user is asked to enter the limits for the x axis, two numbers 
separated by a comma. Note that the scaling takes into account the size of the 
plotting symbols, i.e. the axes are extended somewhat. 
}

\section{Note}{
    This function uses the plates concept. The individual plots can be selected and their 
    properties/appearance changed as if they were stand alone Figaro-compatible plots. 
    See \code{\link{Plate}}, \code{\link{Plate editing}} and 
    \code{\link{figaro}} for details.
}


\seealso{
    \link{figaro},  \link{Plate},  \link{Plate editing}
}


\value{
None.
}

\examples{
multipleMjr("SiO2")

multiple("Na2O+K2O",LILE,xmin=0)
# Plots the LILE against the sum of alkalis

multiple("FeOt/MgO","SiO2,CaO,Na2O+K2O,TiO2",pch="+",col="red",samples=1:10,cex=2.5)

multipleTrc("Zr")
# Plots the default trace-element set against the Zr
}

\keyword{hplot}
\concept{Plot}
\concept{Menu: Plots}

\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}
