\name{plotWithCircles}
\title{xyz plotWithCircles}
\alias{plotWithCircles}

\description{
Plots a binary diagram of two specified variables and the whole dataset or its 
selection. The size and colours of the plotted circles correspond to the third. 
}

\usage{
plotWithCircles(xaxis = "", yaxis = "", zaxis = "",
    colour = "heat.colors", scaling.factor = NULL, 
    bins = NULL, ident = getOption("gcd.ident"))
}

\arguments{
\item{xaxis}{Name of the data column to be used as x axis.}
\item{yaxis}{Name of the data column to be used as y axis.}
\item{zaxis}{Name of the data column to determine the size/colour of the circles.}
\item{colour}{colour scheme for the circles.}
\item{scaling.factor}{a factor determine the size of the circles.}
\item{bins}{number of intervals for the legend.}
\item{ident}{Logical: should be the individual samples identified?}
}

\details{
If no parameters \code{'xlab', 'ylab'} and \code{'zlab'} are given, the user is 
prompted to specify them.

The variables are selected using the function '\code{\link{selectColumnLabel}}.

In the specification of the apices can be used also arithmetic expressions, 
see \code{\link{calcCore}} for the correct syntax.

    \if{html}{\figure{plotWithCircles.png}} 
    \if{latex}{\figure{plotWithCircles.png}{options: width=5.5in}} 
    
The samples to be plotted can be selected based on combination of three searching 
mechanisms (by sample name/label, range or a Boolean condition) - see \code{\link{selectSubset}} 
for details. 

The legal colour schemes are: 
    '\code{"grays","reds","blues","greens","cyans","violets","yellows"}
    '\code{"cm.colors","heat.colors","terrain.colors","topo.colors","rainbow", "jet.colors"}'.
}

\section{Warning}{
This function IS NOT Figaro-compatible.
}

\value{
None.
}

\examples{
plotWithCircles("SiO2","Na2O+K2O","MgO+FeOt",colour="rainbow")

plotWithCircles("SiO2","MgO","K2O",colour="grays",scaling.factor=0.5,ident=TRUE)
}

\keyword{hplot}
\concept{Plot}
\concept{Menu: Plots}

\author{
    Vojtech Janousek, \email{vojtech.janousek@geology.cz} 
    
    & Vojtech Erban, \email{vojtech.erban@geology.cz}}
