\name{peterplot}
\alias{peterplot}
\title{Anomaly plot}

\description{
This function plots a conventional binary diagram but the type and size of the plotting 
symbols is assigned according to the distribution of a third, conditioning variable. 
}

\usage{
peterplot(xaxis = "", yaxis = "", zaxis = "", ident = FALSE, 
    scaling.small = labels[1,"Size"], scaling.big = 2 * scaling.small, 
    assign.symbols = FALSE)
}

\arguments{
  \item{xaxis, yaxis}{character; specification of the axes}
  \item{zaxis}{character; conditioning variable}
  \item{ident}{logical; identify the individual points?}
  \item{scaling.small}{scaling factor for the smaller plotting symbols}
  \item{scaling.big}{scaling factor for the larger plotting symbols}
  \item{assign.symbols}{logical; should be the plotting symbols and their sizes assigned permanently?}
}

\details{
If no parameters \code{xaxis, yaxis} and \code{zaxis} are specified, the user is prompted
to do so interactively. 

The plotting symbols are assigned as follows: the values within 25 % around the median (between the
quartiles) obtain a \code{dot}, the higher ones are denoted by '\code{+}'and lower ones by '\code{-}'.
If the given value is an outlier, its plotting size is doubled.

    \if{html}{\figure{peterplot.png}} 
    \if{latex}{\figure{peterplot.png}{options: width=5.5in}} 

Optionally, the user can assign the plotting symbols and their sizes permanently, for use in other 
diagrams throughout the system.
}

\value{
May modify the variable \code{cex}, as well as the codes of plotting symbols stored
in the data frame \code{labels}.
}

\references{
Reimann C, Filzmoser P, Garrett R G (2002) Factor analysis applied to regional geochemical data: 
problems and possibilities. Applied Geochemistry 17: 185-206
}

\examples{
peterplot("SiO2","MgO","K2O")
 
peterplot("SiO2","MgO","K2O",assign.symbols=TRUE)
plotDiagram("TAS",FALSE)
}

\keyword{aplot}
\concept{Edit}
\concept{Plot}
\concept{Menu: Plot}
\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}
