\name{bpplot2}
\title{Box-Percentile Plot}
\alias{bpplot2}

\description{
Displays statistical distribution each of the variables 
in a data frame using a box-percentile plot 
\emph{(Esty & Banfield 2003)}. 
}

\usage{
bpplot2(x,main="Box-Percentile Plot",sub="",xlab = "",
ylab="",log="y",col="lightgray",horizontal=FALSE,ylim = NULL,axes=TRUE,...)

}

\arguments{
\item{x}{data frame with the data to be plotted}
\item{main}{main title for the plot}
\item{sub}{sub title for the plot}
\item{xlab}{label for x axis}
\item{ylab}{label for y axis}
\item{log}{which of the axes is to be logarithmic?}
\item{col}{colour to fill the boxes}
\item{horizontal}{logical, should be the orientation horizontal?}
\item{ylim}{optional; limits for the y axis}
\item{axes}{logical; should be the axis drawn?}
\item{...}{additional plotting parameters}
}

\details{
    The box-percentile plot is analogous to a \code{\link{boxplot}} but the width of
    the box is variable, mimicking the distribution of the given variable.  
    As in boxplots, the median and two quartiles are marked by horizontal lines.

    \if{html}{\figure{bpplot2.png}} 
    \if{latex}{\figure{bpplot2.png}{options: width=3in}}
}

\section{Warning}{
This function IS NOT Figaro-compatible. It means that the set of diagrams cannot 
be further edited in GCDkit (e.g. tools in "Plot editing" menu are inactive).
}

\value{
None.
}

\keyword{hplot}
\concept{Plot}
\concept{Spiderplot}
\concept{Menu: Plots}

\author{
The code represents a modified function \code{'bpplot'} from the package \code{'Hmisc'}  
by Frank E Harrell Jr. (originally designed by  Jeffrey Banfield). Adopted for
GCDkit by Vojtech Janousek, \email{vojtech.janousek@geology.cz}.
}

\references{
Esty, W. W. & Banfield, J. D. (2003). The Box-Percentile Plot. 
Journal of Statistical Software 8 (17)
}
