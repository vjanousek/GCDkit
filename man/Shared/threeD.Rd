\name{threeD}
\title{3D plot}
\alias{threeD}

\description{
Plots a 3-D plot of three specified variables.
}

\usage{
threeD(xlab="",ylab="",zlab="")
}

\arguments{
\item{xlab}{Name of the data column to be used as x axis.}
\item{ylab}{Name of the data column to be used as y axis.}
\item{zlab}{Name of the data column to be used as z axis.}
}

\details{
This function displays three variables in a form of 3D plot. The plot can be 
rotated interactively, if required so.

The samples to be plotted can be selected based on combination of three 
searching mechanisms (by sample name/label, range or a Boolean condition) - see 
\code{\link{selectSubset}} for details. 

If no parameters \code{'xlab', 'ylab'} and \code{'zlab'} are given, the user is 
prompted to specify them.

The variables are selected using the function '\code{\link{selectColumnLabel}}.
    \if{html}{\figure{threeD.png}} 
    \if{latex}{\figure{threeD.png}{options: width=4.5in}}
    
In the specification of the apices can be used also arithmetic expressions, 
see \code{\link{calcCore}} for the correct syntax.

See manual entry for '\code{\link{cloud}}' for further details.
}

\section{Warning}{
This function IS NOT Figaro-compatible.
}

\value{
None.
}

\examples{
threeD("SiO2","Na2O+K2O","MgO+FeOt")
}

\keyword{hplot}
\concept{Plot}
\concept{Menu: Plots}

\author{
Vojtech Janousek, \email{vojtech.janousek@geology.cz} 
& Vojtech Erban, \email{vojtech.erban@geology.cz}
}
