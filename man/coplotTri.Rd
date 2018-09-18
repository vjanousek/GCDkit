\name{coplotTri}
\title{Coplot for three variables}
\alias{coplotTri}

\description{
Plots a series of binary plots split into several groups according to the values 
of the third, so called conditioning, variable. 
}

\usage{
coplotTri(xaxis = "", yaxis = "", zaxis = "", int = "")
}

\arguments{
\item{xaxis}{Name of the data column to be used as x axis.}
\item{yaxis}{Name of the data column to be used as y axis.}
\item{zaxis}{Name of the data column with the conditioning variable.}
\item{int}{The specification of the intervals: either '\code{auto}' or a list of 
    break points separated by commas.}
}

\details{
For examination of large datasets split into user-defined subsets serves 
in R the function '\code{\link{coplot}}'. It displays a series of binary 
diagrams with the data filtered out according to the values of the third 
(conditioning) variable.

    \if{html}{\figure{coplotTri.png}} 
    \if{latex}{\figure{coplotTri.png}{options: width=5.5in}}
    
If no parameters \code{'xlab', 'ylab'} and \code{'zlab'} are given, the user is 
prompted to specify them.

The variables to be plotted are selected using the function 
'\code{\link{selectColumnLabel}}.

After this is done, the user is prompted to enter a comma-delimited list of at 
least one break point defining the intervals. 
The default includes the mean, that will be automatically supplemented by minimum 
and maximum (i.e. two intervals). 

See manual entry for '\code{\link{coplot}}' for further details.
}

\section{Warning}{
Please note that no formulae can be handled at this stage.

This function IS NOT Figaro-compatible.
}

\value{
None.
}

\seealso{
'\code{\link{coplot}}'
}

\examples{
coplotTri("SiO2","Na2O","MgO","auto")

coplotTri("MgO","Na2O","SiO2","50,60") 
# the intervals of the conditioning variable, SiO2, 
# will be (min(SiO2) - 50),(50 - 60) and (60 - max(SiO2))
}

\keyword{hplot}
\concept{Plot}
\concept{Menu: Plots}

\author{
    Vojtech Janousek, \email{vojtech.janousek@geology.cz} 
    & Vojtech Erban, \email{vojtech.erban@geology.cz}
}
