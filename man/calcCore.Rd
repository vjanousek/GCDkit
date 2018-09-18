\name{calcCore}
\title{Calculation of user-defined parameters}
\alias{calcCore}

\description{
Calculates a user-defined parameter specified by the equation.
}

\usage{
calcCore(equation, where = "WR", redo = TRUE)
}

\arguments{
\item{equation}{a text string to be evaluated.}
\item{where}{which matrix should be used?}
\item{redo}{logical; should be the routine called again and again?}
}

\details{
This is a core calculation function.

The expression specified by '\code{equation}' can involve any combination of 
names of existing numerical columns in the matrix '\code{where}', numbers 
(i.e. constants), arithmetic operators \code{+-*/^} and R functions.

The most useful of the latter are '\code{sqrt}' (square root), '\code{log}' 
(natural logarithm), '\code{log10}' (common logarithm), 
'\code{exp}' (exponential function), '\code{sin}', 
'\code{cos}' and '\code{tan}' (trigonometric functions).
 
Potentially useful can be also 
\code{min} (minimum), 
\code{max} (maximum), 
\code{length} (number of elements/cases), 
'\code{sum}' (sum of the elements), 
'\code{mean}' (mean of the elements), 
and '\code{prod}' (product of the elements).
 
However, any user-defined function can be also invoked here.

For most statistical functions, an useful parameter '\code{na.rm=T}' can be specified. 
This makes the function to calculate the result  from the available data only, 
ignoring the not determined value (see Examples).

The quotation marks in '\code{equation}' need to preceded by a backslash.
Option '\code{redo}' specifies whether the routine should be called repeatedly
until some meaningful result is obtained. Otherwise  '\code{NA}' is returned.  
}

\value{
A list of three items:
\item{equation}{equation as entered by the user}
\item{results}{numeric vector with the results or NA if none can be calculated}
\item{formula}{the unevaluated expression corresponding to the '\code{equation}'}
}

\examples{
calcCore("SiO2/10")

calcCore("Na2O+K2O")

calcCore("log10(Na2O+K2O)")

calcCore("SiO2/MW[\"SiO2\"]") 
# dividing by the built-in molecularWeight, NB the backslashes

calcCore("length(MgO)")
 
calcCore("mean(MgO,na.rm=TRUE)")
# na.rm is a safety measure in case some missing values are present
# otherwise the result would be 'NA'
}

\keyword{manip}
\concept{Print}
\concept{Edit}
\concept{Calculation}
\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}
