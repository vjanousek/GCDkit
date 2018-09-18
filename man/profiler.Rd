\name{profiler}
\title{Profile plotting}
\alias{profiler.r}
\alias{profiler}


\description{
Plotting geochemical profiles. As a x axis can be specified an
arbitrary variable or an numerical interval (for equidistant measurements).
}

\usage{
profiler(x = NULL, y = NULL, method = "Variable", legend = FALSE, 
    pch = 1, col = "black", cex = 1, xaxs = "r", yaxs = "i", 
    main = "",xmin = NULL, xmax = NULL) 
}


\arguments{
    \item{x}{character; optional name of variable to be plotted as x axis.}
    \item{y}{character; name(s) of variable(s) for individual profiles.}
    \item{method}{character; which of the methods is to be used? 
        Valid are "Variable","Equidistant" or "From-To".}
    \item{legend}{logical; should be plotted also legend (in a separate window)?}
    \item{pch}{plotting symbols specification.}
    \item{col}{plotting colour(s).}
    \item{cex}{numeric; relative size of the plotting symbols.}
    \item{xaxs, yaxs}{character; type of the axes. 
        See \code{\link{par}} for details.}
    \item{main}{character; main title for the plot}
    \item{xmin, xmax}{range of the x axis (for methods '\code{Variable}' and '\code{From/To}'))}
}

\details{
The function '\code{profiler}' serves for plotting three different types of 
profiles involving a single or several geochemical parameters. 


\if{html}{\figure{profiler.png}} 
\if{latex}{\figure{profiler.png}{options: width=4in}}

The first one, '\code{Variable}' uses any numeric variable as the x axis 
(e.g., SiO2 contents, depth...). It is in fact a special type of a binary plot, 
in which the data points are, for each of the  y-axis variables, joined by a line. 

The remaining two methods are very similar to each other. The x axis is in both 
cases equidistant, and the order of the individual samples follows from their 
sequence in the data set. 

The method '\code{Equidistant}' uses simply the sequence number of the 
individual samples in the data set. It does not label the x-axis, 
just prints the number of samples used for plotting.

The method '\code{From/To}' serves for drawing equidistant profiles, where
the x axis can be specified by an interval. 

In the specification of the x axis (for the method '\code{Variable}') 
or any of the y variables (all methods) can be used also arithmetic 
expressions, see \code{\link{calcCore}} for the correct syntax.

If not called from the command prompt, the samples can be selected based on 
combination of three searching mechanisms (by sample name/label, range or a 
Boolean condition) - see \code{\link{selectSubset}} for details. 

The easiest way to specify the variable(s) to be plotted on individual profile(s) 
is to type directly the names of the columns, separated by commas. 
Alternatively can be used their sequence numbers or ranges. Also built-in lists 
can be employed, such as 'LILE', 'REE', 'major' and 'HFSE' or their combinations 
with the column names. 

These lists are simple character vectors, and additional ones can be built by the 
user (see Examples). Note that currently only a single, stand-alone, user-defined 
list can be employed as a search criterion.

If the function is not called from the command prompt, and it desired so, 
the symbols and colours for each of the profiles can be specified separately in a 
simple spreadsheet-like interface. 

If x axis occurs among the arguments to be plotted as y axes, 
it is skipped.

Likewise the relative scaling of the plotting symbols and the scale of the y 
axis can be specified.

Lastly, the user is asked to enter the limits for the axes, which are always 
two numbers separated by a comma. 
}

\value{
\item{results}{numeric matrix with the values for individual profiles.}
}

\keyword{hplot}
\concept{Plot}
\concept{Profile plotting}
\concept{Menu: Plots}

\examples{
# Profiles of SiO2 versus (scaled) TiO2, MgO and K2O 
# if x is specified, method="Variable" assumed automatically
profiler("Na2O+K2O",c("TiO2","6*MgO","SiO2"),pch=c("+","o","@"),col=c("red","blue","darkgreen"),
    xmin=2,xmax=10)

# Equidistant profiles of (scaled) MgO, CaO, and Al2O3 (in sample sequence)
# with default symbols and scaling
profiler(y=c("MgO","3*CaO","2*Al2O3"),method="Equidistant",col=c("red","blue","darkgreen"))

# Equidistant profiles of two calculated variables in custom colour 
# and user-defined plotting symbols; range of the x axis will be specified
# interactively
profiler(y=c("2*MgO","10*(Na2O+K2O)"),method="From-To",pch=1:10,
 col=c("blue","red"),cex=1.5,main="My plot",xmin=10,xmax=30)
}

\author{
Vojtech Janousek, \email{vojtech.janousek@geology.cz}
}
