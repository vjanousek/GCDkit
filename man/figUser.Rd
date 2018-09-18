\name{figUser}
\title{Plot editing: User defined parameter}
\alias{figUser}

\section{Menu}{
    Plot editing: User defined parameter
}

\description{
    Enables the power users to modify the plot parameters directly. 
}

\usage{
    figUser(expression=NULL,redraw=TRUE)
}


\arguments{
    \item{expression}{character; expression to be evaluated}
    \item{redraw}{logical; should be modified Figaro template redrawn?}
}

\details{
The parameters can be specified at the function call. If not, they are chosen
by a dialogue. Several of the, can entered simultaneously, as a 
semicolon delimited list. 
The most useful might be:

\tabular{ll}{

main \tab Main title  \cr 
sub \tab Sub title  \cr
xlab \tab Label of x axis \cr
ylab \tab Label of y axis \cr
xlim \tab Limits for the x axis \cr
ylim \tab Limits for the y axis \cr
bg \tab Colour of background \cr
pch \tab Plotting symbols\cr 
col \tab Colour of plotting symbols\cr 
cex \tab Relative size of plotting symbols\cr
log \tab Which of the axes is logarithmic? (\code{"","x","y"} or \code{"xy"})\cr 
}

If no parameters are entered, they can be chosen from a list (still experimental!)
}


\section{Warning}{
If requesting a logarithmic plot, make sure that the axis ranges are positive. 
See Examples or invoke menu items '\code{Plot editing: Scale x axis}' and 
'\code{Plot editing: Scale y axis}'.
}
 
\seealso{
  \link{par}
  \link{figaro}
}

\examples{
plotDiagram("TAS")
figUser()

figUser("pch=1; col=2")

figUser("pch=\"+\"")

figUser("col=\"darkblue\"")

figUser("bg=\"khaki\",cex=1.5") # for camouflage purposes

figUser("main=\"My plot\"; las=2; font.main=4; cex.main=2; col.main=\"blue\"")
}

\keyword{aplot}
\keyword{color}
\keyword{iplot}
\concept{Plot}
\concept{Figaro}
\concept{Menu: Plot editing}

\author{
Colin M. Farrow, \email{colinfarrow537@gmail.com} 

and Vojtech Janousek, \email{vojtech.janousek@geology.cz}
}
