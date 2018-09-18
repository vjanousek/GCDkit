\name{customPlot}
\title{Add a new plot to a script}
\alias{customPlot}

\description{
Adds a custom binary or ternary graph to the specified R script (file '\code{*.r}').
}

\usage{
customPlot()
}

\details{
To specification of the axes may be done by a formula that can involve any 
combination of names of existing numerical columns, with the constants, brackets, 
arithmetic operators \code{+-*/^} and R functions. See \code{\link{calcCore}} 
for a correct syntax.

Then the user is prompted which axes are to be logarithmic and what comment 
should be added to the data file.

The filename is chosen interactively, the default suffix for the R programs is 
\code{.r}. If the file exists already, the script is appended to its end.  

The script can be edited - see parameters to the functions \code{\link{plotWithLimits}} 
(binary plots) and \code{\link{ternary}} (ternary plots).

The script can be executed at a later time using the R command '\code{File|Source}'. 
Alternatively, it can be placed among the plugins into the subdirectory '\code{\\Plugin}'. 
All files found here that have a suffix '\code{*.r}' are executed each time when 
the new data file is being loaded into the GCDkit.
}

\value{
None.
}

\seealso{
\code{\link{customScript}}, \code{\link{plotWithLimits}} (binary plots) and \code{\link{ternary}} (ternary plots).
}

\keyword{hplot}
\keyword{file}
\concept{File}
\concept{Plot}
\concept{Menu: Plots}
\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}
