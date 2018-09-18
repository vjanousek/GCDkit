\name{customScript}
\title{Add a new variable to a script}
\alias{customScript}

\description{
Adds a formula to calculate a single numeric variable to the specified \code{*.r} 
file (a R script).
}

\usage{
customScript()
}

\details{
A formula can be entered that can involve any combination of names of existing 
numerical columns, with the constants, brackets, arithmetic operators \code{+-*/^} 
and R functions. See \code{\link{calcCore}} for a correct syntax.

Then the user is prompted for the name of the variable an any comments that 
should appear in the file.

The filename is chosen interactively, the default suffix for the R programs is 
\code{.r}. If the file exists already, the script is appended to its end.  

If desired, the calculated variable can be, after the script is executed, added 
automatically to the numeric data, i.e. the numeric matrix \code{WR}. If not, 
the contents of the calculated variable can be viewed by simply typing its name 
in the R Console window.

The script can be run at a later time using the R command \code{File|Source}. 
Alternatively, it can be placed among the so-called plugins into the subdirectory 
\code{Plugin}. All files placed here with a suffix \code{*.r} are executed each
time when the new data file is being loaded into the GCDkit.
}

\value{
None.
}

\examples{
\dontrun{
# examples of valid formulae....
(Na2O+K2O)/CaO
Rb^2
log10(Sr)
mean(SiO2)/10

# ... but this command is in fact a simple R shell - 
# meaning lots of fun for power users!
summary(Rb,na.rm=T)
cbind(SiO2/2,TiO2,Na2O+K2O)
cbind(major) 
hist(SiO2,col="red")
boxplot(Rb~factor(groups))

# possibilities are endless
plot(Rb,Sr,col="blue",pch="+",xlab="Rb (ppm)",ylab="Sr (ppm)",log="xy")
}
}

\keyword{manip}
\keyword{file}
\concept{File}
\concept{Edit}
\concept{Print}
\concept{Calculation}
\concept{Menu: Calculations}
\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}
