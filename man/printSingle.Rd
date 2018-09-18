\name{printSingle}
\title{Display a variable}
\alias{printSingle}

\description{
Displays a single numeric variable or a result of a calculation.
}

\usage{
printSingle(default="")
}

\arguments{
\item{default}{character: list of default column names, separated by commas.}
}

\details{
The variable to be printed is selected using the function 
'\code{\link{selectColumnLabel}}'. In the specification of the variable can 
be used also arithmetic expressions, see \code{\link{calcCore}} for the correct 
syntax.

In the specification of the variable can be used also 
arithmetic expressions, see \code{\link{calcCore}} for the correct syntax.
}

\value{
\item{results}{numerical vector/matrix with the results}
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
summary(Rb,na.rm=TRUE)
cbind(SiO2/2,TiO2,Na2O+K2O)
cbind(major) 
hist(SiO2,col="red")
boxplot(Rb~factor(groups))

# possibilities are endless
plot(Rb,Sr,col="blue",pch="+",xlab="Rb (ppm)",ylab="Sr (ppm)",log="xy")
}
}

\keyword{print}
\concept{Calculation}
\concept{Print}
\concept{Menu: Data handling}
\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}
