\name{calc}
\title{Calculate a new variable}
\alias{calc}

\description{
Calculates a single numeric variable and appends it to the  data.
}

\usage{
calc()
}

\details{
The formula can invoke any combination of names of existing numerical columns, 
with the constants, brackets, arithmetic operators \code{+-*/^} and R functions. 
See \code{\link{calcCore}} for a correct syntax.

If the result is a vector of the length corresponding to the number of the samples 
in the system, the user is prompted for the name of the new data column. 
Unless a column with the specified name already exists or the given name is empty, 
the newly calculated column is appended to the data in memory ('\code{WR}'). 
}

\value{
\item{results}{numerical vector with the results}
Modifies, if appropriate, the numeric matrix '\code{WR}'.
}

\seealso{
\code{\link{selectColumnLabel}}.
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
\concept{Print}
\concept{Edit}
\concept{Calculation}
\concept{Menu: Calculations}
\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}
