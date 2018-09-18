\name{Shand}
\alias{Shand}
\title{A/CNK-A/NK diagram (Shand 1943)}

\description{
Assigns data for Shand's diagram into Figaro template (list '\code{sheet}') and appropriate values into '\code{x.data}' and '\code{y.data}'
}

\usage{
Shand()
}

\details{
Classic Shand's diagram (1943). Three rock types are defined in the A/CNK vs A/NK 
plot:

\emph{Peralkaline}\cr
\emph{Metaluminous}\cr
\emph{Peraluminous}

    \if{html}{\figure{Shand.png}} 
    \if{latex}{\figure{Shand.png}{options: width=4in}} 

}

\value{
  \item{sheet}{list with Figaro Style Sheet data}
  \item{x.data}{molecular ratio A/CNK=\eqn{Al_{2}O_{3}}{Al2O3}/(\eqn{CaO+Na_{2}O+K_{2}O}{CaO+Na2O+K2O})}
  \item{y.data}{molecular ratio A/NK=\eqn{Al_{2}O_{3}}{Al2O3}/(\eqn{Na_{2}O+K_{2}O}{Na2O+K2O})}
}

\references{
  Shand (1943) Eruptive Rocks. John Wiley & Sons
}

\author{
  Vojtech Erban, \email{vojtech.erban@geology.cz}\cr 
  & Vojtech Janousek, \email{vojtech.janousek@geology.cz}
}

\seealso{
    \link{classify}
    \link{figaro}
    \link{plotDiagram}
    \link{NaAlK}
}

\examples{
#Within GCDkit, the plot is called using following auxiliary functions:
#To Classify data stored in WR (Groups by diagram)
classify("Shand")

#To plot data stored in WR or its subset (menu Classification)
plotDiagram("Shand", FALSE)
}

\keyword{hplot}
\concept{Plot}
\concept{Grouping}
\concept{Edit}
\concept{Menu: Data handling}
\concept{Menu: Classification}
