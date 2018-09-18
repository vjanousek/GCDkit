\name{AFM}
\alias{AFM}
\title{AFM diagram (Irvine + Baragar 1971)}

\description{
Assigns data for AFM ternary diagram into Figaro template (list '\code{sheet}') and appropriate values into '\code{x.data}' and '\code{y.data}'.  
}

\usage{
AFM(equ=FALSE)
}

\arguments{
\item{equ}{Logical: Should the template use boundary defined by equation?}
}

\details{
The AFM diagram is a triangular plot with apices A, F and M defined as follows:

A = (\eqn{K_{2}O+Na_{2}O}{K2O + Na2O}) wt. \%

F = FeOtot wt. \%

M = MgO wt. \%

A + F + M = 100 \%

The classification diagram divides data into 'tholeiite series' and 'calc-alkaline
series' as proposed by \emph{Irvine & Baragar (1971)}. For extreme values linear
extrapolation of boundary curve is employed.

\if{html}{\figure{AFM.png}} 
\if{latex}{\figure{AFM.png}{options: width=4.5in}}

}
\value{
  \item{sheet}{list with Figaro Style Sheet data}
  \item{x.data, y.data}{A, F, M values (see details) transformed into 2D}
}


\references{
  Irvine T M & Baragar W R (1971) A guide to the chemical classification of common
  volcanic rocks. Canad J Earth Sci 8: 523-548
  \href{http://dx.doi.org/10.1139/e71-055}{doi: 10.1139/e71-055}
}

\author{
  Vojtech Erban, \email{vojtech.erban@geology.cz}\cr 
  & Vojtech Janousek, \email{vojtech.janousek@geology.cz}
}

\seealso{
  \link{classify}
  \link{figaro}
  \link{plotDiagram}
}

\examples{
#Within GCDkit, AFM is called using following auxiliary functions:
#To Classify data stored in WR (Groups by diagram)
classify("AFM")

#To plot data stored in WR or its subset (menu Classification)
plotDiagram("AFM", FALSE)
}

\keyword{hplot}
\concept{Plot}
\concept{Grouping}
\concept{Edit}
\concept{Menu: Data handling}
\concept{Menu: Classification}
