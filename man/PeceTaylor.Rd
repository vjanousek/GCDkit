\name{PeceTaylor}
\alias{PeceTaylor}
\title{SiO2-K2O diagram (Peccerillo + Taylor 1976)}

\description{
Assigns data for \eqn{SiO_{2}}{SiO2} vs. \eqn{K_{2}O}{K2O} diagram into Figaro template (list '\code{sheet}') 
and appropriate values into '\code{x.data}' and '\code{y.data}'
}

\usage{
PeceTaylor()
}

\details{
Diagram in \eqn{SiO_{2}}{SiO2} vs. \eqn{K_{2}O}{K2O} space, proposed by \emph{Peccerillo & Taylor (1976)}, 
defines the following fields:

\emph{Tholeiite Series}\cr
\emph{Calc-alkaline Series}\cr
\emph{High-K Calc-alkaline Series}\cr
\emph{Shoshonite Series}

Field boundaries were linearly extrapolated up to 75\% of \eqn{SiO_{2}}{SiO2} 
between 'Calc-alkaline Series' and 'High-K Calc-alkaline Series',
and up to 70\% of \eqn{SiO_{2}}{SiO2} between 'High-K Calc-alkaline Series' and 
'Shoshonite Series'. 

\if{html}{\figure{Peccerillo.png}} 
\if{latex}{\figure{Peccerillo.png}{options: width=4.5in}}

To employ boundaries as originally defined
by \emph{Peccerillo & Taylor (1976)}, change the value of variable 'extrapolated' 
to 'FALSE' in the file '[R-root] \\ library \\ GCDkit \\ Diagrams \\ Classification \\ PeceTaylor.r'. 
Also note that the second value for the middle boundary (i.e. [52,1.5]) is in 
the original paper obviously misquoted as 1.3 .

Rocks with composition falling beyond defined boundaries are labeled \emph{'undefined'}
by the 'classify' function.

For comparison with similar diagrams used by other authors see \emph{Rickwood (1989)}.
}

\value{
  \item{sheet}{list with Figaro Style Sheet data}
  \item{x.data}{SiO2 weight percent}
  \item{y.data}{K2O weight percent}
}


\references{
Peccerillo A & Taylor SR (1976) Geochemistry of Eocene calc-alkaline volcanic
rocks from the Kastamonu area, Northern Turkey. Contrib Mineral Petrol 58: 63-81
\href{http://dx.doi.org/10.1007/BF00384745}{doi: 10.1007/BF00384745} 

Rickwood PC (1989) Boundary lines within petrologic diagrams which use oxides
of major and minor elements. Lithos 22: 247-263
\href{http://dx.doi.org/10.1016/0024-4937(89)90028-5}{doi: 10.1016/0024-4937(89)90028-5} 
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
#Within GCDkit, the plot is called using following auxiliary functions:
#To Classify data stored in WR (Groups by diagram)
classify("PeceTaylor")

#To plot data stored in WR or its subset (menu Classification)
plotDiagram("PeceTaylor", FALSE)
}


\keyword{hplot}
\concept{Plot}
\concept{Grouping}
\concept{Edit}
\concept{Menu: Data handling}
\concept{Menu: Classification}
