\name{Hastie}
\alias{Hastie}
\title{Co-Th diagram (Hastie et al. 2007)}

\description{
Assigns data for Co vs. Th (ppm) diagram into Figaro template (list '\code{sheet}') 
and appropriate values into '\code{x.data}' and '\code{y.data}'
}

\usage{
Hastie()
}

\details{
Diagram in Co vs. Th space, proposed by \emph{Hastie et al. (2007)} for subdivision
of volcanic arc rocks. This is thought to be a more robust replacement
for \eqn{SiO_{2}}{SiO2} vs. \eqn{K_{2}{O}}{K2O} plot of 
\emph{Peccerillo & Taylor (1976)} for altered/weathered volcanic rocks. The decreasing 
Co concentrations are used as an index of fractionation (as a proxy for \eqn{SiO_{2}}{SiO2}),  
the Th contents mimic those of \eqn{K_{2}O}{K2O}.

The following fields are defined:

\emph{Tholeiite Series}\cr
\emph{Calc-alkaline Series}\cr
\emph{High-K Calc-alkaline and Shoshonite Series}

Rocks with composition falling beyond defined boundaries are labeled \emph{'undefined'}
by the 'classify' function.

In addition, the diagram discriminates between the following rock types:
\tabular{ll}{
\bold{Abbreviation} \tab \bold{Full name} \cr
B \tab basalt \cr 
BA/A \tab basaltic andesite and andesite \cr 
D/R* \tab dacite and rhyolite*
}
* latites and trachytes also fall in the D/R fields

\if{html}{\figure{Hastie.png}} 
\if{latex}{\figure{Hastie.png}{options: width=4.5in}}
}


\value{
  \item{sheet}{list with Figaro Style Sheet data}
  \item{x.data}{Co ppm}
  \item{y.data}{Th ppm}
}


\references{
Hastie AR, Kerr AC, Pearce JA & Mitchell SF (2007) 
Classification of altered volcanic island arc rocks using immobile trace 
elements: development of the Th-Co discrimination diagram. J Pet 48: 2341-2357
\href{http://dx.doi.org/10.1093/petrology/egm062}{doi: 10.1093/petrology/egm062} 

Peccerillo A & Taylor S R (1976) Geochemistry of Eocene calc-alkaline volcanic
rocks from the Kastamonu area, Northern Turkey. Contrib Mineral Petrol 58: 63-81
\href{http://dx.doi.org/10.1007/BF00384745}{doi: 10.1007/BF00384745} 
}

\author{
  Vojtech Janousek, \email{vojtech.janousek@geology.cz}
}

\seealso{
  \link{classify}
  \link{figaro}
  \link{plotDiagram}
}

\examples{
#Within GCDkit, the plot is called using following auxiliary functions:
#To Classify data stored in WR (Groups by diagram)
classify("Hastie")

#To plot data stored in WR or its subset (menu Classification)
plotDiagram("Hastie", FALSE)
}


\keyword{hplot}
\concept{Plot}
\concept{Grouping}
\concept{Edit}
\concept{Menu: Data handling}
\concept{Menu: Classification}
