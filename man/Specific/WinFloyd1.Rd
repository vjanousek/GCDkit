\name{WinFloyd1}
\alias{WinFloyd1}
\title{Nb/Y - Zr/TiO2 diagram (Winchester + Floyd 1977)}

\description{
Assigns data for Nb/Y vs. Zr/\eqn{TiO_{2}}{TiO2} diagram into Figaro template (list '\code{sheet}') and appropriate values into '\code{x.data}' and '\code{y.data}'.
}

\usage{
WinFloyd1()
}

\details{
Classification diagram proposed by \emph{Winchester & Floyd (1977)}.

    \if{html}{\figure{WinFloyd1.png}} 
    \if{latex}{\figure{WinFloyd1.png}{options: width=4.5in}} 
    
Using incompatible element ratios (Nb/Y vs. Zr/\eqn{TiO_{2}}{TiO2}), following 
fields are defined:

\emph{Trachyandesite\cr
Alkali basalt\cr
Basanite/Nephelinite\cr
Trachyte\cr
Phonolite\cr
Comendite/Pantellerite\cr
Rhyolite\cr
Rhyodacite/Dacite\cr
Andesite\cr
Andesite/Basalt\cr
Subalkaline basalt}
}

\value{
  \item{sheet}{list with Figaro Style Sheet data}
  \item{x.data}{Nb/Y wt. \% ratio}
  \item{y.data}{(Zr/TiO2)*0.0001 wt. \% ratio}
}

\references{
Winchester J A & Floyd P A (1977) Geochemical discrimination of different magma
series and their differentiation products using immobile elements. Chem Geol 20:
325-343 \href{http://dx.doi.org/10.1016/0009-2541(77)90057-2}{doi: 10.1016/0009-2541(77)90057-2} 
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
classify("WinFloyd1")

#To plot data stored in WR or its subset (menu Classification)
plotDiagram("WinFloyd1", FALSE)
}

\keyword{hplot}
\concept{Plot}
\concept{Grouping}
\concept{Edit}
\concept{Menu: Data handling}
\concept{Menu: Classification}
