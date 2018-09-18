\name{WinFloyd2}
\alias{WinFloyd2}
\title{Zr/TiO2 - SiO2 (Winchester + Floyd 1977)}

\description{
Assigns data for Zr/\eqn{TiO_{2}}{TiO2} vs. \eqn{SiO_{2}}{SiO2} diagram into Figaro template (list '\code{sheet}') and appropriate values into '\code{x.data}' and '\code{y.data}' 
}

\usage{
WinFloyd2()
}

\details{
Classification diagram proposed by \emph{Winchester & Floyd (1977)}.

    \if{html}{\figure{WinFloyd2.png}} 
    \if{latex}{\figure{WinFloyd2.png}{options: width=4.5in}} 

Using incompatible element ratio and silica (Zr/\eqn{TiO_{2}}{TiO2} vs. \eqn{SiO_{2}}{SiO2}), following fields
are defined:\cr

\emph{Trachyandesite\cr
Basanite/Trachyte/Nephelinite\cr
Phonolite\cr
Trachyte\cr
Comendite/Pantellerite\cr
Rhyolite/Dacite\cr
Rhyodacite/Dacite\cr
Andesite\cr
Subalkaline basalt\cr
Alkaline basalt}
}

\value{
  \item{sheet}{list with Figaro Style Sheet data}
  \item{y.data}{SiO2 wt. \%}
  \item{x.data}{(Zr/TiO2)*0.001 wt. \% ratio}
}

\references{
Winchester J A & Floyd P A (1977) Geochemical discrimination of different
magma series and their differentiation products using immobile elements. Chem
Geol 20: 325-343 \href{http://dx.doi.org/10.1016/0009-2541(77)90057-2}{doi: 10.1016/0009-2541(77)90057-2}
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
classify("WinFloyd2")

#To plot data stored in WR or its subset (menu Classification)
plotDiagram("WinFloyd2", FALSE)
}

\keyword{hplot}
\concept{Plot}
\concept{Grouping}
\concept{Edit}
\concept{Menu: Data handling}
\concept{Menu: Classification}
