\name{Pearce1996}
\alias{Pearce1996}
\title{Nb/Y - Zr/Ti diagram (Winchester + Floyd 1977, modified by Pearce 1996)}

\description{
Assigns data for Nb/Y vs. Zr/Ti diagram into Figaro template 
(list '\code{sheet}') and appropriate values into '\code{x.data}' and '\code{y.data}'.
}

\usage{
Pearce1996()
}

\details{
Classification diagram proposed by \emph{Winchester & Floyd (1977)} using incompatible 
element ratios (Nb/Y vs. Zr/Ti). As the 
original plot has been designed prior to the publication of the TAS diagram 
\emph{Le Bas et al. 1986}, the field definition has been subsequently modified by 
\emph{Pearce (1996)}. 

    \if{html}{\figure{Pearce1996.png}} 
    \if{latex}{\figure{Pearce1996.png}{options: width=4.5in}} 
    
The following fields are defined:

\emph{
    (Subalkaline) Basalt\cr
    Alkali basalt\cr
    Foidite\cr
    Andesite/Basaltic andesite\cr
    Trachyandesite\cr
    Tephriphonolite\cr
    Rhyolite/Dacite\cr
    Trachyte\cr
    Phonolite\cr
    Alkali Rhyolite
}

}

\value{
  \item{sheet}{list with Figaro Style Sheet data}
  \item{x.data}{Nb/Y wt. \% ratio}
  \item{y.data}{Zr/Ti wt. \% ratio}
}

\references{
Le Bas M J, Le Maitre R W, Streckeisen A & Zanettin B (1986)
A chemical classification of volcanic rocks based on the total alkali-silica
diagram. J Petrology 27: 745-750
\href{http://dx.doi.org/10.1093/petrology/27.3.745}{doi: 10.1093/petrology/27.3.745} 

Pearce J A (1996) A User's Guide to Basalt Discrimination Diagrams.
In Wyman D A (ed) Trace Element Geochemistry of Volcanic Rocks: 
Applications for Massive Sulphide Exploration. Geological Association of Canada, 
Short Course Notes 12, pp 79-113

Winchester J A & Floyd P A (1977) Geochemical discrimination of different magma
series and their differentiation products using immobile elements. Chem Geol 20:
325-343
\href{http://dx.doi.org/10.1016/0009-2541(77)90057-2}{doi: 10.1016/0009-2541(77)90057-2} 
}

\author{
  Vojtech Janousek, \email{vojtech.janousek@geology.cz}
}

\seealso{
  \link{WinFloyd1}
  \link{classify}
  \link{figaro}
  \link{plotDiagram}
}

\examples{
#Within GCDkit, the plot is called using following auxiliary functions:
#To Classify data stored in WR (Groups by diagram)
classify("Pearce1996")

#To plot data stored in WR or its subset (menu Classification)
plotDiagram("Pearce1996", FALSE)
}


\keyword{hplot}
\concept{Plot}
\concept{Grouping}
\concept{Edit}
\concept{Menu: Data handling}
\concept{Menu: Classification}
