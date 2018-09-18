\name{Laroche}
\title{R1-R2 diagram (De la Roche et al. 1980)}
\alias{LarochePlut}
\alias{LarocheVolc}
\alias{Laroche}
\alias{LaRoche}

\description{
    Assigns data for the \eqn{R_{1}-R_{2}}{R1-R2} diagram 
    into Figaro template (list '\code{sheet}') and appropriate values 
    into '\code{x.data}' and '\code{y.data}'.  
}

\usage{
LarocheVolc()

LarochePlut()
}

\details{
\eqn{R_{1}-R_{2}}{R1-R2} plot, as proposed by \emph{De La Roche et al. (1980)} 
for volcanic, as well as plutonic rocks:

\tabular{ll}{
\bold{volcanic rocks} \tab \bold{plutonic rocks}\cr
\emph{trachyphonolite} \tab \emph{nepheline syenite}\cr
\emph{phonolite} \tab \emph{nepheline syenite}\cr
\emph{phono-tephrite} \tab \emph{essexite}\cr
\emph{tephrite} \tab \emph{essexite}\cr
\emph{alkali rhyolite} \tab \emph{alkali granite}\cr
\emph{qtz.trachyte} \tab \emph{qtz.syenite}\cr
\emph{trachyte} \tab \emph{syenite}\cr
\emph{rhyolite} \tab \emph{granite}\cr
\emph{qtz.latite} \tab \emph{qtz.monzonite}\cr
\emph{rhyodacite} \tab \emph{granodiorite}\cr
\emph{dacite} \tab \emph{tonalite}\cr
\emph{andesite} \tab \emph{diorite}\cr
\emph{lati-andesite} \tab \emph{monzodiorite}\cr
\emph{latite} \tab \emph{monzonite}\cr
\emph{mugearite} \tab \emph{syenodiorite}\cr
\emph{nephelinite} \tab \emph{ijolite}\cr
\emph{andesi-basalt} \tab \emph{gabbro-diorite}\cr
\emph{lati-basalt} \tab \emph{monzogabbro}\cr
\emph{hawaiite} \tab \emph{syenogabbro}\cr
\emph{tholeiite} \tab \emph{gabbronorite}\cr
\emph{basalt} \tab \emph{gabbro}\cr
\emph{alkali basalt} \tab \emph{alkaligabbro}\cr
\emph{basanite} \tab \emph{theralite}\cr
\emph{ankaratrite} \tab \emph{melteigite}\cr
\emph{picritic rock} \tab \emph{ultramafic rock}
}

\figure{Larochevolc.png}
\figure{Larocheplut.png}

}

\value{
  \item{sheet}{list with Figaro Style Sheet data}
  \item{x.data}{R1 = 4 * Si - 11 * (Na + K) - 2 * (Fe[total as bivalent] + Ti),
  all in millications; as calculated by the function '\code{LaRocheCalc()}'}
  \item{y.data}{R2 = 6 * Ca + 2 * Mg + Al, all in millications; as calculated by
  the function '\code{LaRocheCalc()}'}
}

\references{
De La Roche H, Leterrier J, Grandclaude P, & Marchal M (1980) A classification 
of volcanic and plutonic rocks using \eqn{R_{1}R_{2}}{R1R2}- diagram and major element 
analyses - its relationships with current nomenclature. Chem Geol 29: 183-210
\href{http://dx.doi.org/10.1016/0009-2541(80)90020-0}{doi: 10.1016/0009-2541(80)90020-0} 
}

\author{
  Vojtech Erban, \email{vojtech.erban@geology.cz}\cr 
  & Vojtech Janousek, \email{vojtech.janousek@geology.cz}
}

\seealso{
  \link{classify}
  \link{figaro}
  \link{LaRocheCalc}
  \link{millications}
  \link{plotDiagram}
}

\examples{
#Within GCDkit, the plot is called using following auxiliary functions:
#To classify data stored in WR (Groups by diagram)
classify("LarocheVolc") 
#or 
classify("LarochePlut")

#To plot data stored in WR or its subset (menu Classification)
plotDiagram("LarocheVolc", FALSE) 
#or
plotDiagram("LarochePlut", FALSE)
}

\keyword{hplot}
\concept{Plot}
\concept{Grouping}
\concept{Edit}
\concept{Menu: Data handling}
\concept{Menu: Classification}
