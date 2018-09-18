\name{Debon}
\title{BA and PQ diagrams (Debon + Le Fort 1983)}
\alias{DebonBA}
\alias{DebonPQ}
\alias{Debon}

\description{
Assigns data for Debon & Le Fort's B-A and P-Q diagrams into Figaro template (list '\code{sheet}') and appropriate values into '\code{x.data}' and '\code{y.data}'.  
}

\usage{
DebonBA()
DebonPQ()
}

\details{
\figure{DebonBA.png}

The B-A diagram as proposed by \emph{Debon and Le Fort (1983)} defines six
sectors (I - VI), reflecting alumina balance of samples. Following minerals
are characteristic for individual sectors:

\tabular{lll}{
I\tab Peraluminous domain \tab \emph{muscovite > biotite}\cr
II\tab \tab \emph{biotite > muscovite}\cr
III\tab \tab \emph{biotite (+- minor amphibole)}\cr
IV\tab Metaluminous domain \tab \emph{biotite, amphibole, +- pyroxene}\cr
V\tab \tab \emph{clinopyroxene, +- amphibole, +-biotite}\cr
VI\tab \tab \emph{unusual mineral associations (carbonatites} \ldots)
}

\figure{DebonPQ.png}

Layout of the P-Q diagram of the same authors corresponds to cationic proportions of quartz, K-feldspar and plagioclase.
Abbreviations used as classification output represent following rocks groups:

\tabular{lll}{
\bold{label}\tab \bold{plutonic rocks}\tab \bold{volcanic rocks}\cr
go\tab \emph{gabbro, diorite, anorthosite} \tab \emph{basalt, andesite, kenningite}\cr
mzgo\tab \emph{monzogabbro, monzodiorite} \tab \emph{latibasalt, latiandesite}\cr
mz\tab \emph{monzonite} \tab \emph{latite}\cr
s\tab \emph{syenite} \tab \emph{trachyte}\cr
dq\tab \emph{qtz diorite,qtz gabbro,qtz anorthosite}\tab \emph{qtz andesite,qtz basalt}\cr
mzdq\tab \emph{qtz monzodiorite,qtz monzogabbro}\tab \emph{qtz latiandesite,qtz latibasalt}\cr
mzq\tab \emph{quartz monzonite} \tab \emph{quartz latite}\cr
sq\tab \emph{ quartz syenite} \tab \emph{quartz trachyte}\cr
to\tab \emph{ tonalite, trondhjemite} \tab \emph{dacite}\cr
gd\tab \emph{ granodiorite, granogabbro} \tab \emph{rhyodacite}\cr
ad\tab \emph{ adamellite} \tab \emph{dellenite}\cr
gr\tab \emph{ granite} \tab \emph{rhyolite}
}

Parameters for the diagram are calculated by the function '\code{DebonCalc}'.
All of them are based on millications (1000 gram-atoms per 100 grams).


P = K - (Na + Ca)

Q = Si / 3 - (K + Na + 2 * Ca / 3)

A = Al - (K + Na + 2 Ca)

B = Fe + Mg + Ti


Note that the diagrams B-A and P-Q are recommended
as complementary, i.e. resulting names should be used in conjunction
(\emph{granite II} etc.). For details, see \emph{Debon & Le Fort (1983)} or \emph{(1988)}.
}

\value{
  \item{sheet}{list with Figaro Style Sheet data}
  \item{x.data}{P or B value. See details.}
  \item{y.data}{Q or A value. See details.}
}

\references{
  Debon F & Le Fort P (1983) A chemical-mineralogical classification of common
  plutonic rocks and associations. Trans Roy Soc Edinb; Earth Sci 73: 135-149

  Debon F & Le Fort P (1988) A cationic classification of common plutonic rocks
  and their magmatic associations: principles, method, applications. Bull.
  Mineral 111: 493-511
}

\author{
  Vojtech Erban, \email{vojtech.erban@geology.cz} 
  
  & Vojtech Janousek, \email{vojtech.janousek@geology.cz}
}

\seealso{
  \link{classify}
  \link{figaro}
  \link{plotDiagram}
  \link{DebonCalc}
}

\keyword{hplot}
\concept{Plot}
\concept{Grouping}
\concept{Edit}
\concept{Menu: Data handling}
\concept{Menu: Classification}
