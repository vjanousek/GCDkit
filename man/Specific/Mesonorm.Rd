\name{Mesonorm}
\title{Improved Mesonorm for granitoid rocks}
\alias{Mesonorm}
\alias{Streckeisen}

\description{
Calculates \emph{eine bessere} Mesonorm for granitoids of \emph{Mielke & Winkler (1979)}.
}

\usage{
Mesonorm(WR, GUI = FALSE, precision = getOption("gcd.digits"))

Streckeisen(x, new = TRUE)
}

\arguments{
\item{WR}{a numerical matrix; the whole-rock data to be normalized.}
\item{GUI}{logical, is the function called from the GUI?}
\item{precision}{precision of the result.}
\item{x}{Normative minerals calculated by the function \code{Mesonorm}.}
\item{new}{logical, is a new plotting window to be opened?}
}

\details{
This method of norm calculation should yield mineral proportions close to the 
actual mode of granitoid rocks. The calculated components are:

\preformatted{
Orthoclase, Albite, Anorthite, Quartz, Apatite, Magnetite, 
Hematite, Ilmenite, Biotite, Amphibole, Calcite, Corundum, 
Rest
}

    \if{html}{\figure{QANOR.png}} 
    \if{latex}{\figure{QANOR.png}{options: width=5.5in}}

If desired, the function plots Q'-ANOR diagram of Streckeisen & Le Maitre (1979)
using the function \code{Streckeisen}. The fields in this diagram are labeled
as follows:

\tabular{ll}{
2 \tab alkali feldspar granite \cr
3 \tab granite \cr
4 \tab granodiorite \cr
5 \tab tonalite \cr
6* \tab quartz alkali feldspar syenite \cr
7* \tab quartz syenite \cr
8* \tab quartz monzonite \cr
9* \tab quartz monzodiorite/quartz monzogabbro \cr
10* \tab quartz diorite/quartz gabbro \cr
6 \tab alkali feldspar syenite \cr
7 \tab  syenite \cr
8 \tab  monzonite \cr
9 \tab  monzodiorite/monzogabbro \cr
10 \tab diorite/gabbro
}
}
 
\references{
Mielke P & Winkler H G F (1979) Eine bessere Berechnung der Mesonorm fuer 
granitische Gesteine. Neu Jb Mineral, Mh 471-480

Streckeisen, A. & Le Maitre, R. W. (1979) A chemical approximation to the modal 
QAPF classification of the igneous rocks. Neu Jb Mineral, Abh 136, 169-206.
}

\value{
A numeric matrix '\code{results}'. 
}

\keyword{manip}
\concept{Menu: Calculations}
\concept{Calculation}
\concept{Norm}

\author{
    Vojtech Janousek, \email{vojtech.janousek@geology.cz} 
    
    & Vojtech Erban, \email{vojtech.erban@geology.cz}
}
