\name{NaAlK}
\Rdversion{1.1}
\alias{NaAlK}

\title{Na2O - Al2O3 - K2O (mol. \%) diagram}

\description{
Assigns data for ternary diagram \eqn{Na_{2}O}{Na2O} - \eqn{Al_{2}O_{3}}{Al2O3} - 
\eqn{K_{2}O}{K2O} (mol. \%)
into Figaro template (list '\code{sheet}') 
and appropriate values into '\code{x.data}' and '\code{y.data}'.
Calculates molar concentrations of alkalis and alumina, as well
as several molar ratios involving these three oxides. 
}

\usage{
NaAlK()
}


\details{
Ternary plot \eqn{Na_{2}O}{Na2O} - \eqn{Al_{2}O_{3}}{Al2O3} - 
\eqn{K_{2}O}{K2O} (mol. \%). 
Dashed lines define the following compositional fields (all oxides are
expressed in mol. \%): 

\if{html}{\figure{NaAlK.png}} 
\if{latex}{\figure{NaAlK.png}{options: width=4.5in}}
\tabular{ll}{
peraluminous + metaluminous (Shand 1943) 
\tab \eqn{(Na_{2}O+K_{2}O)/Al_{2}O_{3} < 1 }{(Na2O+K2O)/Al2O3 < 1 }\cr
peralkaline (Shand 1943)\tab \eqn{(Na_{2}O+K_{2}O)/Al_{2}O_{3} > 1 }{(Na2O+K2O)/Al2O3 > 1}\cr
perpotassic \tab \eqn{K_{2}O/Al_{2}O_{3} > 1 and K_{2}O/Na_{2}O > 1}{K2O/Al2O3 > 1 and K2O/Na2O > 1} \cr
potassic \tab \eqn{1 < K_{2}O/Na_{2}O < 3}{1 < K2O/Na2O < 3}\cr
ultrapotassic \tab  \eqn{K_{2}O/Na_{2}O >= 3 }{K2O/Na2O >= 3}\cr
}

The molar ratio of \eqn{K_{2}O/Na_{2}O}{K2O/Na2O} >= 3, 
is equivalent to \eqn{K_{2}O/Na_{2}O}{K2O/Na2O} >= 2 in wt. \%, i.e. 
to the definition of ultrapotassic igneous rocks by 
\emph{Foley et al. (1987)}. 
}

\value{
  \item{sheet}{list with Figaro Style Sheet data}
  \item{x.data, y.data}{\eqn{Na_{2}O, Al_{2}O_{3} and K_{2}O contents in mol. \% transformed into 2D}{Al2O3, Na2O and K2O contents in mol. \% transformed into 2D}}
  \item{Na2O}{\eqn{Na_{2}O in mol. \%}{Na2O in mol. \%}}
  \item{Al2O3}{\eqn{Al_{2}O_{3} in mol. \%}{Al2O3 in mol. \%} }
  \item{K2O}{\eqn{K_{2}O  in mol. \%}{K2O  in mol. \%}}
  \item{(Na2O+K2O)/Al2O3}{molecular ratio \eqn{(Na_{2}O+K_{2}O)/Al_{2}O_{3}}{(Na2O+K2O)/Al2O3}}
  \item{K2O/Al2O3}{molecular ratio \eqn{K_{2}O/Al_{2}O_{3}}{K2O/Al2O3}}
  \item{K2O/Na2O}{molecular ratio \eqn{K_{2}O/Na_{2}O}{K2O/Na2O}}
}

\references{
    Foley S F, Venturelli G, Green D H, Toscani L (1987) Ultrapotassic rocks: 
    characteristics, classification and constraints for petrogenetic models. 
    Earth Sci Rev 24: 81-134
    \href{http://dx.doi.org/10.1016/0012-8252(87)90001-8}{doi: 10.1016/0012-8252(87)90001-8} 
    
    Shand (1943) Eruptive Rocks. John Wiley & Sons
}

\author{
  Vojtech Janousek, \email{vojtech.janousek@geology.cz}
}

\seealso{
    \link{classify}
    \link{figaro}
    \link{plotDiagram}
    \link{Shand}
}

\examples{
#Within GCDkit, the plot is called using following auxiliary functions:
#To Classify data stored in WR (Groups by diagram)
classify("NaAlK")

#To plot data stored in WR or its subset (menu Classification)
plotDiagram("NaAlK", FALSE)
}

\keyword{hplot}
\concept{Plot}
\concept{Grouping}
\concept{Edit}
\concept{Menu: Data handling}
\concept{Menu: Classification}

