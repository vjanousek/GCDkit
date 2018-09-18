\name{Maniar}
\title{Maniar and Piccoli (1989)}
\alias{Maniar}

\description{
Plots data stored in '\code{WR}' (or its subset) into Maniar and Piccoli's series 
of diagrams.
}

\usage{
Maniar(plot.txt = getOption("gcd.plot.text"))
}

\arguments{
\item{plot.txt}{logical, annotate fields by their names?}
}

\details{
Collection of six binary diagrams, based on major elements chemistry,
developed by \emph{Maniar & Piccoli (1989)} for tectonic discrimination of granitic 
rocks. \emph{Shand's (1943)} diagram is also used.
Diagrams are defined as follows:

\tabular{lll}{
\bold{ x axis} \tab \bold{y axis }\cr \cr
\eqn{SiO_{2}}{SiO2}\tab \eqn{K_{2}O}{K2O} \cr \cr
\eqn{SiO_{2}}{SiO2} \tab \eqn{Al_{2}O_{3}}{Al2O3} \cr \cr
\eqn{SiO_{2}}{SiO2} \tab \eqn{\frac{FeO(T)}{(FeO(T)+MgO)}}{FeO(T)/(FeO(T)+MgO)} \cr  \cr

\eqn{\frac{100*MgO}{(Al_{2}O_{3}+Na_{2}O+K_{2}O+FeO(T)+MgO)}}{100*MgO/(Al2O3+Na2O+K2O+FeO(T)+MgO)}
\tab \eqn{\frac{100*FeO(T)}{(Al_{2}O_{3}+Na_{2}O+K_{2}O+FeO(T)+MgO)}}{100*FeO(T)/(Al2O3+Na2O+K2O+FeO(T)+MgO)}  \cr M and F proportion in the AFM system\cr

\eqn{\frac{100*CaO}{(Al_{2}O_{3}+Na_{2}O+K_{2}O+FeO(T)+MgO+CaO)}}{100*CaO/(Al2O3+Na2O+K2O+FeO(T)+MgO+CaO)}
\tab \eqn{\frac{100*(FeO(T)+MgO)}{(Al_{2}O_{3}+Na_{2}O+K_{2}O+FeO(T)+MgO+CaO)}}{100*(FeO(T)+MgO)/(Al2O3+Na2O+K2O+FeO(T)+MgO+CaO)} \cr C and F proportion in the ACF system\cr
A/CNK (molar) \tab A/NK (molar) \cr \cr
}

Abbreviations used in diagrams represent granitoids from following geotectonic 
environments:

\tabular{ll}{
IAG \tab \emph{Island Arc Granitoids}\cr
CAG \tab \emph{Continental Arc Granitoids}\cr
CCG \tab \emph{Continental Collision Granitoids}\cr
POG \tab \emph{Post-orogenic Granitoids}\cr
RRG \tab \emph{Rift-related Granitoids}\cr
CEUG \tab \emph{Continental Epeirogenic Uplift Granitoids}\cr
OP \tab \emph{Oceanic Plagiogranites}\cr
}

\if{html}{\figure{Maniar.png}} 
\if{latex}{\figure{Maniar.png}{options: width=5.5in}}

\emph{Peralkaline, Metaluminous and Peraluminous} rocks are defined in the last
(Shand's) diagram.
}

\section{Note}{
    This function uses the plates concept. The individual plots can be selected and their 
    properties/appearance changed as if they were stand alone Figaro-compatible plots.\cr 
    See \code{\link{Plate}}, \code{\link{Plate editing}} and 
    \code{\link{figaro}} for details.
}

\seealso{
    \link{Plate},  \link{Plate editing},  \link{figaro}, \link{plotPlate},
}

\references{
Maniar P D & Piccoli P M (1989) Tectonic discriminations of granitoids. Geol Soc 
Amer Bull 101: 635-643. 
\href{http://dx.doi.org/10.1130/0016-7606(1989)101<0635:TDOG>2.3.CO;2}{doi: 10.1130/0016-7606(1989)101<0635:TDOG>2.3.CO;2}

Shand (1943) Eruptive Rocks. John Wiley & Sons.
}
\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}

\examples{
#plot the diagrams
plotPlate("Maniar")
}

\keyword{hplot}
\concept{Menu: Geotectonic}
\concept{Geotectonic}
\concept{Plot}
