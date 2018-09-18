\encoding{utf8}
\name{Jung}
\title{Al/Ti thermometer for granitic rocks (Jung + Pfänder 2007)}
\alias{Jung}

\section{Plugin}{Jung.r}

\description{
This function estimates the temperature of a granitic magma based on
measured \eqn{Al_{2}O_{3}/TiO_{2}}{Al2O3/TiO2} ratio and 
experimental constraints. The regression formulae were 
defined by \emph{Jung & Pfänder (2007)}. 
}

\usage{
Jung(model = NULL, plot = TRUE)
}

\arguments{
  \item{model}{specification of the model}
  \item{plot}{logical; should be shown a 
  \eqn{Al_{2}O_{3}/TiO_{2}}{Al2O3/TiO2} vs. \eqn{CaO/Na_{2}O}{CaO/Na2O} plot?}
}


\details{
As shown by \emph{Sylvester (1998)}, the \eqn{Al_{2}O_{3}/TiO_{2}}{Al2O3/TiO2}
ratio in the granitic magmas is temperature sensitive, decreasing with the 
increasing temperature of the crustal anatexis. This probably reflects an
increasing instability of Ti-bearing phases with progressive crustal fusion.

\emph{Jung & Pfänder (2007)} compiled the available experimental data and 
defined a set of regression formulae (linear, power law and exponential) for
several types of protoliths. 

Any of the following models can be chosen:
pelite melting, psammite melting, igneous rock melting, A-type granite melting,
amphibolite melting after \emph{Rapp & Watson (1995)} and amphibolite melting after 
\emph{Patino Douce & Beard (1995)}.

Optionally, also 
  \eqn{Al_{2}O_{3}/TiO_{2}}{Al2O3/TiO2} vs. \eqn{CaO/Na_{2}O}{CaO/Na2O} plot
  could be displayed with three secondary axes annotated by the calculated
  temperatures. 
  
  \if{html}{\figure{Jung.png}} 
  \if{latex}{\figure{Jung.png}{options: width=5.5in}}  
}

\value{
Returns a matrix '\code{results}' with the following columns:
  \item{Al2O3/TiO2}{wt. \% ratio of \eqn{Al_{2}O_{3}/TiO_{2}}{Al2O3/TiO2}}
  \item{T_Al/Ti.power.C}{temperature in C, power law calibration}
  \item{T_Al/Ti.exp.C}{temperature in C, exponential calibration}
  \item{T_Al/Ti.linear.C}{temperature in C, linear calibration}
  \item{T_Al/Ti.mean.C}{mean temperature in C, based on the above three models}
}

\section{Erratum}{
As pointed out by \emph{S. Jung (pers. com. 2009)}, in Table 1 of their original paper 
were printed wrongly several of the regression coefficients. 
These are:
\tabular{llll}{
Rock \tab Model \tab \emph{Jung and Pfander (2007)}\tab Corrected \cr
A-type \tab power law \tab B = 0.992 \tab B = 9.921 \cr
amphibolite \emph{(Rapp and Watson 1995)} \tab power law  \tab A = 2.82x10^3 \tab A = 2.82x10^30
}
The function implements these corrected values.
}

\references{
    Jung S, Pfänder J A (2007) Source composition and melting temperatures 
    of orogenic granitoids: constraints from \eqn{CaO/Na_{2}O}{CaO/Na2O}, 
    \eqn{Al_{2}O_{3}/TiO_{2}}{Al2O3/TiO2} and 
    accessory mineral saturation thermometry. Eur J Mineral 19: 859-870
    \href{http://dx.doi.org/10.1127/0935-1221/2007/0019-1774}{doi: 10.1127/0935-1221/2007/0019-1774}
    
    Patino Douce A E, Beard J S (1995) Dehydration-melting of biotite 
    gneiss and quartz amphibolite from 3 to 15 kbar. J Petrol 36: 707-738
    \href{http://dx.doi.org/10.1093/petrology/36.3.707}{doi: 10.1093/petrology/36.3.707} 
    
    Rapp R P, Watson E B (1995) Dehydration melting of metabasalt at 
    8-32 kbar: implications for continental growth and crust-mantle 
    recycling. J Petrol 36: 891-931
    \href{http://dx.doi.org/10.1093/petrology/36.4.891}{doi: 10.1093/petrology/36.4.891} 
    
    Sylvester P J (1998) Post-collisional strongly peraluminous 
    granites. Lithos 45: 29-44
    
    \href{http://dx.doi.org/10.1016/S0024-4937(98)00024-3}{doi: 10.1016/S0024-4937(98)00024-3} 
}

\examples{
Jung()

Jung("A-type")

Jung("psammite",plot=FALSE)
}

\keyword{manip}
\concept{Calculation}
\concept{Granitoids}
\concept{Thermometry}
\concept{Menu: Plugins}

\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}



