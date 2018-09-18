\encoding{utf8}
\name{mzSaturation}
\title{Monazite saturation (Montel 1993)}
\alias{mzSaturation}

\section{Plugin}{Saturation.r}

\description{
Calculates monazite saturation temperatures for given major-element compositions 
and LREE contents of the magma.
}

\usage{
mzSaturation(cats = milli, 
    REE = filterOut(WR, c("La", "Ce", "Pr","Nd","Sm", "Gd"), 1), 
    H2O = 3, Xmz = 0)
}

\arguments{
  \item{cats}{numeric matrix; whole-rock data recast to millications}
  \item{REE}{numeric matrix with LREE concentrations - only complete set of La-Gd}
  \item{H2O}{assumed water contents of the magma}
  \item{Xmz}{mole fractions of the REE-phosphates in monazite}
}

\details{
This function uses saturation model of \emph{Montel (1993)}. The formulae are as 
follows:

    \deqn{LREE = \frac{\sum{(\frac{REE_{i}}{at.weight(REEi)})}}{Xmz}}{%
    LREE = sum(REEi/at.weight(REEi))/Xmz}

where \eqn{REE_{i}}{REEi}: La, Ce, Pr, Nd, Sm, Gd.

    \deqn{Dmz = 100\frac{Na+K+2Ca}{Al}.\frac{1}{Al + Si}}{%
          Dmz = 100 (Na + K + 2Ca)/Al * 1/(Al + Si)}

    \deqn{Tmz.sat.C = \frac{13318}{9.5 + 2.34Dmz + 0.3879\sqrt{H_{2}O}-ln(LREE)}-273.15}{%
    T.calc = 13318/(9.5 + 2.34 Dmz + 0.3879 sqrt(H2O) - ln(LREE)) - 273.15}
}

\value{
Returns a matrix '\code{results}' with the following components:
  \item{Dmz}{distribution coefficient}
  \item{Tmz.sat.C}{monazite saturation temperature in °C}
}

\references{
Montel JM (1993) A model for monazite/melt equilibrium and application to the 
generation of granitic magmas. Chem Geol 110: 127-146
\href{http://dx.doi.org/10.1016/0009-2541(93)90250-M}{doi: 10.1016/0009-2541(93)90250-M} 
}

\keyword{manip}
\concept{Calculation}
\concept{Accessory minerals}
\concept{Saturation}
\concept{Monazite}
\concept{Menu: Plugins}

\author{Vojtěch Janoušek, \email{vojtech.janousek@geology.cz}}
