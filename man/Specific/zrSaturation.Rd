\encoding{utf8}
\name{zrSaturation}
\title{Zircon saturation (Watson + Harrison 1983, Boehnke et al. 2013)}
\alias{zrSaturation}

\section{Plugin}{Saturation.r}

\description{
Calculates zircon saturation temperatures for the observed major-element data 
and Zr concentrations. Returns also Zr saturation levels for the given 
major-element compositions and assumed magma temperature.
}

\usage{
zrSaturation(cats = milli, T = 0, Zr = filterOut(WR, "Zr", 1))
}

\arguments{
  \item{cats}{numeric matrix; whole-rock data recast to millications}
  \item{T}{assumed temperature of the magma in °C}
  \item{Zr}{numeric vector with Zr concentrations}
}

\details{
Calculates Zr saturation concentration at a given temperature. Given '\code{T}' 
is the estimated absolute temperature (K) of the magma and '\code{M}' is a 
cationic ratio:

    \deqn{M = 100\frac{Na+K+2Ca}{Al.Si}}{%
    M = 100(Na + K + 2Ca)/(Al*Si)}

it can be written \emph{(Watson & Harrison 1983)}:

    \deqn{D_{Zr} = e^{(-3.8-0.85(M-1)+\frac{12900}{T})}}{%
          DZr = exp(-3.8 - 0.85(M - 1) + 12900/T)}

The Zr saturation level is then given by:

    \deqn{Zr.sat = \frac{497644}{D_{Zr}}}{%
          Zr.sat = 497644/DZr}

On the other hand, the saturation temperature can be obtained from the observed 
Zr concentration and magma composition (assuming no zircon inheritance)

    \deqn{D_{Zr} = \frac{497644}{Zr}}{%
          DZr = 497644/Zr}

    \deqn{TZr.sat.C = \frac{12900}{ln(D_{Zr})+3.8+0.85(M-1)}-273.15}{%
          TZr.sat.C = 12900/(ln(DZr) + 3.8 + 0.85(M - 1)) - 273.15}

An improved calibration of \emph{Boehnke et al. (2013)} has given:

  \deqn{D_{Zr} = e^{(\frac{10108}{T}-1.16(M-1)-1.48)}}{%
          DZr = exp(10108/T - 1.16(M - 1) - 1.48)}
  
and 

    \deqn{TZr.sat.C = \frac{10108}{ln(D_{Zr})+1.16(M-1)+1.48}-273.15}{%
          TZr.sat.C = 10108/(ln(DZr) + 1.16(M - 1) + 1.48) - 273.15}  
}


\value{
Returns a matrix '\code{results}' with the following columns:
  \item{M}{cationic ratios}
  \item{Zr}{observed Zr concentrations}
  \item{Zr.sat}{saturation levels of Zr after \emph{Watson & Harrison (1983)} for assumed temperature}
  \item{TZr.sat.C}{zircon saturation temperatures after \emph{Watson & Harrison (1983)} in °C}
  \item{Zr.sat (Boehnke)}{saturation levels of Zr after \emph{Boehnke et al. (2013)} for assumed temperature}
  \item{TZr.sat.C (Boehnke)}{zircon saturation temperatures after \emph{Boehnke et al. (2013)} in °C}
}

\references{
    Boehnke P, Watson EB, Trail D, Harrison TM, Schmitt AK (2013) 
    Zircon saturation re-revisited. Chem Geol 351: 324-334
    \href{http://dx.doi.org/10.1016/j.chemgeo.2013.05.028}{doi: 10.1016/j.chemgeo.2013.05.028}

    Watson EB & Harrison M (1983) Zircon saturation revisited: temperature and 
    composition effects in a variety of crustal magma types. Earth Planet Sci Lett 
    64: 295-304 
    \href{http://dx.doi.org/10.1016/0012-821X(83)90211-X}{doi: 10.1016/0012-821X(83)90211-X}  
}

\keyword{manip}
\concept{Calculation}
\concept{Accessory minerals}
\concept{Saturation}
\concept{Zircon}
\concept{Menu: Plugins}

\author{Vojtěch Janoušek, \email{vojtech.janousek@geology.cz}}
