\encoding{utf8}
\name{apSaturation}
\title{Apatite saturation}
\alias{apSaturation}

\section{Plugin}{Saturation.r}

\description{
Calculates apatite saturation temperatures for observed whole-rock major-element
compositions. Prints also phosphorus saturation levels for the given major-
element compositions and assumed magma temperature.
}

\usage{
apSaturation(Si = WR[, "SiO2"], ACNK = WR[, "A/CNK"],
P2O5 = WR[, "P2O5"], T = 0)
}

\arguments{
  \item{Si}{\eqn{SiO_{2}}{SiO2} contents in the melt (wt. \%)}
  \item{ACNK}{vector with A/CNK (mol \%) values}
  \item{P2O5}{vector with \eqn{P_{2}O_{5}}{P2O5} concentrations}
  \item{T}{assumed magma temperature in °C}
}

\details{
* Calculates phosphorus saturation levels following \emph{Harrison & Watson (1984)}:

\deqn{ln(D_{P}) = \frac{8400+26400(SiO_{2}-0.5)}{T}-3.1-12.4(SiO_{2}-0.5)}{%
          ln(DP) = (8400 + 26400 (SiO2 - 0.5))/T - 3.1 - 12.4 (SiO2 - 0.5)}

\deqn{P_{2}O_{5}.HW = \frac{42}{D_{P}}}{%
          P2O5.HW = 42/DP}

where '\code{T}' = absolute temperature (K), '\eqn{D_{P}}{DP}' = 
distribution coefficient for phosphorus between\cr
apatite and melt and '\eqn{SiO_{2}}{SiO2}' 
is the weight fraction of silica in the melt, \eqn{SiO_{2}}{SiO2} wt. \%/100.

These formulae were shown to be valid only for metaluminous rocks, i.e. A/CNK < 1,
and were modified for peraluminous rocks (A/CNK > 1) by \emph{Bea et al. (1992):}

\deqn{P_{2}O_{5}.Bea = P_{2}O_{5}.HW e^{\frac{6429(A/CNK-1)}{(T-273.15)}}}{%
          P2O5.Bea = P2O5.HW * exp(6429(A/CNK - 1)/(T-273.15))}

and \emph{Pichavant et al. (1992):}

\deqn{P_{2}O_{5}.PV = P_{2}O_{5}.HW + (A/CNK - 1) e^{\frac{-5900}{T}-3.22SiO_{2}+9.31}}{%
          P2O5.PV = P2O5.HW + (A/CNK - 1) exp(-5900/T - 3.22 SiO2 + 9.31)}

Note that the phosphorus saturation concentrations are not returned by the function 
but printed only.


* Calculates saturation temperatures in °C using the observed 
\eqn{P_{2}O_{5}}{P2O5} concentrations \emph{(Harrison & Watson, 1984)}:

\deqn{T.HW = \frac{8400 + 26400(SiO_{2} - 0.5)}{ln(\frac{42}{P_{2}O_{5}})+3.1+12.4(SiO_{2}-0.5)} - 273.15}{%
          T.HW = (8400 + 26400 (SiO2 - 0.5))/(ln(42/P2O5) + 3.1 + 12.4(SiO2 - 0.5)) - 273.15}


for peraluminous rocks (A/CNK > 1) the equation of \emph{Bea et al. (1992)} 
needs to be solved for '\code{T}' (in K) by iterations:

\deqn{P_{2}O_{5}.Bea = \frac{42}{e^{\frac{8400 + 26400(SiO_{2} - 0.5)}{T}-3.1-12.4(SiO_{2}-0.5)}} e^{\frac{6429(A/CNK-1)}{(T-273.15)}}}{%
          P2O5.Bea = 42/(exp(8400 + 26400 (SiO2 - 0.5))/T - 3.1 - 12.4 (SiO2 - 0.5)) * exp(6429(A/CNK - 1)/(T-273.15))}

as is that of \emph{Pichavant et al. (1992)}:

\deqn{P_{2}O_{5}.PV = \frac{42}{e^{\frac{8400 + 26400(SiO_{2} - 0.5)}{T}-3.1-12.4(SiO_{2}-0.5)}}+(A/CNK-1)e^{\frac{-5900}{T}-3.22SiO_{2}+9.31}}{%
          P2O5.PV = 42/(exp(8400 + 26400 (SiO2 - 0.5))/T - 3.1 - 12.4 (SiO2 - 0.5)) + (A/CNK - 1) exp(-5900/T - 3.22 SiO2 + 9.31)}

}

\value{
Returns a matrix '\code{results}' with the following columns:
  \item{A/CNK}{A/CNK values}
  \item{Tap.sat.C.H+W}{saturation T of \emph{Harrison & Watson (1984)} in °C}
  \item{Tap.sat.C.Bea}{saturation T of \emph{Bea et al. (1992)} in °C, peraluminous rocks only}
  \item{Tap.sat.C.Pich}{saturation T of \emph{Pichavant et al. (1992)} in °C, peraluminous rocks only}
}

\references{
Bea F, Fershtater GB & Corretge LG (1992) The geochemistry of phosphorus in
granite rocks and the effects of aluminium. Lithos 29: 43-56
\href{http://dx.doi.org/10.1016/0024-4937(92)90033-U}{doi: 10.1016/0024-4937(92)90033-U} 

Harrison TM & Watson EB (1984) The behavior of apatite during crustal anatexis:
equilibrium and kinetic considerations. Geochim Cosmochim Acta 48: 1467-1477
\href{http://dx.doi.org/10.1016/0016-7037(84)90403-4}{doi: 10.1016/0016-7037(84)90403-4} 

Pichavant M, Montel JM & Richard LR (1992) Apatite solubility in peraluminous
liquids: experimental data and extension of the Harrison-Watson model. Geochim
Cosmochim Acta 56: 3855-3861
\href{http://dx.doi.org/10.1016/0016-7037(92)90178-L}{doi: 10.1016/0016-7037(92)90178-L} 
}

\keyword{manip}
\concept{Calculation}
\concept{Accessory minerals}
\concept{Saturation}
\concept{Apatite}
\concept{Menu: Plugins}

\author{Vojtěch Janoušek, \email{vojtech.janousek@geology.cz}}
