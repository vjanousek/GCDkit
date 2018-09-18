\encoding{utf8}
\name{rtSaturation}
\title{Rutile saturation (Hayden + Watson 2007)}
\alias{rtSaturation}

\section{Plugin}{Saturation.r}

\description{
Calculates rutile saturation temperatures for the observed major-element data 
and Ti concentrations. Also returns  Ti saturation levels for the given 
major-element compositions and assumed magma temperature.
}
\usage{
rtSaturation(cats=milli,T=0,P=0,Ti=filterOut(WR,"Ti",1))
}

\arguments{
  \item{cats}{numeric matrix; whole-rock data recast to millications}
  \item{T}{assumed temperature of the magma in °C}
  \item{P}{assumed pressure in kbar, \emph{Ryerson & Watson (1987)} model only}
  \item{Ti}{numeric vector with Ti concentrations in ppm}
}

\details{
\emph{Ryerson & Watson (1987)} have first formulated rutile saturation model
for melts ranging in composition from basalt to rhyodacite. 
The distribution of \eqn{TiO_{2}}{TiO2} between rutile and liquid was given as:

\deqn{D_{TiO2} = e^{(-3.16+\frac{9373}{T}+0.026P-0.152FM)}}{%
          DTiO2 = exp(-3.16+9373/T+0.026*P-0.152*FM)}

where '\code{T}' is the absolute temperature (K) of the magma, '\code{P}' pressure (kbar) 
and '\code{FM}' is a melt composition parameter:

    \deqn{FM = \frac{1}{Si}\frac{Na+K+2(Ca+Mg+Fe)}{Al}}{%
    FM = 1/Si*(Na+K+2(Ca+Mg+Fe))/Al}.

The Ti saturation level then would be:

    \deqn{Ti.sat.RW = \frac{599342.9}{D_{TiO2}} (ppm)}{%
          Ti.sat.RW = 599342.9/DTiO2 (ppm)}

In turn, when the rutile saturation was reached, the magma temperature (in °C) can 
be calculated as:

    \deqn{TRt.sat.C.RW = \frac{9373}{(3.16+ln(100/TiO2)-0.026P+0.152FM)}-273.15}{%
          TRt.sat.C.RW<-9373/(3.16+ln(100/TiO2)-0.026*P+0.152*FM)-273.15}  

The Ti solubility in rutile-saturated hydrous siliceous melts was revisited by 
 \emph{Hayden & Watson (2007)}. According to these authors, it can be expressed as:
\deqn{Ti.sat.HW = 10^{(7.95-\frac{5305}{T}+0.124FM)} (ppm)}{%
          Ti.sat.HW = 10^(7.95 - 5305/T + 0.124 FM) (ppm)}

where '\code{T}' is the absolute temperature (K) of the magma, and '\code{FM}' 
is the melt composition parameter defined above.

The temperature (in °C) for rutile-saturated magma can be calculated as:

    \deqn{TRt.sat.C.HW = \frac{5305}{7.95-log(Ti)+0.124FM}-273.15}{%
          TRt.sat.C.HW = 5305/(7.95 -log(Ti) + 0.124 FM) - 273.15}

Using these formulae, the function '\code{rtSaturation}' calculates the rutile 
saturation levels, Ti activities and rutile saturation temperatures following
both models. 

The formulation of \emph{Ryerson & Watson (1987)}
may be more suitable for basic rocks, whereas the more recent model of 
\emph{Hayden & Watson (2007)} seems to be  appropriate for siliceous magmas. 
Please note also that the latter does not take into account effects of pressure 
(having been calibrated at 1 GPa; \emph{Hayden & Watson 2007}).
}

\value{
Returns a matrix '\code{results}' with the following columns:
  \item{FM}{melt composition parameter}
  \item{Ti}{observed Ti concentrations}
  \item{Ti.sat.RW}{saturation levels of Ti for assumed temperature, \emph{Ryerson & Watson (1987)}}
  \item{aTi.RW}{activity of Ti (ratio of Ti/Ti.sat), \emph{Ryerson & Watson (1987)}}
  \item{TRt.sat.C.RW}{rutile saturation temperatures in °C, \emph{Ryerson & Watson (1987)}}
  \item{Ti.sat.HW}{saturation levels of Ti for assumed temperature, \emph{Hayden & Watson (2007)}}
  \item{aTi.HW}{activity of Ti (ratio of Ti/Ti.sat), \emph{Hayden & Watson (2007)}}
  \item{TRt.sat.C.HW}{rutile saturation temperatures in °C, \emph{Hayden & Watson (2007)}}
}

\references{
Ryerson FJ, Watson EB (1987) Rutile saturation in magmas; implications 
for Ti-Nb-Ta depletion in island-arc basalts. Earth Planet Sci Lett 86: 225-239
\href{http://dx.doi.org/10.1016/0012-821X(87)90223-8}{doi: 10.1016/0012-821X(87)90223-8} 

Hayden LA, Watson EB (2007) Rutile saturation in hydrous siliceous melts and its 
bearing on Ti-thermometry of quartz and zircon. Earth Planet Sci Lett 258: 561-568
\href{http://dx.doi.org/doi:10.1016/j.epsl.2007.04.020}{doi: doi:10.1016/j.epsl.2007.04.020} 
}

\keyword{manip}
\concept{Calculation}
\concept{Accessory minerals}
\concept{Saturation}
\concept{Rutile}
\concept{Menu: Plugins}

\author{Vojtěch Janoušek, \email{vojtech.janousek@geology.cz}}
