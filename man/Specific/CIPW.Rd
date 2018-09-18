\name{CIPW} 
\title{CIPW norm} 
\alias{CIPW} 
\alias{CIPWhb}

\description{
Calculates various modifications of the CIPW norm.
}

\usage{
CIPW(wrdata, precision = getOption("gcd.digits"), normsum = 
FALSE, cancrinite = FALSE, spinel = FALSE, complete.results = FALSE)

CIPWhb(wrdata, precision = getOption("gcd.digits"), normsum = FALSE,
cancrinite = FALSE, spinel = FALSE, complete.results = FALSE) }

\arguments{
\item{wrdata}{a numerical matrix; the whole-rock data to be normalized.}
\item{precision}{precision of the result.}
\item{normsum}{logical; shall be the normative minerals recast to 100 \%?}
\item{cancrinite}{logical; is cancrinite present/to be calculated?}
\item{spinel}{logical; is spinel to be calculated (for ultrabasic rocks, i.e. 
for samples with \eqn{SiO_{2}}{SiO2} < 45 \% only)?}
\item{complete.results}{logical; should be returned more extensive list of minerals, 
including the end members making up Di, Hy, Ol, Bi and Hbl?}
}

\details{ The method adopted for 'classic' CIPW norm calculation is 
that of \emph{Hutchison (1974, 1975)}. The function '\code{CIPWHB}' 
is its modification with biotite and hornblende \emph{(Hutchison 
1975)}. 


\bold{Normative minerals of the standard CIPW norm}
\tabular{llll}{ 
\bold{Parameter} \tab \bold{Full name} \tab \bold{Formula} \tab \bold{Molecular weight} \cr 
Q \tab Quartz \tab \eqn{SiO_{2}}{SiO2} \tab 60.08 \cr 
C \tab Corundum \tab \eqn{Al_{2}O_{3}}{Al2O3} \tab 101.96 \cr 
Or \tab Orthoclase \tab \eqn{K_{2}O.Al_{2}O_{3}.6SiO_{2}}{K2O.Al2O3.6SiO2} \tab 556.64 \cr
Ab \tab Albite \tab \eqn{Na_{2}O.Al_{2}O_{3}.6SiO_{2}}{Na2O.Al2O3.6SiO2} \tab 524.42 \cr 
An \tab Anorthite \tab \eqn{CaO.Al_{2}O_{3}.2SiO_{2}}{CaO.Al2O3.2SiO2} \tab 278.20 \cr 
Lc \tab Leucite \tab \eqn{K_{2}O.Al_{2}O_{3}.4SiO_{2}}{K2O.Al2O3.4SiO2} \tab 436.48 \cr 
Ne \tab Nepheline \tab \eqn{Na_{2}O.Al_{2}O_{3}.2SiO_{2}}{Na2O.Al2O3.2SiO2} \tab 284.10 \cr 
Kp \tab Kaliophilite \tab \eqn{K_{2}O.Al_{2}O_{3}.2SiO_{2}}{K2O.Al2O3.2SiO2} \tab 316.32 \cr 
Nc \tab Sodium carbonate \tab \eqn{Na_{2}O.CO_{2}}{Na2O.CO2} \tab 105.99 \cr 
Ac \tab Acmite \tab \eqn{Na_{2}O.Fe_{2}O_{3}.4SiO_{2}}{Na2O.Fe2O3.4SiO2} \tab 461.99 \cr 
Ns \tab Sodium metasilicate \tab \eqn{Na_{2}O.SiO_{2}}{Na2O.SiO2} \tab 122.06 \cr 
Ks \tab Potassium metasilicate \tab \eqn{K_{2}O.SiO_{2}}{K2O.SiO2} \tab 154.28 \cr 
Di \tab Diopside \tab  \tab \cr
 __(MgDi) \tab __(Mg-diopside) \tab \eqn{CaO.MgO.2SiO_{2}}{CaO.MgO.2SiO2} \tab 216.55 \cr
 __(FeDi) \tab __(Fe-diopside) \tab \eqn{CaO.FeO.2SiO_{2}}{CaO.FeO.2SiO2} \tab 248.09 \cr
Wo \tab Wollastonite \tab \eqn{CaO.SiO_{2}}{CaO.SiO2} \tab 116.16 \cr 
Hy \tab Hypersthene \tab  \tab \cr
 __(En) \tab __(Enstatite) \tab \eqn{MgO.SiO_{2}}{MgO.SiO2} \tab 100.39 \cr
 __(Fs) \tab __(Ferrosillite) \tab \eqn{FeO.SiO_{2}}{FeO.SiO2} \tab 131.93 \cr
Ol \tab Olivine \tab  \tab \cr
 __(Fo) \tab __(Forsterite) \tab \eqn{2MgO.SiO_{2}}{2MgO.SiO2} \tab 140.70 \cr
 __(Fa) \tab __(Fayalite) \tab \eqn{2FeO.2SiO_{2}}{2FeO.SiO2} \tab 203.78 \cr
Dcs \tab Dicalcium silicate \tab \eqn{2CaO.SiO_{2}}{2CaO.SiO2} \tab 172.24 \cr 
Mt \tab Magnetite \tab \eqn{FeO.Fe_{2}O_{3}}{FeO.Fe2O3} \tab 231.54 \cr
Il \tab Ilmenite \tab \eqn{FeO.TiO_{2}}{FeO.TiO2} \tab 151.75 \cr 
Hm \tab Hematite \tab \eqn{Fe_{2}O_{3}}{Fe2O3} \tab 159.69 \cr
Tn \tab Sphene \tab \eqn{CaO.TiO_{2}.SiO_{2}}{CaO.TiO2.SiO2} \tab 196.06 \cr 
Pf \tab Perovskite \tab \eqn{CaO.TiO_{2}}{CaO.TiO2} \tab 135.98 \cr 
Ru \tab Rutile \tab \eqn{TiO_{2}.SiO_{2}}{TiO2} \tab 79.90 \cr 
Ap \tab Apatite \tab \eqn{3CaO.P_{2}O_{5}.1/3CaF_{2}}{3CaO.P2O5.1/3CaF2} \tab 336.21 \cr 
Fr \tab Fluorite \tab \eqn{CaF_{2}}{CaF2} \tab 78.08 \cr 
Py \tab Pyrite \tab \eqn{FeS_{2}}{FeS2} \tab 119.98 \cr 
Sp \tab Spinel \tab  \tab \cr
 __(MgSp) \tab __(Mg-spinel; spinel s. s.) \tab \eqn{CaO.MgO.2SiO_{2}}{CaO.MgO.2SiO2} \tab 142.27 \cr
 __(FeSp) \tab __(Fe-spinel; hercynite) \tab \eqn{CaO.FeO.2SiO_{2}}{CaO.FeO.2SiO2} \tab 173.81 \cr
Cc \tab Calcite \tab \eqn{CaO.CO_{2}}{CaO.CO2} \tab 100.09 \cr 
}

\bold{Additional minerals of the modification with hornblende and biotite}
\tabular{llll}{
\bold{Parameter} \tab \bold{Full name} \tab \bold{Formula} \tab \bold{Molecular weight} \cr
Bi \tab Biotite \tab  \tab  \cr 
  __(MgBi) \tab __(Phlogopite) \tab \eqn{KO_{0.5}.3MgO.AlO_{1.5}.3SiO_{2}}{KO0.5 3MgO AlO1.5 3SiO2} \tab 798.50 \cr 
  __(FeBi) \tab __(Annite) \tab \eqn{KO_{0.5}.3FeO.AlO_{1.5}.3SiO_{2}}{KO0.5 3FeO AlO1.5 3SiO2} \tab 987.74 \cr 
Hbl \tab Hornblende \tab  \tab  \cr 
 Act \tab Actinolite \tab  \tab  \cr 
  __(MgAct) \tab __(Tremolite) \tab \eqn{2CaO.5MgO.8SiO_{2}}{2CaO 5MgO 8SiO2} \tab 794.35 \cr 
  __(FeAct) \tab __(Ferroactinolite) \tab \eqn{2CaO.5FeO.8SiO_{2}}{2CaO 5FeO 8SiO2} \tab 952.05 \cr 
 Ed \tab Edenite \tab  \tab  \cr 
  __(MgEd) \tab __(Edenite) \tab \eqn{NaO_{0.5}.2CaO.5MgO.AlO_{1.5}.7SiO_{2}}{NaO0.5 2CaO 5MgO AlO1.5 7SiO2} \tab 1632.48 \cr 
  __(FeEd) \tab __(Ferroedenite) \tab \eqn{NaO_{0.5}.2CaO.5FeO.AlO_{1.5}.7SiO_{2}}{NaO0.5 2CaO 5FeO AlO1.5 7SiO2} \tab 1947.88 \cr 
 Ri \tab Riebeckite \tab \eqn{2NaO_{0.5}.2FeO_{1.5}.3FeO.8SiO_{2}}{2NaO0.5 2FeO1.5 3FeO 8SiO2} \tab 917.87 \cr 
}
}

\references{
    Hutchison C S (1974) Laboratory Handbook of Petrographic Techniques. John Wiley & Sons, New York, p. 1-527

    Hutchison C S (1975) The norm, its variations, their calculation and relationships. Schweiz Mineral Petrogr 
    Mitt 55: 243-256
}

\value{
A numeric matrix '\code{results}'. 
}

\keyword{manip}
\concept{Norm}
\concept{Calculation}
\concept{Menu: Calculations}

\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}
