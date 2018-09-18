\name{Catanorm}
\title{Niggli's Molecular Norm (Catanorm)}
\alias{Catanorm}

\description{
Calculates the Niggli's Molecular Norm (Catanorm) using the algorithm given by 
\emph{Hutchison (1974)}.
}

\usage{
Catanorm(WR,precision=getOption("gcd.digits"))
}

\arguments{
\item{WR}{a numerical matrix; the whole-rock data to be normalized.}
\item{precision}{precision of the result.}
}

\details{
\bold{Normative minerals of the Catanorm}

\tabular{lll}{
\bold{Parameter} \tab \bold{Full name} \tab \bold{Formula} \cr
Q \tab Quartz \tab \eqn{SiO_{2}}{SiO2} \cr
C \tab Corundum \tab \eqn{AlO_{1.5}}{AlO1.5} \cr
Or \tab Orthoclase \tab \eqn{KO_{0.5}.AlO_{1.5}.3SiO_{2}}{KO0.5 AlO1.5 3SiO2} \cr
Plag \tab Plagioclase \tab \eqn{Ab_{x}.An_{100-x}}{Abx An100-x} \cr
 \emph{Ab} \tab \emph{(Albite)} \tab \eqn{NaO_{1.5}.AlO_{1.5}.3SiO_{2}}{NaO1.5 AlO1.5 3SiO2} \cr
 \emph{An} \tab \emph{(Anorthite)} \tab \eqn{CaO.2AlO_{1.5}.2SiO_{2}}{CaO 2AlO1.5 2SiO2} \cr
Lc \tab Leucite \tab \eqn{KO_{0.5}.AlO_{1.5}.2SiO_{2}}{KO0.5 AlO1.5 2SiO2}\cr
Ne \tab Nepheline \tab \eqn{NaO_{0.5}.AlO_{1.5}.SiO_{2}}{NaO0.5 AlO1.5 SiO2}\cr
Kp \tab Kaliophilite \tab \eqn{KO_{0.5}.AlO_{1.5}.SiO_{2}}{KO0.5 AlO1.5 SiO2} \cr
Ac \tab Acmite \tab \eqn{NaO_{0.5}.FeO_{1.5}.2SiO_{2}}{NaO0.5 FeO1.5 2SiO2} \cr
Ns \tab Sodium metasilicate \tab \eqn{2NaO_{0.5}.SiO_{2}}{2NaO0.5 SiO2} \cr
Ks \tab Potassium metasilicate \tab \eqn{2KO_{0.5}.SiO_{2}}{2KO0.5 SiO2} \cr
Hy \tab Hypersthene \tab \eqn{En_{x}.Fs_{100-x}}{EnxFs100-x} \cr
Di \tab Diopside \tab \eqn{Wo_{50}.En_{x}.Fs_{50-x}}{Wo50 Enx Fs50-x} \cr
 \emph{Wo} \tab \emph{(Wollastonite)} \tab \eqn{CaO.SiO_{2}}{CaO.SiO2} \cr
 \emph{En} \tab \emph{(Enstatite)} \tab \eqn{MgO.SiO_{2}}{MgO.SiO2} \cr
 \emph{Fs} \tab \emph{(Ferrosillite)} \tab \eqn{FeO.SiO_{2}}{FeO.SiO2} \cr
Ol \tab Olivine \tab \eqn{Fo_{x}.Fa_{100-x}}{Fox Fa100-x} \cr
 \emph{Fo} \tab \emph{(Forsterite)} \tab \eqn{2MgO.SiO_{2}}{2MgO.SiO2} \cr
 \emph{Fa} \tab \emph{(Fayalite)} \tab \eqn{2FeO.SiO_{2}}{2FeO.SiO2} \cr
Cs \tab Calcium orthosilicate \tab \eqn{2CaO.SiO_{2}}{2CaO.SiO2} \cr
Mt \tab Magnetite \tab \eqn{FeO.2FeO_{1.5}}{FeO.2FeO1.5} \cr
Hm \tab Hematite \tab \eqn{FeO_{1.5}}{FeO1.5} \cr
Il \tab Ilmenite \tab \eqn{FeO.TiO_{2}}{FeO.TiO2} \cr
Tn \tab Sphene \tab \eqn{CaO.TiO_{2}.SiO_{2}}{CaO.TiO2.SiO2} \cr
Pf \tab Perovskite \tab \eqn{CaO.TiO_{2}}{CaO.TiO2} \cr
Ru \tab Rutile \tab \eqn{TiO_{2}}{TiO2} \cr
Ap \tab Apatite \tab \eqn{9CaO.6PO_{2.5}.CaF_{2}}{9CaO.6PO2.5 CaF2} \cr
 \tab or with no F \tab \eqn{5CaO.3PO_{2.5}}{5CaO.3PO2.5}  \cr
Fr \tab Fluorite \tab \eqn{CaF_{2}}{CaF2} \cr
Py \tab Pyrite \tab \eqn{FeS_{2}}{FeS2} \cr
Cf \tab Calcite \tab \eqn{CaO.CO_{2}}{CaO.CO2} \cr
}

}

\references{
Hutchison C S (1974) Laboratory Handbook of Petrographic Techniques. 
John Wiley & Sons, New York, p. 1-527
}

\value{
A numeric matrix '\code{results}'. 
}

\keyword{manip}
\concept{Norm}
\concept{Calculation}
\concept{Menu: Calculations}

\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}
