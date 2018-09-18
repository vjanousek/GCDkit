\name{FeMiddlemost}
\Rdversion{1.1}
\alias{FeMiddlemost}

\title{
Adjustment of Fe oxidation ratio (Middlemost 1989))
}

\description{
Auxiliary function performing adjustment of the iron-oxidation ratio 
as proposed by \emph{Middlemost (1989)}.
}

\usage{
FeMiddlemost(anhydrous = TRUE)
}

\arguments{
  \item{anhydrous}{logical; should be returned major-element analyses recast to anhydrous basis?}
}

\details{
This function performs an adjustment of the iron-oxidation ratio 
for individual volcanic rock types as proposed by \emph{Middlemost (1989)}.

The classification is based on TAS classification (\emph{Le Bas et al. 1986}, 
\emph{Le Maitre et al. 1989}).  

The \eqn{Fe_{2}O_{3}/FeO}{Fe2O3/FeO} ratios 
for individual rock types, based on
\emph{Verma et al. (2002)} (Fig. 1), 
are as follows:

\tabular{ll}{
foidite, \eqn{Na_{2}O+K_{2}O}{Na2O+K2O} <= 3\tab 0.15\cr
foidite, 3 < \eqn{Na_{2}O+K_{2}O}{Na2O+K2O} <= 7\tab 0.2\cr
foidite, 7 < \eqn{Na_{2}O+K_{2}O}{Na2O+K2O} <= 10\tab 0.3\cr
foidite, \eqn{Na_{2}O+K_{2}O}{Na2O+K2O} > 10\tab 0.4\cr
picrobasalt \tab 0.15 \cr  
basalt \tab 0.2 \cr   
basaltic andesite\tab 0.3 \cr   
andesite \tab 0.35 \cr   
dacite \tab 0.4 \cr   
rhyolite \tab 0.5 \cr   
trachybasalt \tab 0.3 \cr  
basaltic trachyandesite \tab 0.35 \cr  
trachyandesite \tab 0.4 \cr  
trachyte/trachydacite \tab 0.5 \cr  
tephrite/basanite, \eqn{Na_{2}O+K_{2}O}{Na2O+K2O} <= 6\tab 0.2 \cr
tephrite/basanite, \eqn{Na_{2}O+K_{2}O}{Na2O+K2O} > 6\tab 0.3 \cr
phonotephrite \tab 0.35 \cr  
tephriphonolite \tab 0.4 \cr   
phonolite \tab 0.5 \cr      
}

If the parameter '\code{anhydrous}' is set, returned are the major-element
data recast to 100 \% anhydrous basis. 
}


\value{
    A matrix with adjusted whole-rock chemical data.\cr
    No permanent changes to either '\code{WR}' or '\code{WRanh}' are made.
}

\references{
    Le Bas M J, Le Maitre R W, Streckeisen A & Zanettin B (1986)
    A chemical classification of volcanic rocks based on the total alkali-silica
    diagram. J Petrology 27: 745-750
    \href{http://dx.doi.org/10.1093/petrology/27.3.745}{doi: 10.1093/petrology/27.3.745} 

    Le Maitre R W et al (1989) Igneous Rocks: A Classification and Glossary of Terms,
    1st edition. Cambridge University Press

    Middlemost E A K (1989) Iron oxidation ratios, norms and the classification 
    of volcanic rocks. Chem Geol 77: 19-26
    \href{http://dx.doi.org/10.1016/0009-2541(89)90011-9}{doi: 10.1016/0009-2541(89)90011-9} 
    
    Verma S P, Torres-Alvarado I S, Sotelo-Rodriguez Z T (2002) SINCLAS: 
    standard igneous norm and volcanic rock classification system. 
    Comput and Geosci 28:  711-715
    \href{http://dx.doi.org/10.1016/S0098-3004(01)00087-5}{doi: 10.1016/S0098-3004(01)00087-5} 
}

\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}

\seealso{
\link{TAS}
\link{Verma}
}

\keyword{manip}
\concept{Geotectonic}
\concept{Plot}
\concept{Menu: Geotectonic}
\concept{Norm}
\concept{Calculation}
\concept{Menu: Calculations}
