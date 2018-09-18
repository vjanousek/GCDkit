\name{Verma}
\Rdversion{1.1}
\alias{Verma}

\title{
Major-element based discrimination plots for (ultra-)basic rocks (Verma et al. 2006)
}

\description{
Plots data stored in '\code{WR}' (or its subset) into discrimination plots proposed by 
\emph{Verma et al. (2006)} for (ultra-) basic rocks (\eqn{SiO_{2}}{SiO2} < 52 wt. \%).
}

\usage{
    Verma(FeMiddlemost=FALSE,GUI=FALSE)
}

\arguments{
    \item{FeMiddlemost}{logical, should be iron adjusted according to Middlemost (1989)?}
    \item{GUI}{logical, is the function called from a GUI?}
}

\details{
Suite of five diagrams for discrimination of geotectonic environment of ultrabasic
and basic rocks (\eqn{SiO_{2}}{SiO2} < 52 wt. \%), proposed by \emph{Verma et al. (2006)}.
It is based on log-transformed concentration ratios of major-element oxides. 
Note that prior to the transformation, the analyses are recast to 
100\% anhydrous basis. Each diagram is a plot of two discriminant functions,
DF1 and DF2, respectively in x- and y-axes. Only samples with \eqn{SiO_{2}}{SiO2} < 52 wt. \% are 
plotted. To work properly, the major element analysis should be complete 
\eqn{(SiO_{2}, TiO_{2}, Al_{2}O_{3}, Fe_{2}O_{3}, FeO, MnO, MgO, CaO, Na_{2}O, K_{2}O, P_{2}O_{5})}{(SiO2, TiO2, Al2O3, Fe2O3, FeO, MnO, MgO, CaO, Na2O, K2O, P2O5)}. 
Following the recommendation by 
\emph{Verma et al. (2006)}, prior to the plotting can be performed an adjustment 
of the iron-oxidation ratio as proposed by \emph{Middlemost (1989)} (see '\link{FeMiddlemost}'). 

For the \eqn{Fe_{2}O_{3}/FeO}{Fe2O3/FeO} ratios implemented
for individual rock types (based on TAS classification), 
see \emph{Verma et al. (2002)} (Fig. 1).

\if{html}{\figure{Verma.png}} 
\if{latex}{\figure{Verma.png}{options: width=5.5in}}

Following geotectonic settings may be deduced:
\tabular{ll}{
Abbreviation used \tab Environment \cr
IAB \tab \emph{island arc basic rocks} \cr
CRB \tab \emph{continental rift basic rocks} \cr
OIB \tab \emph{ocean-island basic rocks} \cr
MORB \tab \emph{mid-ocean ridge basic rocks} \cr
}
}

\value{
None.
}

\section{Note}{
This function uses the plates concept. The individual plots can be selected and their 
properties/appearance changed as if they were stand alone Figaro-compatible plots. 
See \code{\link{Plate}}, \code{\link{Plate editing}} and 
\code{\link{figaro}} for details.
}

\references{
Middlemost E A K (1989) Iron oxidation ratios, norms and the classification 
of volcanic rocks. Chem Geol 77: 19-26. 
\href{http://dx.doi.org/10.1016/0009-2541(89)90011-9}{doi: 10.1016/0009-2541(89)90011-9}

Verma S P, Torres-Alvarado I S, Sotelo-Rodriguez Z T (2002) SINCLAS: 
standard igneous norm and volcanic rock classification system. 
Comput and Geosci 28:  711-715. 
\href{http://dx.doi.org/10.1016/S0098-3004(01)00087-5}{doi: 10.1016/S0098-3004(01)00087-5}

Verma S P, Guevara M, Agrawal S (2006) Discriminating four tectonic settings: 
Five new geochemical diagrams for basic and ultrabasic volcanic rocks based on 
log-ratio transformation of major-element data. 
Journal of Earth System Science 115: 485-528. 
\href{http://dx.doi.org/10.1007/BF02702907}{doi: 10.1007/BF02702907}
}

\examples{
#plot the diagrams
plotPlate("Verma")
}

\seealso{
\link{FeMiddlemost}
\link{Agrawal}
\link{Plate}
\link{Plate editing}
\link{plotPlate}
\link{figaro}
}

\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}

\keyword{hplot}
\concept{Geotectonic}
\concept{Plot}
\concept{Menu: Geotectonic}

