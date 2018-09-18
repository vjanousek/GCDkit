\encoding{UTF-8}
\name{Ague}
\alias{Ague}
\title{Concentration ratio diagram (Ague 1994)}

\section{Plugin}{Isocon.r}

\description{
    Implementation of Concentration ratio diagrams after \emph{Ague (1994)}
    used for judging the mobility of elements or oxides in course of various 
    geochemically open-system processes such as alteration or partial melting.
}

\usage{
Ague(x = NULL,
whichelems = "SiO2,TiO2,Al2O3,FeOt,MnO,MgO,CaO,Na2O,K2O,P2O5",
immobile = NULL, bars = NULL, plot = TRUE)
}

\arguments{
  \item{x}{two sample names for analyses of the protolith and altered rock 
    compositions, respectively.}
  \item{whichelems}{list of elements to be plotted.}
  \item{immobile}{list of (one or more) elements considered as immobile.}
  \item{bars}{optional name of the variable containing 1\eqn{\sigma}{sigma} errors} 
    for plotting error bars. 
  \item{plot}{logical, should be the diagram plotted or just the results calculated?}
}

\details{
    The Concentration ratio diagram shows concentration ratio of each 
    geochemical species of interest (element or oxide) in the 'altered rock' to that
    in its presumed 'protolith'. These ratios are plotted on the y-axis, and the 
    elements are arranged in any convenient order along x.

    Following an open-system geological process, any of the perfectly immobile 
    constituents \eqn{i} should ideally have exactly the same concentration ratio 
    \eqn{r_{inv}}{rinv} defined as \emph{(Ague 2003)}: 
    
    \deqn{r_{inv}=\frac{c{A\atop i}}{c{0\atop i}}}{%
        rinv = c(A,i)/c(0,i)} 
    
    where \eqn{c_i}{c(i)} is the concentration of the species \eqn{i}, \eqn{0} refers
    to the 'protolith' and \eqn{A} to the 'altered rock'.

    This ratio, however, would  only exceptionally equate unity, 
    when the mass of the whole system is conserved. Using the presumably 
    immobile species \eqn{i} as the geochemical reference frame, the 
    change in the rock mass can be defined as \emph{Ague (1994)}:

    \deqn{\Delta_{Mass} = \frac{c{0\atop i}}{c{A\atop i}}-1}{%
        Delta_Mass = c(0,i)/c(A)-1}
        
    Thus \eqn{r_{inv}>1}{rinv > 1} indicates overall rock mass
    loss due to removal of mobile constituents; this has the effect of increasing
    the concentrations of the immobile species ("residual enrichment"). Conversely, 
    \eqn{r_{inv}<1}{rinv < 1}  shows an overall rock mass gain ("residual dilution").
    
    The mass change of any mobile constituent \eqn{j} can be expressed as \emph{(Ague 1994)}:

    \deqn{\Delta_j = \frac{1}{r_{inv}}\frac{c{A\atop j}}{c{0\atop j}}-1}{%
        Delta = 1/rinv*c(0,i)/c(A)-1}

    Mobile species \eqn{j} that have \eqn{\frac{c{A\atop j}}{c{0\atop j}}}{c(A,j)/c(0,j)}
    ratios greater than \eqn{r_{inv}}{rinv} have been added to the system,
    and those with ratios lower than \eqn{r_{inv}}{rinv} have been lost.
    
    In the GCDkit's implementation of the Concentration ratio diagrams,
    firstly the parental and altered rock samples can be chosen interactively from
    a binary plot \eqn{MgO-SiO_2}{MgO-SiO2}, if not specified at the function call. 
    Then the user is prompted for the elements/oxides to be plotted. 
    
    If not provided as a comma delimited list among the arguments, 
    the presumably immobile elements are to be specified. 
    To facilitate this choice, printed and plotted as barplots are ordered ratios 
    of the elemental concentrations in the 'altered rock' to that in the 'protolith' 
    \eqn{(\frac{c{A\atop j}}{c{0\atop j}})}{(c(A,j)/c(0,j))}). 
    
    Finally the concentration ratio diagram is plotted. 
    If the parameter \code{bars} is given, error bars are also shown corresponding to
    \eqn{+/- 1\sigma}{+/- 1 sigma}. 
    
    \if{html}{\figure{ague.png}} 
    \if{latex}{\figure{ague.png}{options: width=5.5in}}
  
}

\value{
    Returns a matrix '\code{results}' with the following columns:
    \item{Altered/Protolith}{concentration ratios of the given 
        geochemical species in the 'altered rock' to that in the 'protolith' - 
        primary y axis of the plot}
    \item{Gain/loss in \%}{relative gains (positive) or losses (negative) 
        corrected for the rock mass change - secondary y axis of the plot}
}

\seealso{
    \link{Wedge},  \link{isocon} 
}


\references{ 
    Ague J J (1994) Mass transfer during Barrovian metamorphism of pelites, south-central 
    Connecticut; I, Evidence for changes in composition and volume. Amer J Sci 294: 989-1057
    \href{http://dx.doi.org/10.2475/ajs.294.8.989}{doi: 10.2475/ajs.294.8.989}
    
    Ague J J (2003) Fluid infiltration and transport of major, minor, and trace 
    elements during regional metamorphism of carbonate rocks, Wepawaug Schist, 
    Connecticut, USA. Amer J Sci 303: 753-816
    \href{http://dx.doi.org/10.2475/ajs.303.9.753}{doi: 10.2475/ajs.303.9.753}
    
    Grant J A (1986) The isocon diagram - a simple solution to Gresens equation for 
    metasomatic alteration. Econ Geol 81: 1976-1982
    \href{http://dx.doi.org/10.2113/gsecongeo.81.8.1976}{doi: 10.2113/gsecongeo.81.8.1976}

    Grant J A (2005) Isocon analysis: a brief review of the method and applications. 
    Phys Chem Earth (A) 30: 997-1004
    \href{http://dx.doi.org/10.1016/j.pce.2004.11.003}{doi: 10.1016/j.pce.2004.11.003} 
    
    Gresens R L (1967) Composition-volume relationships of metasomatism. 
    Chem Geol 2: 47-55
    \href{http://dx.doi.org/10.1016/0009-2541(67)90004-6}{doi: 10.1016/0009-2541(67)90004-6}
}

\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}

\examples{
data<-loadData("sazava.data",sep="\t")
    
Ague(c("Po-4","Po-1"),
"SiO2,TiO2,Al2O3,FeOt,MgO,CaO,Rb,Sr,Ba,Zr,La,Nd,Eu,Gd,Yb,Y",
"TiO2,SiO2,FeOt")      
}

\keyword{manip}
\concept{Calculation}
\concept{Alteration}
\concept{Plugin}
\concept{Plot}
