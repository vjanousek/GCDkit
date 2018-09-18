\name{isocon}
\alias{isocon}
\alias{isoconAtoms}
\alias{isoconOxides}

\title{Isocon plots (Grant 1986)}

\section{Plugin}{Isocon.r}

\description{
Implementation of isocon plot after \emph{Grant (1986, 2005)} widely used for 
quantitative estimates of changes in mass/volume/concentration of elements or 
oxides in course of various open-system geochemical processes such as alteration 
or partial melting.
}

\usage{
isocon(x = NULL, whichelems = NULL, immobile = NULL, atomic = FALSE, plot = TRUE)

isoconAtoms()

isoconOxides()

}

\arguments{
  \item{x}{ numeric matrix with the chemical data}
  \item{whichelems}{list of elements for plotting, separated by commas}
  \item{immobile}{list of presumed immobile elements, separated by commas}
  \item{atomic}{ logical; should be atomic wt. \% used for oxides?}
  \item{plot}{logical; is  the graphical output desirable?}
}
\details{
Isocon plot \emph{(Grant 1986, 2005)} spans from the theoretical quantitative 
treatment of losses or gains of geochemical species (elements or oxides). It is 
applicable to balancing mass, volume and/or concentration changes in course of 
open-system processes such as weathering, hydrothermal alteration, metasomatic 
addition/leaching or migmatitization. 

According to \emph{Grant (2005 and references therein)} the equation for 
composition/volume changes in open-system process can be written as:

 \deqn{c{A\atop i} = \frac{M^0}{M^A}(c{0\atop i}+\Delta c_i)}{%
    c(A,i)=M0/MA*(c(0,i)+delta Ci)}

where \eqn{c_i}{c(i)} is the concentration of the species \eqn{i}, \eqn{0} refers 
to the original rock and \eqn{A} to the altered rock, \eqn{M^0}{M0} is the 
equivalent mass before and \eqn{M^A}{MA} after alteration. 

For immobile element (\eqn{\Delta c_i = 0}{delta Ci = 0}) the ratio 
\eqn{\frac{M^0}{M^A}}{M0/MA}
reflecting the overall change in mass can be obtained. 
This can be done graphically in the plot of the analytical data 
for presumed protolith (\eqn{c{0\atop i}}{c(0,i)}) and altered rock 
(\eqn{c{A\atop i}}{c(A,i)}).
Such a straight line passing through the origin is termed isocon, 
the equation of which is:

\deqn{c^A = (\frac{M^0}{M^A})c^0}{%
    cA = M0/MA*c0}
    
Species plotting above the isocon were gained, whereas those plotting below 
were lost, and the gain or loss is according to \emph{Grant (2005)}:

\deqn{\frac{\Delta c_i}{c{0\atop i}} = \frac{M^A}{M^0}\frac{c{A\atop i}}{c{0\atop i}}-1}{%
    delta ci = MA/M0*c(A,i)/c(0,i)-1}

where \eqn{\frac{c{A\atop i}}{c{0\atop i}}}{c(A,i)/c(0,i)}
is the slope of the tie line from the origin to
the data point.

In the GCDkit's implementation of the function \code{'isocon'}, 
firstly the parental and altered rock samples are to be chosen interactively from 
a binary plot \eqn{MgO-SiO_2}{MgO-SiO2}. Then the user is prompted 
for the elements/oxides to be used in the isocon analysis. Printed and plotted 
in the form of barplots are ordered slopes for each data point in the isocon diagram. 

The user can choose the presumably immobile elements. These can be either provided
as a comma delimited list, or, if empty, chosen interactively from the isocon plot.
Finally are plotted two isocons, as well as a blue equiline
(a straight line with the slope 1). 

Implemented are two methods for assessing the change in mass of the system. 
Traditionally used has been the slope of the isocon line, obtained by linear 
regression of the presumably immobile data (dark green). However, this 
depends on the scaling of the isocon plot, which is arbitrary. In particular, 
the data plotted close to the origin may appear erroneously
to lie on an isocon \emph{(Baumgartner & Olsen, 1995)}.
    \if{html}{\figure{isocon.png}} 
    \if{latex}{\figure{isocon.png}{options: width=5.5in}}
More objectively, the change in the mass can be estimated by clustering 
slopes to data points, deciphering the elements/oxides with a similar behaviour and 
averaging the slopes for the selected presumably immobile species. 

Functions \emph{'isoconAtoms'} and \emph{'isoconOxides'} are frontends to the 
function \code{'isocon'}, providing different default values. See Arguments above.
}

\value{
  Returns a list '\code{results}' with the following components:
  \item{slope.avg}{slope of the isocon obtained as an average of the slopes for 
  the individual presumably 'immobile' species}
  \item{slope.regression}{slope obtained by linear regression}
  \item{balance}{numeric matrix; balance of individual species. This matrix 
  contains the following columns:}
\item{XXX=orig.}{composition of the parental (unaltered) rock}
\item{XXX=alt.}{composition of the altered rock}
\item{Slope data point}{slope of the line connecting the data point with origin}
\item{G/L rel.(LQ)}{relative mass gain/loss, isocon slope by least-squares fit}
\item{G/L rel.(avg)}{relative mass gain/loss, averaged slopes for immobile elements}
\item{G/L wt\%/ppm(LQ)}{absolute mass gain/loss, isocon slope by least-squares fit}
\item{G/L wt\%/ppm(avg)}{absolute mass gain/loss, averaged slopes for immobile elements}
}
\references{ 
Baumgartner L P & Olsen S N (1995). A least-squares approach to
mass transport calculations using the isocon method. Econ Geol 90: 1261-1270
\href{http://dx.doi.org/10.2113/gsecongeo.90.5.1261}{doi: 10.2113/gsecongeo.90.5.1261}

Grant J A (1986) The isocon diagram - a simple solution to Gresens equation for 
metasomatic alteration. Econ Geol 81: 1976-1982
\href{http://dx.doi.org/10.2113/gsecongeo.81.8.1976}{doi: 10.2113/gsecongeo.81.8.1976}

Grant J A (2005) Isocon analysis: A brief review of the method and applications. 
Phys Chem Earth (A) 30: 997-1004
\href{http://dx.doi.org/10.1016/j.pce.2004.11.003}{doi: 10.1016/j.pce.2004.11.003}

Gresens R L (1967) Composition-volume relationships of metasomatism. 
Chem Geol 2: 47-55
}

\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}

\examples{
# Grant (2005) - see Tab. 1, Fig. 1
x<-matrix(c(46.45,1.29,14.30,11.05,0.17,5.28,12.14,2.93,0.49,3.00,3.29,42,327,
313,67,77,100,170,29,80,45.62,1.30,14.74,8.20,0.15,3.89,8.29,2.09,3.12,2.18,
10.96,39,305,282,42,75,72,214,17,140), byrow=TRUE,nrow=2)

y<-"SiO2,TiO2,Al2O3,Fe2O3,MnO,MgO,CaO,Na2O,K2O,H2O,CO2,Sc,V,Cr,Ni,Cu,Zn,Sr,Y,Ba"

colnames(x)<-unlist(strsplit(y,","))
rownames(x)<-c("UA","401")

isocon(x,y,atomic=FALSE,plot=TRUE,immobile="Al2O3,SiO2,TiO2,Cu,Sc")

isocon(x,y,atomic=TRUE,plot=FALSE)
}

\keyword{manip}
\concept{Calculation}
\concept{Alteration}
\concept{Plugin}
\concept{Plot}

