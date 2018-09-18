\name{fcMajorsInverseOptim}
\alias{fcMajorsInverseOptim}
\title{Fractional crystallization - reverse modelling of major elements (bound constrained optimization)}
\description{
This function calculates best estimate of mineral proportions and degree of fractional 
crystallization by bound constrained optimization \emph{(Byrd et al. 1995)}. 
The input data are the composition of the most primitive (parental) melt, 
the residual (the most fractionated) melt and composition of crystallizing mineral phases.
}


\usage{
fcMajorsInverseOptim(Parent,Daughter,Minerals,whichelems=colnames(Minerals),
plot=TRUE,xaxis="SiO2",yaxis=NULL,plot.cum=FALSE,
plot.mins=FALSE,pch=labels$Symbol,col=labels$Colour) 
}

\arguments{
\item{Parent}{name of the sample corresponding to an unfractionated melt.}
\item{Daughter}{name of the sample corresponding to the most fractionated melt.}
\item{Minerals}{composition of fractionating minerals.}
\item{whichelems}{which oxides are to be modelled (default = all available data)?}
\item{plot}{logical; shall be the results plotted?}
\item{xaxis}{common xaxis for plots.}
\item{yaxis}{y axes for plots.}
\item{plot.cum}{logical, should be the composition of cumulate also plotted?}
\item{plot.mins}{logical, should be the composition of fractionating minerals also plotted?}
\item{pch}{plotting symbols.}
\item{col}{plotting colour.}
}

\note{
The function \code{fcMajorsInverseOptim} expects the whole-rock major-element data 
(i.e. composition of the parental and fractionated melt, plus/minus values to 
be plotted onto binary diagrams) to be stored in a numeric matrix \code{WR} 
with individual samples given in rows. 
}

\value{
A list with the following components:
\item{end.members}{sample names of the parent and daughter}
\item{fc}{degree of fractional crystallization in \%}
\item{f}{proportions of individual fractionating minerals}
\item{original.data}{composition of the parental melt, crystallizing minerals, 
observed and modelled composition of the residual melt, and composition of the cumulate}
\item{R.squared}{sum of squared residuals}
}

\details{
This routine takes advantage of the R function \code{\link{optim}}. Its advantage over
the conventional least squares is that the results are always positive or zero. 
In fact, each of the calculated mineral proportions can be constrained between 
0 and 100 \%.

The calculated composition of the rock  is obtained using the auxiliary 
function \code{WRComp}. The sum of squared residuals is a measure of fit. 
As a rough guide it should be less than ca. 1. Note that  it decreases 
strongly with increasing number of mineral phases involved in the calculation.  
 
The results may be shown in a graphical form. The effects of fractional 
crystallization are plotted as dashed red arrows and can be compared with 
actual distribution of data points and the parent-daughter tie line 
(solid blue arrows). The ranges of diagrams are chosen so that each of them 
accommodates all data points, the calculated arrows and, if required, 
the composition of the cumulate.
}


\examples{
# Albarede (1995) - reverse of exercise on page 8
# Calculate proportions of fractionating minerals and degree of 
# FC given proportions of parental magma, the residual liquid and mineral data
WR<-matrix(c(49.79,50.1265,16.95,17.01625,8.52,9.68725,8.59,7.21725,12.17,11.25975,2.61,2.98125),
    2,6,byrow=F)
colnames(WR)<-c("SiO2","Al2O3","FeO","MgO","CaO","Na2O")
rownames(WR)<-paste("Sample",1:2,sep="")
print(WR)

mins<-matrix(c(40.01,54.69,48.07,0,0,33.37,14.35,3.27,0,45.64,16.51,0,0,25.52,16.31,0,0,2.25),
    3,6, byrow=F) 
rownames(mins)<-c("ol","di","an")
colnames(mins)<-c("SiO2","Al2O3","FeO","MgO","CaO","Na2O")
print(mins)

fc<-fcMajorsInverseOptim("Sample1","Sample2",mins,plot=FALSE)
}
\references{
Albarede, F., 1995. Introduction to geochemical modeling. Cambridge: University Press.

Byrd R, Lu P, Nocedal J, Zhu C (1995) A Limited Memory Algorithm for Bound Constrained Optimization. 
SIAM J Sci Comput 16: 1190-1208
}
\keyword{manip}
\concept{Modelling}
\concept{Menu: Modelling}

\seealso{
     \link{optim}, \link{fcMajorsInverse}
}

\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}
