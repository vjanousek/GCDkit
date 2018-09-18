\name{fcTraceInverse}
\alias{fcTraceInverse}
\title{Fractional crystallization - reverse modelling of trace elements}
\description{
This function calculates best estimate of mineral proportions and degree of fractional crystallization by least-squares method \emph{(Albarede 1995)}. The input data are the composition of the most primitive (parental) melt, the residual (the most fractionated) melt and Kds (distribution coefficients) for suspected crystallizing mineral phases.

}
\usage{
fcTraceInverse(Parent,Daughter,kd,whichelems,whichmins,plot=TRUE,xaxis="SiO2",
    yaxis=NULL,pch=labels$Symbol,col=labels$Colour,log="xy")
}

\arguments{
\item{Parent}{name of the sample corresponding to an unfractionated melt.}
\item{Daughter}{name of the sample corresponding to the most fractionated melt.}
\item{kd}{distribution coefficients for fractionating minerals and modelled elements.}
\item{whichelems}{which oxides are to be modelled (default = all available data)?}
\item{whichmins}{which mineral phases are to be modelled (default = all available data)?}
\item{plot}{logical; shall be the results plotted?}
\item{xaxis}{xaxis for the plot.}
\item{yaxis}{yaxis for the plot.}
\item{pch}{plotting symbols.}
\item{col}{plotting colour.}
\item{log}{which axes are to be in logarithmic scale?}
}

\note{
The function \code{FC.trace.reverse} expects the whole-rock major-element data (i.e. composition of the parental and fractionated melt) to be stored in a numeric matrix \code{WR} with individual samples given in rows. 
}

\value{
A list with the following components:
\item{end.members}{sample names of the parent and daughter}
\item{fc}{degree of fractional crystallization in \%}
\item{f}{proportions of individual fractionating minerals}
\item{original.data}{composition of the parental melt, observed and modelled composition of the residual melt, and relative difference between the two}
}

\details{
The results may be shown in a graphical form. The effects of fractional crystallization are plotted as dashed red arrows and can be compared with actual distribution of data points and the parent-daughter tigh line (solid blue arrows). 
}


\examples{
# Albarede (1995) - page 496
# Calculate proportions of fractionating minerals and degree of FC given compositions of parental
# and fractionated magma and distribution coefficients for fractionating minerals 

WR<-matrix(c(150,100,3,10,65.7,99.6,3.58,12.5),2,4,byrow=T)
colnames(WR)<-c("Ni","Sr","Yb","Rb")
rownames(WR)<-c("Sample1","Sample2")
print(WR)

kd<-matrix(c(15,1,0,0,0.1,2.0,0.05,0.35,0.25,0,0,0),3,4,byrow=F)
colnames(kd)<-c("Ni","Sr","Yb","Rb")
rownames(kd)<-c("ol","cpx","plg")
print(kd)

Parent<-"Sample1"
Daughter<-"Sample2"
result<-fcTraceInverse(Parent,Daughter,kd,plot=FALSE)

}
\references{
Albarede F 1995. Introduction to Geochemical Modeling. Cambridge: University Press.

Blundy JD & Shimizu N 1991. Trace element evidence for plagioclase recycling in calc-alkaline magmas. Earth Planet Sci Lett 102: 178-197.

}
\keyword{GCDkit}

\author{Vojtech Janousek, \email{janousek@cgu.cz}}
