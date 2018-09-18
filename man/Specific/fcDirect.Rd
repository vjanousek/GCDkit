\name{fcDirect}
\alias{fcDirect}
\alias{fcDirectFractional}
\alias{fcDirectBatch}

\title{
    Fractional crystallization - direct modelling of major and/or trace elements
}

\description{
    These functions calculate major- or trace-element (or both) compositions of the residual liquid 
    following fractional crystallization.
}

\usage{
    fcDirectBatch(c0.lab, min.prop, ff, ...)
    
    fcDirectFractional(c0.lab, min.prop, ff, ...)
}

\arguments{
  \item{c0.lab}{name of the sample corresponding to an unfractionated melt.}
  \item{min.prop}{proportions of minerals in the fractionating assemblage (sum=1).}
  \item{ff}{amount of melt left after crystallization (1-degree of fractional crystallization).}
  \item{\dots}{can set further slots in the \code{\link{PetroModel-class}} object of the 
    \code{\link{simple.direct-class}}, See Details.}
}

\details{
    The input are the parental magma composition ('\code{c0.lab}'), proportions of fractionating phases 
    ('\code{min.prop}'), and amount of melt left after crystallization ('\code{ff}').
     
    In addition, (at least) one of the following parameters needs to be specified:
    \enumerate{
        \item{name of variable storing a matrix of major-element compositions of the fractionating minerals 
            (slot '\code{min.name}') if majors are to be calculated}
        \item{name of variable storing the table of distribution coefficients (slot '\code{kd.name}') 
            if trace elements are desired}
    }
     
    For trace elements, the function '\code{fcDirectBatch}' calculates effects of batch crystallization
    using the formula \emph{(see Janousek et al. 2006 and references therein)}
    \deqn{{C}_{L}=\frac{{{C}_{0}}}{D+F(1-D)}}{%
          CL = c0/(D+F*(1-D))}
    
    and the function '\code{fcDirectFractional}' those of Rayleigh-type fractional
    crystallization:
    
    \deqn{\frac{{{C}_{L}}}{{{C}_{0}}}={{F}^{(D-1)}}}{%
          CL = C0*D^(D-1)}
    
    For majors, ruled by mass balance, both functions yield identical results:
    
    \deqn{{{C}_{L}}=\frac{{{C}_{0}}-\left( 1-F \right){{C}_{S}}}{F}}{%
          (CL = C0 - (1 - F) * CS)/F} 
}

\value{
    Alters the slots, especially '\code{cL.calc}', '\code{cs}' and/or '\code{dd}' of the given modelling object.
}

\note{
    In its simplest form, these functions would expect the whole-rock major-element 
    data  (here composition of the parental melt) to be stored in the numeric matrix \code{WR} 
    with individual samples given in rows. 
}

\references{
    Albarede, F (1995) Introduction to Geochemical Modelling. Cambridge University Press.
    
    Janousek V, Moyen JF, Martin H, Erban V, Farrow C (2016) Geochemical Modelling of Igneous Processes - 
    Principles and Recipes in R Language. Springer-Verlag, Berlin.
    \href{http://dx.doi.org/10.1007/978-3-662-46792-3}{doi: 10.1007/978-3-662-46792-3}
}

\keyword{GCDkit}
\keyword{manip}
\concept{Modelling}

\author{
    Jean-Francois Moyen, \email{jfmoyen@gmail.com}
    
    Vojtech Janousek, \email{vojtech.janousek@geology.cz}
}

\seealso{
     \link{fcReverseFractional}
      
     \link{pmDirect}
     
     \link{PetroModel-class}
      
    \link{simple.direct-class}
}

\examples{
    # Albarede (1995) - page 8
    # calculate composition of residual liquid after 20 \% fc of cumulate 
    # containing 20 \% olivine, 30 \% diopside and 50 \% anorthite. 
    WR<-t(as.matrix(c(49.79,16.95,8.52,8.59,12.17,2.61)))
    colnames(WR)<-c("SiO2","Al2O3","FeO","MgO","CaO","Na2O")
    rownames(WR)<-"MORB"
    print(WR)

    mins<-matrix(c(40.01,54.69,48.07,0,0,33.37,14.35,3.27,0,45.64,
    16.51,0,0,25.52,16.31,0,0,2.25),3,6,byrow=F) 
    rownames(mins)<-c("ol","di","an")
    colnames(mins)<-c("SiO2","Al2O3","FeO","MgO","CaO","Na2O")
    print(mins)

    min.prop<-c(0.2,0.3,0.5)
    names(min.prop)<-c("ol","di","an")
    print(min.prop)

    model<-fcDirectFractional(c0.lab="MORB",ff=0.8,min.prop=min.prop,min.name="mins") 
    summary(model)


    # In these examples, the user needs to make sure that the dataset contains sample ATAC-4, 
    # mineral proportions are given in variable props, name of variable with mineral 
    # compositions is mins, name of variable with distribution coefficients kds
    
    # Direct trc-element model (Rayleigh-type fractionation)
    model<-fcDirectFractional(c0.lab="ATAC-4",min.prop=props,ff=0.531,kd.name="kds")
    summary(model)
    
    # Direct trc-element model (batch crystallization)
    model<-fcDirectBatch(c0.lab="ATAC-4",min.prop=props,ff=0.531,kd.name="kds")
    summary(model)

    # Direct fc models - both mjr and Rayleigh-type fractionation (trc) combined
    model<-fcDirectFractional(c0.lab="ATAC-4",min.prop=props,ff=0.531,kd.name="kds",min.name="mins")
    summary(model)
}
