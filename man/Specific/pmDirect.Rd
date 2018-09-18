\name{pmDirect}
\alias{pmDirect}
\alias{pmDirectBatch}
\alias{pmDirectFractionalInst}
\alias{pmDirectFractionalAggr}

\title{
    Partial melting  - direct modelling of major and/or trace elements
}

\description{
    These functions calculate major- or trace-element (or both) compositions of the melt and solid residue 
    in course of partial melting.
}

\usage{
    pmDirectBatch(c0.lab, min.prop, ff, ...)

    pmDirectFractionalInst(c0.lab, min.prop, ff, ...)
    
    pmDirectFractionalAggr(c0.lab, min.prop, ff, ...)    
}

\arguments{
  \item{c0.lab}{name of the sample corresponding to an unfractionated melt.}
  \item{min.prop}{proportions of minerals in the fractionating assemblage (sum=1).}
  \item{ff}{amount of melt left after crystallization (1-degree of fractional crystallization).}
  \item{\dots}{can set further slots in the \code{\link{PetroModel-class}} object of the 
    \code{\link{simple.direct-class}}, See Details.}
}

\details{
    The input are the melting source composition ('\code{c0.lab}'), proportions of phases in the residue 
    ('\code{min.prop}') AFTER MELTING, and degree of melting, 0 to 1 ('\code{ff}'). 
    
    In addition, (at least) one of the following parameters needs to be specified:
    
    \enumerate{
        \item{name of variable  storing a matrix of major-element compositions
            of the residual minerals (slot '\code{min.name}') if majors are to be calculated}
        \item{name of variable storing the table of distribution coefficients (slot '\code{kd.name}') 
            for the same if trace elements are desired}
    }
    
    For trace elements, the function '\code{pmDirectBatch}' calculates effects of batch crystallization
    using the formula \emph{(see Janousek et al. 2006 and references therein)}
    \deqn{{C}_{L}=\frac{{{C}_{0}}}{D+F(1-D)}}{%
          CL = c0/(D+F*(1-D))}
    
    Regarding the fractional melting model, the function '\code{pmDirectFractionalInst}' 
    calculates the instantaneous liquid composition:
    
    \deqn{{C}_{L}=\frac{{C}_{0}}{D}{{(1-F)}^{\left( \frac{1}{D}-1 \right)}}}{%
         CL = c0/D*(1-F)^(1/D-1)}

   and the function '\code{pmDirectFractionalAggr}' that of the aggregated liquid:
 
     \deqn{{C}_{L.bulk}=~\frac{{C}_{0}}{F}\left(1-{{(1-F)}^{\frac{1}{D}}} \right)}{%
          CL.bulk = c0/F*(1-(1-F)^(1/D))}
 
 
   For majors, ruled by mass balance, all three functions yield identical results:
   \deqn{{{C}_{L}}=\frac{{{C}_{0}}-\left( 1-F \right){{C}_{S}}}{F}}{%
          (CL = C0 - (1 - F) * CS)/F}  
}

\value{
    Alters the slots, especially '\code{cL.calc}', '\code{cs}' and/or '\code{dd}' 
    of the given modelling object.
}

\note{
    In its simplest form, these functions would expect the whole-rock major-element 
    data  (i.e. composition of the source) to be stored in the numeric matrix \code{WR} 
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
    \link{fcDirect} 
     
    \link{PetroModel-class}
      
    \link{simple.direct-class}
}

\examples{
    # In these examples, user needs to make sure that the dataset contains sample ATAC-4, 
    # mineral proportions are given in variable props, name of variable with mineral 
    # compositions is mins, name of variable with distribution coefficients kds
    
    # Direct mjr-element model (all give the same results for majors)
    
    model<-pmDirectBatch(c0.lab="ATAC-4",min.prop=props,ff=0.531,min.name="mins")
    summary(model)
    
    model<-pmDirectFractionalInst(c0.lab="ATAC-4",min.prop=props,ff=0.531,min.name="mins")
    summary(model)
    
    model<-pmDirectFractionalAggr(c0.lab="ATAC-4",min.prop=props,ff=0.531,min.name="mins")
    summary(model)

    # Direct trc-element model, batch melting
    model<-pmDirectBatch(c0.lab="ATAC-4",min.prop=props,ff=0.531,kd.name="kds")
    summary(model)
    
    # Direct trc-element model, fractional melting, instantaneous liquid
    model<-pmDirectFractionalInst(c0.lab="ATAC-4",min.prop=props,ff=0.531,kd.name="kds")
    summary(model)
    
    # Direct trc-element model, fractional melting, aggregated liquid
    model<-pmDirectFractionalAggr(c0.lab="ATAC-4",min.prop=props,ff=0.531,kd.name="kds")
    summary(model)
}
