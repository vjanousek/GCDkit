\name{fcReverseFractional}
\alias{fcReverse}
\alias{fcReverseFractional}
\alias{ReverseMjrThenDirectTrc}
    
\title{
    Fractional crystallization - reverse modelling of major or trace elements
}

\description{
    This function calculates best estimates of mineral proportions and degree of fractional 
    crystallization by the least-square method. 
    The input data are the compositions of the most primitive (parental) melt, 
    the residual (the most fractionated) melt and those of the crystallizing mineral phases.
}
\usage{
    fcReverseFractional(c0, cL, min.comp,...)
    
    ReverseMjrThenDirectTrc(c0.lab, cL.lab, min.name, min.used = NULL, 
        mjr.set = NULL, kd.name = NULL, trc.set = NULL)
}

\arguments{
  \item{c0}{composition of the most primitive (parental) melt.}
  \item{cL}{composition of the residual (the most fractionated) melt.}
  \item{min.comp}{matrix with compositions of the crystallizing minerals.}
  \item{\dots}{can set further slots in the \code{\link{PetroModel-class}} object of 
    the \code{\link{fc.reverse-class}}, See Details.}
}

\details{
    This function invokes a least-squares method (function \link{lsfit}) as described in detail 
    in \emph{Albarede (1995), Janousek and Moyen (2014) and Janousek et al. (2016)}. 
    
    The sum of squared residuals is a measure of fit. 
    As a rough guide it should be always less than ca. 1, and ideally close to zero. 
    Note that  it decreases strongly with increasing number of mineral phases involved in the calculation.
}

\value{
    Alters the slots, especially '\code{cL.calc}', '\code{min.prop}', '\code{f.range}' and '\code{r.sq}'  
    of the given modelling object.
}

\note{
    In its simplest form, the function \code{fcReverseFractional} would expect the whole-rock major-element 
    data  (i.e. composition of the parental and fractionated melt) to be stored in the numeric matrix \code{WR} 
    with individual samples given in rows. 
    
    Then the arguments given above can be replaced by the following: 
    \enumerate{
        \item{c0.lab - name of the sample in \code{WR} corresponding to the most primitive (parental) melt}
        \item{cL.lab - name of the sample in \code{WR} corresponding to the residual (the most fractionated) melt}
        \item{min.name - name of the numeric matrix with the composition of the crystallizing minerals.}
    }
}

\references{
    Albarede, F (1995) Introduction to Geochemical Modelling. Cambridge University Press.
    
    Janousek V, Moyen JF (2014) Mass Balance Modelling of Magmatic Processes in GCDkit. 
    In: Kumar S, Singh RN (eds) Modelling of Magmatic and Allied Processes. Society of Earth Scientists Series 83, 
    Springer, Berlin, pp 225-238
    \href{http://dx.doi.org/10.1007/978-3-662-46792-3}{doi: 10.1007/978-3-662-46792-3}
    
    Janousek V, Moyen JF, Martin H, Erban V, Farrow C (2016) Geochemical Modelling of Igneous Processes - 
    Principles and Recipes in R Language. Springer-Verlag, Berlin.
}

\keyword{manip}
\keyword{GCDkit}
\concept{Modelling}

\seealso{
     \link{fcDirect} 

     \link{pmDirect} 
     
     \link{PetroModel-class}
      
     \link{fc.reverse-class}
}

\author{
    Jean-Francois Moyen, \email{jfmoyen@gmail.com}
    
    Vojtech Janousek, \email{vojtech.janousek@geology.cz}
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
        3,6,byrow=F) 
    rownames(mins)<-c("ol","di","an")
    colnames(mins)<-c("SiO2","Al2O3","FeO","MgO","CaO","Na2O")
    print(mins)

    model<-fcReverseFractional(c0.lab="Sample1",cL.lab="Sample2",min.name="mins") 
    summary(model)
    
    
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
    
    model2<-fcReverseFractional(c0.lab="Sample1",cL.lab="Sample2",kd.name="kd") 
    summary(model2) 
}
