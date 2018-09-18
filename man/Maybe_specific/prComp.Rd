\name{prComp}
\title{Statistics: Principal components}
\alias{prComp}

\description{
    Performs principal components analysis (scaled variables, covariance or 
    correlation matrix) and plots a biplot \emph{(Gabriel, 1971)}.
}

\usage{
    prComp(comp.data=NULL,use.cov=FALSE,scale=TRUE,GUI=FALSE)
}

\arguments{
  \item{comp.data}{a numerical matrix; the data to be normalized. 
        Or just names of variables in the data matrix '\code{WR}'.}
  \item{use.cov}{logical; should be the covariance matrix used instead of correlation matrix?}
  \item{scale}{logical; the scalings applied to each variable.}
  \item{GUI}{logical; is the function called from a menu (GUI)?}
}

\details{
    Biplot aims to represent both the observations and variables of a data matrix on 
    a single bivariate plot  \emph{(Gabriel, 1971; Buccianti & Peccerillo, 1999)}. 

    In the biplots, the length of the individual arrows is proportional to the 
    relative variation of each variable. A comparable direction of two arrows implies 
    that both variables are positively correlated; the opposite one indicates a 
    strong negative correlation. When two links are perpendicular it indicates 
    independence of the two variables \emph{(Buccianti & Peccerillo, 1999)}. 

    If called from menu (GUI version), a list of major elements 
    (SiO2,TiO2,Al2O3,FeOt,MnO,MgO,CaO,Na2O,K2O) is assumed as a default, but 
    different variables can be specified by the function 
    '\code{\link{selectColumnsLabels}}'.
    
    The samples can be selected based on combination of three searching mechanisms 
    (by sample name/label, range or a Boolean condition) - see 
    \code{\link{selectSamples}} for details. 
}

\section{Warning}{
    Names of existing numeric data columns and not formulae involving these can be 
    handled at this stage. Only complete cases are used for the principal components 
    analysis. 
}

\value{
    Vector of the scores of the supplied data on the principal components 
    is stored in a variable 'results'. 
    Returns invisibly the complete output from the underlying function 
    '\code{princomp}'.
}

\references{
    Buccianti A & Peccerillo A (1999) The complex nature of potassic and 
    ultrapotassic magmatism in Central-Southern Italy: a multivariate analysis of 
    major element data. In: Lippard S J, Naess A, Sinding-Larsen R (eds) Proceedings 
    of the 5th Annual Conference of the International Association for Mathematical 
    Geology. Tapir, Trondheim, p. 145-150

    Gabriel K R (1971) The biplot graphical display of matrices with application to 
    principal component analysis. Biometrika 58: 453-467
}

\seealso{
    For further details on the used principal components algorithm and biplots, 
    see the R manual entries of '\code{\link{princomp}}' and '\code{\link{biplot.princomp}}'.
}

\keyword{multivariate}
\concept{Statistics}
\concept{Plot}
\concept{Menu: Calculations: Statistics}

\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}
