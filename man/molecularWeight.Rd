\name{Molecular weights}
\alias{molecularWeight}
\title{Calculating molecularWeights of oxides}

\description{
    These functions plot multiple binary plots with a common x axis, such as Harker 
    plots.
}

\usage{
molecularWeight(formula) 
}

\arguments{
    \item{formula}{a character vector of length 1, a formula of the oxide.}
}

\details{
    So far only simple oxide formulae in form of \eqn{A_{x}O_{y}}{AxOy} (where x, y are optional indexes)
    can be handled. The atomic weights are stored in a file \code{MW.data}. The atomic weights come from
    official CIAAW  web site \href{http://www.ciaaw.org}{http://www.ciaaw.org}.  
}

\value{
    A list with items:
    \tabular{ll}{
    MW \tab molecularWeight \cr
    x.atoms \tab number of atoms in the formula\cr
    x.oxygen \tab number of oxygens
    }
}

\references{
    Commission on Isotopic Abundances and Atomic Weights (CIAAW) of the 
    International Union of Pure and Applied Chemistry. 
    Accessed on January 8, 2016, at 
    \href{http://www.ciaaw.org}{http://www.ciaaw.org} 
}


\examples{
molecularWeight("SiO2")

molecularWeight("SiO2")[[1]]

oxides<-c("SiO2","TiO2","Al2O3","Fe2O3","FeO")
sapply(oxides,molecularWeight)
}

\keyword{hplot}
\concept{Plot}
\concept{Menu: Plots}

\author{
Vojtech Janousek, \email{vojtech.janousek@geology.cz}
Vojtech Erban, \email{vojtech.erban@geology.cz}
}
