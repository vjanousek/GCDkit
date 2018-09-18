\name{printSamples}
\title{Display samples}
\alias{printSamples}

\description{
Displays specified combination of numeric variable(s) and/or labels for selected 
range of samples.
}

\usage{
printSamples(elems=NULL,which=NULL,select.samples=FALSE,print=TRUE)
}

\arguments{
  \item{elems}{list of variables to be printed}
  \item{which}{list of samples, useful only for select.samples=FALSE}
  \item{select.samples}{logical: if TRUE, samples can be chosen using the appropriate dialogue}
  \item{print}{logical: should be the result indeed printed or just returned for further evaluation?}
}

\details{
This function prints the desired numerical columns, textual labels, or their 
combinations, for selected samples. 

The samples can be selected based on combination of three searching mechanisms 
(by sample name/label, range or a Boolean condition) - see 
\code{\link{selectSamples}} for details. 

The variables to be printed are chosen by the function 
'\code{\link{selectColumnsLabels}}'. In the specification of the variable can 
be used also arithmetic expressions, see \code{\link{calcCore}} for the correct 
syntax.
}

\value{
    \item{results}{data matrix with the desired data for the specified samples}
}

\examples{
\dontrun{ 
# Querying names of numeric data columns

Search pattern = SiO2, MgO, CaO 

Search pattern = major
SiO2, TiO2, Al2O3, Fe2O3, FeO, MnO, MgO, CaO, Na2O, K2O, P2O5 

Search pattern = LILE
Rb, Sr, Ba, K, Cs, Li

Search pattern = HFSE
Nb, Zr, Hf, Ti, Ta, La, Ce, Y, Ga, Sc, Th, U

Search pattern = REE
La, Ce, Pr, Nd, Sm, Eu, Gd, Tb, Dy, Ho, Er, Tm, Yb, Lu

Search pattern = Locality,SiO2,LILE,HFSE
Locality, SiO2, Rb, Sr, Ba, K, Cs, Li, Nb, Zr, Hf, Ti, 
Ta, La, Ce, Y, Ga, Sc, Th, U

Search pattern = 1:5, 7  
Numeric data columns number 1, 2, ...5, 7

# User-defined list
my.elems<-c("Rb","Sr","Ba")
Search pattern = my.elems
Rb, Sr, Ba
}
}

\keyword{print}
\concept{Calculation}
\concept{Print}
\concept{Menu: Data handling}
\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}
