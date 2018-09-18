\name{selectColumnsLabels}
\alias{selectColumnsLabels}

\title{Selecting several data columns }

\description{
An auxiliary function invoked by many others to select several variables 
simultaneously.    
}

\usage{
selectColumnsLabels(where = colnames(WR), 
message = "Select variable(s), e.g. 'SiO2,TiO2,MgO'
or press ENTER to pick from a list", default = "", print = TRUE, 
exact.only = TRUE)
}

\arguments{
  \item{where}{vector of names for data columns to choose from}
  \item{message}{prompt}
  \item{default}{comma delimited list of default names}
  \item{print}{logical, echo on/off}
  \item{exact.only}{logical, should be the input checked for correctness?}
}

\details{
The variable(s) can be specified in several ways. The easiest is to type 
directly the name(s) of the  column(s), separated by commas. Alternatively can 
be used their sequence numbers or ranges. Also built-in lists can be employed, 
such as \code{'LILE', 'REE', 'major'} and \code{'HFSE'} or their combinations 
with the column names. 

These lists are simple character vectors, and additional ones can be built by 
the user (see Examples). Note that currently only a single, stand-alone, 
user-defined list can be employed as a search criterion.

Empty response invokes list of all variables available. The correct variables 
have to be selected by mouse + SHIFT from this list.

If \code{exact.only=TRUE}, the individual items in the input line are checked 
against the list of existing column/variable names (i.e. components in the vector 
\code{'where'}).
}

\value{
  Vector with the selected column names.
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
Locality, SiO2, Rb, Sr, Ba, K, Cs, Li, Nb, 
Zr, Hf, Ti, Ta, La, Ce, Y, Ga, Sc, Th, U

Search pattern = 1:5, 7  
Numeric data columns number 1, 2, ...5, 7

# User-defined list
my.elems<-c("Rb","Sr","Ba")
Search pattern = my.elems
Rb, Sr, Ba
}
}

\keyword{manip}
\keyword{database}
\concept{Subsetting}
\concept{Edit}
\concept{Menu: Data handling}

\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}
