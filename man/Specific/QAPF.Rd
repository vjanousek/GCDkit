\name{QAPF}
\alias{QAPF}
\alias{QAPFVolc}
\alias{QAPFPlut}
\title{QAPF diagram (Streckeisen 1974, 1978)}
\description{
Assigns data for Streckeisen's diagram into Figaro template (list '\code{sheet}') and                                   
appropriate values into '\code{x.data}' and '\code{y.data}'. The Q, A, P and F 
coordinates are assigned into matrix '\code{results}'.    
}

\usage{
QAPFVolc()
QAPFPlut()
}

\details{
Following the IUGS recommendation (Le Maitre et al 2002), the QAPF diagram should 
be the prime classification scheme for holocrystalline plutonic and volcanic 
rocks containing at least 10\% of felsic minerals. 

\if{html}{\figure{QAPFVolc.png}} 
\if{latex}{\figure{QAPFVolc.png}{options: width=4.5in}}

The apices are defined as follows:

Q = Quartz modal \%

A = Alkali feldspar modal \%

P = Plagioclase modal \%

F = feldspathoid modal \%

Q + A + P + F = 100 \%
  
As the whole QAPF diagram is rather complicated, GCDkit plots just the appropriate 
triangle if the dataset contains only Si-oversaturated or only Si-undersaturated 
rock samples. If both kinds of rock samples are present, the whole double triangle 
is shown. This behaviour may be changed in the source code of the diagram (in 
file '\code{QAPFPlut.r}' or '\code{QAPFVolc.r}', stored in the subdirectory 
GCDkit\\Diagrams\\Classification, change the '\code{triangle<-"auto"}' to 
'\code{triangle<-"both"}' and complete double triangle will be always plotted).

\if{html}{\figure{QAPFPlut.png}} 
\if{latex}{\figure{QAPFPlut.png}{options: width=4.5in}}

}
\value{
  \item{sheet}{list with Figaro Style Sheet data}
  \item{x.data, y.data}{Q, A, P and F data (see details) transformed to orthogonal coordinates}
}

\references{
Streckeisen A (1974) Classification and nomenclature of plutonic rocks. Geol 
Rundsch 63: 773-786
\href{http://dx.doi.org/10.1007/BF01820841}{doi: 10.1007/BF01820841} 

Streckeisen A (1978) IUGS Subcommission on the Systematics of Igneous Rocks: 
Classification and nomenclature of volcanic rocks, lamprophyres, carbonatites 
and melilitic rocks; recommendation and suggestions. 
Neu Jb Min, Abh 134: 1-14.

Le Maitre R. W. et al. (2002) Igneous Rocks. A Classification and Glossary of Terms. 
2nd edition. Cambridge University Press.
}

\author{Vojtech Erban, \email{vojtech.erban@geology.cz}}

\seealso{
\link{classify}
\link{figaro}
\link{plotDiagram}
}

\examples{
#plots the QAPF diagram for current dataset

plotDiagram("QAPFVolc", FALSE)

plotDiagram("QAPFPlut", FALSE)

#classifies the current dataset using the QAPF diagram

classify("QAPFVolc")

classify("QAPFPlut")

}
\keyword{hplot}
\concept{Menu: Plots}
\concept{Plots}
\concept{Plot}
