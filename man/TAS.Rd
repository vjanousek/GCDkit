\name{TAS}
\alias{TAS}
\alias{TASadd}
\title{IUGS recommended TAS (Le Bas et al. 1986)}

\description{
Assigns data for IUGS recommended TAS diagram into Figaro template 
(list '\code{sheet}') and appropriate values into '\code{x.data}' and '\code{y.data}'
}

\usage{
TAS(cutoff=95)
}

\arguments{
  \item{cutoff}{numeric; the minimal sum of the analysis to be considered for classification}
}

\details{
TAS diagram, as proposed by \emph{Le Bas et al. (1986)}, codified by \emph{Le Maitre
et al. (1989)} and slightly modified by \emph{Le Bas (2000)}.

    \if{html}{\figure{TAS.png}} 
    \if{latex}{\figure{TAS.png}{options: width=4.5in}} 
    
The diagram (in its basic form) defines following fields:

 \emph{foidite}\cr
 \emph{picrobasalt}\cr
 \emph{basalt}\cr
 \emph{basaltic andesite}\cr
 \emph{andesite}\cr
 \emph{dacite}\cr
 \emph{rhyolite}\cr
 \emph{trachybasalt}\cr
 \emph{basaltic trachyandesite}\cr
 \emph{trachyandesite}\cr
 \emph{trachyte/trachydacite}\cr
 \emph{tephrite/basanite}\cr
 \emph{phonotephrite}\cr
 \emph{tephriphonolite}\cr
 \emph{phonolite}

This primary division is further enhanced by 
the '\code{TASadd}' routine
(called automatically by 
'\code{classify}').

Following actions are carried out:
\itemize{
\item Analyses with \eqn{H_{2}O}{H2O} > 2 and \eqn{CO_{2}}{CO2} > 0.5 (weight 
percent) are filtered out

\item \emph{Trachybasalt} is subdivided into \emph{hawaiite} and 
\emph{potassic trachybasalt}

\item \emph{Basaltic trachyandesite} is subdivided into \emph{mugearite} and 
\emph{shoshonite}

\item \emph{Trachyandesite} is subdivided into \emph{benmoreite} and \emph{latite}

\item High-Mg rocks are split into \emph{picrite, komatiite, meimechite} and 
\emph{boninite}
}

Note that systematics of high-Mg rocks follows revised IUGS Recommendations
\emph{(Le Bas et al., 2000; Le Maitre et al. 2002)} which differ from their 1st
edition \emph{(Le Maitre et al, 1989)}.
Further subdivisions recommended by \emph{Le Maitre et al. (1989)} are not
implemented in GCDkit, mainly for poorly defined CIPW version used by the Subcommission.
}

\value{
  \item{x.data}{SiO2 data recast to anhydrous sum (matrix 'WRanh')}
  \item{y.data}{Na2O+K2O data recast to anhydrous sum (matrix 'WRanh')}
  \item{sheet}{list with Figaro Style Sheet data}
  \item{results}{matrix with classification results}
  \item{groups}{vector with classification results}
  \item{grouping}{set to -1}
}

\references{
Le Bas M J, Le Maitre R W, Streckeisen A & Zanettin B (1986)
A chemical classification of volcanic rocks based on the total alkali-silica
diagram. J Petrology 27: 745-750
\href{http://dx.doi.org/10.1093/petrology/27.3.745}{doi: 10.1093/petrology/27.3.745} 

Le Bas M J (2000) IUGS Reclassification of the High-Mg and Picritic Volcanic
Rocks. J Petrology 41: 1467-1470
\href{http://dx.doi.org/10.1093/petrology/41.10.1467}{doi: 10.1093/petrology/41.10.1467} 

Le Maitre R W et al (1989) Igneous Rocks: A Classification and Glossary of Terms,
1st edition. Cambridge University Press

Le Maitre R W et al (2002) A Classification and Glossary of Terms, 1st edition.
Cambridge University Press
}

\author{
  Vojtech Erban, \email{vojtech.erban@geology.cz}\cr 
  & Vojtech Janousek, \email{vojtech.janousek@geology.cz}
}

\seealso{
\link{classify}
\link{figaro}
\link{plotDiagram}
}

\examples{
#Within GCDkit, the plot is called using following auxiliary functions:
#To Classify data stored in WR (Groups by diagram)
classify("TAS")

#To plot data stored in WR or its subset (menu Classification)
plotDiagram("TAS", FALSE)
}

\keyword{hplot}
\concept{Plot}
\concept{Grouping}
\concept{Edit}
\concept{Menu: Data handling}
\concept{Menu: classification}
