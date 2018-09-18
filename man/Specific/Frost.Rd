\name{Frost}
\alias{Frost}
\title{Frost et al. (2001)}

\description{
Classification of granitic rocks proposed by Frost et al (2001). 
}

\usage{
    Frost(plot.txt = getOption("gcd.plot.text"), 
    clssf = FALSE, GUI = FALSE)
}

\arguments{
  \item{plot.txt}{logical, annotate fields by their names?}
  \item{clssf}{logical, should the samples be classified?}
  \item{GUI}{logical, is the function called from a GUI?}
}

\details{
Classification scheme proposed by \emph{Frost et al. (2001)}. It consists of 
three diagrams:
\itemize{
\item \eqn{Fe\ number}{Fe number} vs. \eqn{SiO_{2}}{SiO2}. 
    Note, that the Fe-number is calculated as weight proportion 
    of \eqn{FeO/(FeO+MgO)}{FeO/(FeO+MgO)} 
    (or \eqn{FeO_{tot}/(FeO_{tot}+MgO)}{FeOtot/(FeOtot+MgO)}), see below). 
    The approach used here should not be confused with the more common usage
    of the term "Fe-number" (as well as "Mg-number") as molecular proportions. 

\item \eqn{Na_{2}O+K_{2}O-CaO}{Na2O+K2O-CaO} vs. \eqn{SiO_{2}}{SiO2} (in wt. \%). 

\item \eqn{A/NK}{A/NK} vs. \eqn{ASI}{ASI}, where \eqn{A/NK}{A/NK} stands for molecular 
    \eqn{Al_{2}O_{3}/(Na_{2}O+K_{2}O)}{Al2O3/(Na2O+K2O)}, and ASI for molecular
    \eqn{Al_{2}O_{3}/(Na_{2}O+K_{2}O+CaO-3.33 P_{2}O_{5})}{Al2O3/(Na2O+K2O+CaO-3.33 P2O5)}.
    In fact, it is the A/CNK parameter of \emph{Shand (1943)}, corrected for the Ca 
    content in apatite. As approved by one of the authors \emph{(C. Barnes, 
    pers. comm., 2008)}, the equation for ASI in  the original work
    \emph{(Frost et al. 2001)} was stated erroneously in molecular proportions 
    of elements, instead of oxides. In fact,
    this diagram was not plotted in the paper, but it replaces the conditions
    mentioned in the text and is in our view more instructive.    
}

The classification is designed to work both with analyses distinguishing between 
ferrous and ferric iron (preferred) and those with total iron only. The dialogue 
box lets the user decide, whether to use the ferrous iron value or the total iron.
 
Similarly, if some \eqn{P_{2}O_{5}}{P2O5} concentration are missing in the dataset, 
the user is prompted whether the missing values should be replaced with zero. 
If not, the problematic analyses are not plotted/classified.
}

\section{Note}{
Due to the specific design of this classification (combination of multiple diagrams), 
the classification option is not available via the pull-down menus. 
Currently, the only way to apply Frost's classification in GCDkit on 
individual samples is to call the function manually
from the Console (\code{Frost(clssf = TRUE)}). 
}


\value{
The function returns table of calculated coefficients (Fe-Number, MALI, ASI). There 
are two values for the ASI: one labeled \code{'ASI'} is calculated from molecular 
proportions of oxides, and is used for plotting and classification.
The other one is labeled \code{'ASI_orig'}, and is calculated exactly as stated 
in the original paper (i.e. \eqn{Al/(Ca-1.67P+Na+K)}{Al/(Ca-1.67P+Na+K)}.

The following associations are distinguished:

\emph{ferroan}

\emph{magnesian}

As well as:

\emph{alkalic}

\emph{alkali-calcic}

\emph{calc-alkalic}

\emph{calcic}

Or:

\emph{peralkaline}

\emph{metaluminous}

\emph{peraluminous}

\if{html}{\figure{Frost.png}} 
\if{latex}{\figure{Frost.png}{options: width=5.5in}}

The geologically reasonable combinations, together with examples, 
are listed in the 
\url{../doc/FrostTable.html}, 
modified from the original article.
}

\section{Note}{
    This function uses the plates concept. The individual plots can be selected and their 
    properties/appearance changed as if they were stand alone Figaro-compatible plots. 
    See \code{\link{Plate}},
     
    \code{\link{Plate editing}} and 
    \code{\link{figaro}} for details.
}

\references{
Frost B R, Barnes C G, Collins W J, Arculus R J, Ellis D J, Frost C D (2001)
A geochemical classification for granitic rocks. J Petrol 42: 2033-2048. 
\href{http://dx.doi.org/10.1093/petrology/42.11.2033}{doi: 10.1093/petrology/42.11.2033}

Shand, S J (1943) Eruptive rocks, 2nd ed. John Wiley, New York, pp 1-444 
}

\author{
  Vojtech Erban, \email{vojtech.erban@geology.cz} 
  
  & Vojtech Janousek \email{vojtech.janousek@geology.cz}
}

\seealso{
\link{classify}
\link{Shand}
\link{Plate}
\link{Plate editing}
\link{plotPlate}
\link{figaro}
}

\examples{
#plot the diagrams
plotPlate("Frost")

#classify the samples, suppress the graphical output
Frost(clssf = TRUE)
}


\keyword{hplot}
\concept{Plot}
\concept{Menu: Geotectonic}
