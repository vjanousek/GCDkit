\name{Sylvester}
\Rdversion{1.1}
\alias{Sylvester}
\title{Sylvester (1989)}

\description{
    Assigns data for a binary plot 
    \eqn{(Al_{2}O_{3}+CaO)/(FeOt+Na_{2}O+K_{2}O)}{(Al2O3+CaO)/(FeOt+Na2O+K2O)}
    vs. \eqn{100*(MgO+FeOt+TiO_{2})/SiO_{2}}{100*(MgO+FeOt+TiO2)/SiO2}, 
    proposed by \emph{Sylvester (1989)} to distinguish the 
    alkaline collision-related alkaline granites
    into Figaro template (list '\code{sheet}') and appropriate values 
    into '\code{x.data}' and '\code{y.data}'.  
}

\usage{
Sylvester()
}

\details{
In the plot \eqn{(Al_{2}O_{3}+CaO)/(FeOt+Na_{2}O+K_{2}O)}{(Al2O3+CaO)/(FeOt+Na2O+K2O)} 
vs. \eqn{100*(MgO+FeOt+TiO_{2})/SiO_{2}}{100*(MgO+FeOt+TiO2)/SiO2} of 
\emph{Sylvester (1989)} can be distinguished 'Alkaline'collision-related granites, from 
'Calc-alkaline & Strongly peraluminous' types (solid line). The strongly fractionated 
calc-alkaline varieties are separated by the dashed line.

    \if{html}{\figure{Sylvester.png}} 
    \if{latex}{\figure{Sylvester.png}{options: width=4.5in}} 

Note that only samples with \eqn{SiO_{2}}{SiO2} > 68 wt. \% are plotted.
}

\value{
  \item{sheet}{list with Figaro Style Sheet data}
  \item{x.data}{(Al2O3+CaO)/(FeOt+Na2O+K2O) [wt. \%]}
  \item{y.data}{100*(MgO+FeOt+TiO2)/SiO2 [wt. \%]}
}

\references{
    Sylvester P J (1989) Post-collisional alkaline granites. 
    J Geol 97: 261-280. \href{http://dx.doi.org/10.1086/629302}{doi: 10.1086/629302}
}

\examples{
#plot the diagram
plotDiagram("Sylvester", FALSE)
}

\seealso{
\link{figaro}
\link{plotDiagram}
}

\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}

\keyword{hplot}
\concept{Geotectonic}
\concept{Plot}
\concept{Menu: Geotectonic}
