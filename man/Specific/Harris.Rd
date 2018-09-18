\name{Harris}
\title{Harris et al. (1986) Hf-Rb/30-Ta*3}
\alias{Harris}

\description{
    Assigns data for the Hf-Rb/30-Ta*3 ternary diagram of 
    \emph{Harris et al. (1986)} into Figaro template (list '\code{sheet}') 
    and appropriate values into '\code{x.data}' and '\code{y.data}'.  
}

\usage{
Harris()
}

\details{
Triangular diagram with apices Hf, Rb/30 and Ta*3, proposed by \emph{Harris et al. (1986)}
for classification of collisional granites.

Following fields are defined:

\tabular{ll}{
VA \tab \emph{Volcanic-Arc granites (Group 1, VA)}\cr
WP \tab \emph{Within-Plate granites (Group 4, WP)}\cr
Group 2 \tab \emph{}\cr
Group 3 \tab \emph{}\cr
}

\if{html}{\figure{Harris.png}} 
\if{latex}{\figure{Harris.png}{options: width=4.5in}}

Quoting from their abstract:

(i) Group 1 - \emph{Pre-collision calc-alkaline (volcanic-arc) intrusions} which are mostly 
derived  from mantle modified by a subduction component and which are characterized by 
selective enrichments in LIL elements.

(ii) Group 2 - \emph{Syn-collision peraluminous intrusions (leucogranies)} which may be 
derived from the hydrated bases of continental thrust sheets and which are characterized 
by high Rb/Zr and Ta/Nb and low K/Rb ratios.

(iii) Group 3 - \emph{Late or post-collision calc-alkaline intrusions} which may be 
derived from a mantle source but undergo extensive crustal contamination and can only be 
distinguished from volcanic-arc intrusions by their higher ratios of Ta/Hf and Ta/Zr.

(iv) Group 4 - \emph{Post-collision alkaline intrusions} which may be derived from mantle 
lithosphere beneath the collision zones and which carry high concentrations of both 
LIL and HFS elements.
}

\value{
  \item{sheet}{list with Figaro Style Sheet data}
  \item{x.data, y.data}{Th, Hf/3 and Ta in ppm recalculated into two dimensions}
}

\references{
Harris N B W, Pearce J A, Tindle A G (1986) Geochemical characteristics of 
collision-zone magmatism. In: Coward M P, Ries A C (eds) Collision Tectonics. 
Geological Society London Special Publication 19, pp 67-81
}

\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}

\examples{
#plot the diagram
plotDiagram("Harris", FALSE)
}

\seealso{
\link{figaro}
\link{plotDiagram}
}

\keyword{hplot}
\concept{Geotectonic}
\concept{Plot}
\concept{Menu: Geotectonic}
