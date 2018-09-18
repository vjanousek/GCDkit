\encoding{latin1}
\name{Paulick}
\title{Al2O3/SiO2 - MgO/SiO2 (Paulick et al. 2006)}
\alias{Paulick}

\description{
    Assigns data for \eqn{Al_{2}O_{3}/SiO_{2}}{Al2O3/SiO2} vs. 
    \eqn{MgO/SiO_{2}}{MgO/SiO2} 
    binary diagram into Figaro template (list '\code{sheet}') 
    and appropriate values into '\code{x.data}' and '\code{y.data}'.
}

\usage{
Paulick()
}

\arguments{
None.
}

\details{
    According to \emph{Paulick et al. (2006)}, the global analyses of mantle 
    peridotites form a 'Terrestrial Array' in the binary plot 
    \eqn{Al_{2}O_{3}/SiO_{2}}{Al2O3/SiO2} vs. 
    \eqn{MgO/SiO_{2}}{MgO/SiO2}. This linear correlation reflects the successive magmatic
    depletion of a primitive mantle and highly depleted
    compositions are characterized by low 
    \eqn{Al_{2}O_{3}/SiO_{2}}{Al2O3/SiO2} values 
    (<0.01; \emph{Jagoutz et al. 1979; Hart and Zindler 1986}).
    
    \if{html}{\figure{Paulick.png}}
    \if{latex}{\figure{Paulick.png}{options: width=4.5in}}
}

\value{
  \item{sheet}{list with Figaro Style Sheet data}
  \item{x.data}{x coordinates}
  \item{y.data}{y coordinates}
}

\references{
    Hart SR, Zindler A (1986) In search of a Bulk-Earth composition. Chem Geol 57: 247-267
    \href{http://dx.doi.org/10.1016/0009-2541(86)90053-7}{doi: 10.1016/10.1016/0009-2541(86)90053-7} 

    Jagoutz E, Palme H, Baddenhausen H, Blum K, Cendales M, Dreibus G, Spettel B, Waenke H, Lorenz V (1979) 
    The abundances of major, minor and trace elements in the Earth's mantle as derived from primitive 
    ultramafic nodules. Geochim Cosmochim Acta Suppl 10: 2031-2050

    Paulick H, Bach W, Godard M, De Hoog JCM, Suhr G, Harvey J (2006) Geochemistry of abyssal 
    peridotites (Mid-Atlantic Ridge, 15o20'N, ODP Leg 209): implications for fluid/rock 
    interaction in slow spreading environments. Chem Geol 234: 179-210
    \href{http://dx.doi.org/10.1016/j.chemgeo.2006.04.011}{doi: 10.1016/j.chemgeo.2006.04.011} 
}

\author{
    Vojtech Janousek, \email{vojtech.janousek@geology.cz}
}

\examples{
# plot the diagram
# assuming a dataset is loaded, of course!
\dontrun{ 
plotDiagram("Paulick",FALSE,TRUE)
}
}

\seealso{
\link{figaro} \link{plotDiagram}
}

\keyword{hplot} 
\concept{Geotectonic} 
\concept{Plot} 
\concept{Menu: Geotectonic}
