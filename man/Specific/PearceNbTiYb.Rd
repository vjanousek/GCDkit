\name{Pearce Nb-Ti-Yb}
\title{Pearce (2008) Nb/Yb-TiO2/Yb diagram}
\alias{PearceNbTiYb}

\description{
    Assigns data for a \eqn{TiO_{2}}{TiO2}/Yb vs. Nb/Yb diagram  
    into Figaro template (list '\code{sheet}') and appropriate values 
    into '\code{x.data}' and '\code{y.data}'. 
}

\usage{
PearceNbTiYb(reservoirs=TRUE,xmin=0.1,xmax=100,ymin=0.1,ymax=10)
}

\arguments{
    \item{reservoirs}{logical, should be plotted average NMORB, EMORB and OIB?}
    \item{xmin,xmax}{numeric, limits for the x axis.}
    \item{ymin,ymax}{numeric, limits for the y axis.}
}

\details{
    The diagram \eqn{TiO_{2}}{TiO2}/Yb vs. Nb/Yb   serves as 'melting depth proxy'
    and hence for indicating mantle temperature and thickness of the conductive lithosphere \emph{(Pearce 2008)}. 
    It distinguishes basalts, which have originated by shallow melting, out of garnet stability 
    field ('MORB array') from those spanning from deep melting with garnet in the residue ('OIB array').
    Plotted for reference are average compositions of NMORB, EMORB and OIB taken 
    from \emph{Sun and McDonough (1989)}. 

    \if{html}{\figure{PearceNbTiYb.png}}
    \if{latex}{\figure{PearceNbTiYb.png}{options: width=4.5in}}
}

\value{
  \item{sheet}{list with Figaro Style Sheet data}
  \item{x.data}{Nb/Yb}
  \item{y.data}{TiO2/Yb}
}

\references{
Pearce JA (2008) Geochemical fingerprinting of oceanic basalts with applications to 
ophiolite classification and the search for Archean oceanic crust. Lithos 100: 14-48
\href{http://dx.doi.org/10.1016/j.lithos.2007.06.016}{doi:10.1016/j.lithos.2007.06.016}	

Sun SS, McDonough WF (1989) Chemical and isotopic systematics of oceanic 
basalts: implications for mantle composition and processes. In: Saunders AD, 
Norry M (eds) Magmatism in Ocean Basins. Geological Society of London Special 
Publications 42, pp 313-345
}

\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}

\examples{
# Plot the diagram
plotDiagram("PearceNbTiYb",FALSE,FALSE,reservoirs=TRUE)

plotDiagram("PearceNbTiYb",FALSE,FALSE,reservoirs=FALSE)
}

\seealso{
\link{figaro}
\link{plotDiagram}
\link{PearceNbThYb}
}

\keyword{hplot}
\concept{Geotectonic}
\concept{Plot}
\concept{Menu: Geotectonic}

