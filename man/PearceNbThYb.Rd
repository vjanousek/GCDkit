\encoding{latin1} \name{Pearce Nb-Th-Yb}
\title{Pearce (2008) Nb/Yb-Th/Yb diagram}
\alias{PearceNbThYb}

\description{
    Assigns data for a Th/Yb vs. Nb/Yb diagram
    into Figaro template (list '\code{sheet}') and appropriate values
    into '\code{x.data}' and '\code{y.data}'.
}

\usage{
PearceNbThYb(reservoirs=TRUE,xmin=0.1,xmax=1000,ymin=0.01,ymax=100)
}

\arguments{
    \item{reservoirs}{logical, should be plotted average NMORB, EMORB and OIB?}
    \item{xmin,xmax}{numeric, limits for the x axis.}
    \item{ymin,ymax}{numeric, limits for the y axis.}
}

\details{
    This diagram (Th/Yb vs. Nb/Yb) has been developed by J. Pearce in the 2000s
    to characterize (and discriminate) arc magmatism. The current version is based on
    paper by \emph{Pearce (2008)} dealing with oceanic basalts, though.
    According to this author, Th-Nb serves as a 'crustal input proxy' and hence for
    demonstrating an oceanic, non-subduction setting.

    The 'MORB-OIB array' at the bottom extends from N-MORB to OIB (plotted for reference are
    average compositions of NMORB, EMORB and OIB taken from \emph{Sun and McDonough (1989)}.
    Melting of the metasomatized mantle yields trends parallel to the mantle array.

    Arc lavas, formed by fluxed melting of the mantle, are shifted above the
    mantle array; the same effects have mantle-derived magma-crust interactions.
    The top dashed line is the outer limit of typical arc lavas, but there is a great
    deal of variation.

    \if{html}{\figure{PearceNbThYb.png}}
    \if{latex}{\figure{PearceNbThYb.png}{options: width=4.5in}}
}

\value{
  \item{sheet}{list with Figaro Style Sheet data}
  \item{x.data}{Nb/Yb}
  \item{y.data}{Th/Yb}
}

\references{ Pearce JA (2008) Geochemical fingerprinting of oceanic basalts with applications to ophiolite classification and the search for Archean
oceanic crust. Lithos 100: 14-48 \href{http://dx.doi.org/10.1016/j.lithos.2007.06.016}{doi:10.1016/j.lithos.2007.06.016}	

Sun SS, McDonough WF (1989) Chemical and isotopic systematics of oceanic basalts: implications for mantle composition and processes. In: Saunders AD,
Norry M (eds) Magmatism in Ocean Basins. Geological Society of London Special Publications 42, pp 313-345 }

\author{
    Vojtech Janousek, \email{vojtech.janousek@geology.cz} and
    Jean-Francois Moyen, \email{jfmoyen@gmail.com}
}

\examples{
# Plot the diagram
plotDiagram("PearceNbThYb",FALSE,FALSE,reservoirs=TRUE)
plotDiagram("PearceNbThYb",FALSE,FALSE,reservoirs=FALSE)
}

\seealso{ \link{figaro} \link{plotDiagram} \link{PearceNbTiYb} }

\keyword{hplot} \concept{Geotectonic} \concept{Plot} \concept{Menu: Geotectonic}
