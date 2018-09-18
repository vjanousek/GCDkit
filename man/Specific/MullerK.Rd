\encoding{utf8}
\name{MullerK}
\title{Muller et al. (1992) potassic igneous rocks discrimination}
\alias{MullerKbinary}
\alias{MullerKternary}

\description{ Assigns Figaro templates to geotectonic diagrams for potassic igneous rocks of \emph{Müller et al. (1992)} into the list
'\code{plate}') and appropriate values into the list '\code{plate.data}' for subsequent plotting. }

\usage{
MullerKbinary(plot.txt=getOption("gcd.plot.text"))
MullerKternary(plot.txt=getOption("gcd.plot.text"))
}

\arguments{
    \item{plot.txt}{logical, annotate fields by their names?}
}

\details{
    Suite of binary and ternary diagrams for discrimination of geotectonic environment of potassic
    igneous rocks, proposed by \emph{Müller et al. (1992)} and \emph{Müller & Groves (1995)}.
    Following geotectonic settings may be deduced:

    \tabular{ll}{
        Abbreviation used \tab Environment \cr
        CAP \tab \emph{Continental Arc} \cr
        PAP \tab \emph{Postcollisonal Arc} \cr
        IOP \tab \emph{Initial Oceanic Arc} \cr
        LOP \tab \emph{Late Oceanic Arc} \cr
        WIP \tab \emph{Within Plate} \cr
    }

    \if{html}{\figure{MullerKbinary.png}}
    \if{latex}{\figure{MullerKbinary.png}{options: width=5.5in}}

\if{html}{\figure{MullerKternary.png}}
\if{latex}{\figure{MullerKternary.png}{options: width=6in}}
}

\section{Note}{
    This function uses the plates concept. The individual plots can be selected and their
    properties/appearance changed as if they were stand alone Figaro-compatible plots.
    See \code{\link{Plate}}, \code{\link{Plate editing}} and
    \code{\link{figaro}} for details.
}

\references{
Müller D, Rock NMS, Groves DI (1992) Geochemical discrimination between shoshonitic and
potassic volcanic rocks in different tectonic
settings: a pilot study. Mineral Petrol 46: 259-289
\href{http://dx.doi.org/10.1007/BF01173568}{doi:10.1007/BF01173568}

Müller D, Groves DI (1995) Potassic Igneous Rocks and Associated Gold-Copper Mineralization.
Springer, Berlin, pp 1-210 }

\examples{
plotPlate("MullerKbinary")

plotPlate("MullerKternary")
}

\seealso{
     \link{Plate}, \link{Plate editing},  \link{plotPlate},   \link{figaro}
}

\author{Vojtěch Janoušek, \email{vojtech.janousek@geology.cz}}

\keyword{hplot}
\concept{Geotectonic}
\concept{Plot}
\concept{Menu: Geotectonic}
