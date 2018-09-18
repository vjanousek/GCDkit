\name{saveResultsIso}
\title{Save Sr-Nd isotopic data}
\alias{saveResultsIso}

\section{Plugin}{SrNd.r}

\description{
Saves the calculated isotopic parameters stored in the matrix '\code{init}' to a 
text file.
}

\usage{
saveResultsIso(digits = 6)
}

\arguments{
\item{digits}{precision of the results to be saved.}
}

\details{
Saves the data matrix \code{init} with the following columns:

\tabular{ll}{
Age (Ma)\tab Age in Ma\cr
87Sr/86Sri\tab Initial Sr isotopic ratios\cr
143Nd/144Ndi\tab Initial Nd isotopic ratios\cr
EpsNdi\tab Initial \eqn{\epsilon(Nd)} values\cr
TDM\tab Single-stage depleted-mantle Nd model ages \emph{(Liew & Hofmann, 1988)}\cr
TDM.Gold\tab Single-stage depleted-mantle Nd model ages \emph{(Goldstein et al., 1988)}\cr
TDM.2stg\tab Two-stage depleted-mantle Nd model ages \emph{(Liew & Hofmann, 1988)}\cr
}
}

\value{
None.
}

\references{
Liew T C & Hofmann A W (1988) Precambrian crustal components, plutonic associations, 
plate environment of the Hercynian Fold Belt of Central Europe: indications from 
a Nd and Sr isotopic study. Contrib Mineral Petrol 98: 129-138

Goldstein S L, O'Nions R K & Hamilton P J (1984) A Sm-Nd isotopic study of 
atmospheric dusts and particulates from major river systems. Earth Planet Sci 
Lett 70: 221-236
}

\seealso{
'\code{\link{saveResults}}'
}

\keyword{file}
\concept{File}
\concept{Isotopic}
\concept{Menu: GCDkit}
\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}
