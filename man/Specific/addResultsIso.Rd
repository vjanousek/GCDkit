\name{addResultsIso}
\title{Append Sr-Nd isotopic data}
\alias{addResultsIso}

\section{Plugin}{SrNd.r}

\description{
    Appends the calculated isotopic parameters stored in the matrix '\code{init}'
    to the numeric data already in the system.
}

\usage{
    addResultsIso()
}

\value{
    Modifies the numeric data matrix('\code{WR}') to which it appends the following
    columns:

    \item{Age (Ma)}{Age in Ma}
    \item{87Sr/86Sri}{Initial \eqn{^{87}Sr/^{86}Sr}{87Sr/86Sr} ratios}
    \item{143Nd/144Ndi}{Initial \eqn{^{143}Nd/^{144}Nd}{143Nd/144Nd}  ratios}
    \item{EpsNdi}{Initial \eqn{\epsilon(Nd)} values}
    \item{TDM}{Single-stage depleted-mantle Nd model ages \emph{(Liew & Hofmann, 1988)}}
    \item{TDM.Gold}{Single-stage depleted-mantle Nd model ages \emph{(Goldstein et al., 1988)}}
    \item{TDM.2stg}{Two-stage depleted-mantle Nd model ages \emph{(Liew & Hofmann, 1988)}}
}

\references{
    Goldstein S L, O'Nions R K & Hamilton P J (1984) A Sm-Nd isotopic study of 
    atmospheric dusts and particulates from major river systems. Earth Planet Sci 
    Lett 70: 221-236
    \href{http://dx.doi.org/10.1016/0012-821X(84)90007-4}{doi: 10.1016/0012-821X(84)90007-4}

    Liew T C & Hofmann A W (1988) Precambrian crustal components, plutonic 
    associations, plate environment of the Hercynian Fold Belt of Central Europe: 
    indications from a Nd and Sr isotopic study. Contrib Mineral Petrol 98: 129-138
    \href{http://dx.doi.org/10.1007/BF00402106}{doi: 10.1007/BF00402106}
}

\seealso{
'\code{\link{addResults}}'
}

\keyword{manip}
\concept{Calculation}
\concept{Edit}
\concept{Isotopic}
\concept{Menu: Plugins}

\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}
