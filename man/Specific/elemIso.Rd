\name{elemIso}
\title{Binary plot of a WR geochemical parameter vs isotopic ratio}
\alias{elemIso}

\section{Plugin}{SrNd.r}

\description{
Plots a diagram of a selected whole-rock geochemical parameter vs initial Sr 
isotopic ratios or initial \eqn{\epsilon(Nd)} for selected samples.
    \if{html}{\figure{elemIso.png}} 
    \if{latex}{\figure{elemIso.png}{options: width=4in}}
}

\usage{
    elemIso(xlab=NULL,what=NULL,GUI=FALSE,...)
}

\arguments{
    \item{xlab}{variable name or a formula for the x axis; if NULL a dialogue is displayed}
    \item{what}{name of the desired isotopic parameter}
    \item{GUI}{logical; is the function called from the GUI?}  
    \item{...}{optional parameters to the underlying function \code{{plotWithLimits}}}
}

\details{
    The variable to be plotted as x axis is based on whole-rock geochemical data.
    If not specified upon the function call, it is selected using the function 
    '\code{\link{selectColumnLabel}}'. In the specification of the variable can 
    be used also an arithmetic expression, see \code{\link{calcCore}} for the correct 
    syntax.

    The plotted isotopic parameters (y axis) can be one of:

    \tabular{ll}{
    \bold{Menu item} \tab \bold{Explanation}\cr
    87Sr/86Sri\tab Initial Sr isotopic ratios\cr
    143Nd/144Ndi\tab Initial Nd isotopic ratios\cr
    EpsNdi\tab Initial \eqn{\epsilon(Nd)} values\cr
    1 stg DM model ages \emph{(Goldstein et al. 1988)}\tab Single-stage DM Nd model ages\cr
    1 stg DM model ages \emph{(Liew & Hofmann 1988)}\tab Single-stage DM Nd model ages\cr
    2 stg DM model ages \emph{(Liew & Hofmann 1988)}\tab Two-stage DM Nd model ages
    }

    If called from GUI, the samples can be selected based on combination of three searching mechanisms 
    (by sample name/label, range or a Boolean condition) - see \code{\link{selectSamples}} 
    for details. 
}

\value{
    None.
}

\seealso{
    The actual plotting is done by the function \code{\link{plotWithLimits}}.
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

\keyword{hplot}
\concept{Plot}
\concept{Isotopic}
\concept{Menu: Plugins}

\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}
