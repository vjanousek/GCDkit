\name{reciprocalIso}
\title{Binary plots of reciprocal element concentration vs initial isotopic composition}
\alias{reciprocalIso}

\section{Plugin}{SrNd.r}

\description{
    Plots a diagram 1/Sr vs initial Sr isotopic ratios or 1/Nd vs initial \eqn{\epsilon(Nd)} 
    for selected samples.
    \if{html}{\figure{reciprocalIso.png}} 
    \if{latex}{\figure{reciprocalIso.png}{options: width=4in}}
}

\usage{
    reciprocalIso(what=NULL,GUI=FALSE,...)
}

\arguments{
    \item{what}{name of the desired isotopic parameter}
    \item{GUI}{logical; is the function called from the GUI?}  
    \item{...}{optional parameters to the underlying function \code{{plotWithLimits}}}
}

\details{
    The recognized types of diagrams (specified by '\code{what}') are: '\code{Rb-Sr}' and 
    '\code{Sm-Nd}' for the 1/Sr vs. \eqn{^{87}Sr/^{86}Sr[i]}{87Sr/86Sri}
    or 1/Nd vs. \eqn{\epsilon(Nd)} plots, respectively.

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

\keyword{hplot}
\concept{Plot}
\concept{Isotopic}
\concept{Menu: Plots}

\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}
