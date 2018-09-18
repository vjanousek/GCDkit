\name{epsEps}
\title{Binary plot of initial Sr isotopic ratios vs. initial epsilon Nd values}
\alias{87Sr/86Sr vs EpsNdi}
\alias{epsEps}

\section{Plugin}{SrNd.r}

\description{
Plots a diagram of initial  \eqn{^{87}Sr/^{86}Sr}{87Sr/86Sr} ratios vs. initial 
\eqn{\epsilon(Nd)} values for selected samples.
    \if{html}{\figure{epsEps.png}} 
    \if{latex}{\figure{epsEps.png}{options: width=5.5in}}
}

\usage{
epsEps(GUI=FALSE,...)
}

\arguments{
    \item{GUI}{logical; is the function called from the GUI?}  
    \item{...}{optional parameters to the underlying function \code{{plotWithLimits}}}
}

\details{
    If in GUI, the samples can be selected based on combination of three searching mechanisms 
    (by sample name/label, range or a Boolean condition) - see \code{\link{selectSamples}} 
    for details. 
}

\seealso{
    The actual plotting is done by the function \code{\link{plotWithLimits}}.
}

\value{
    None.
}

\keyword{hplot}
\concept{Plot}
\concept{Isotopic}
\concept{Menu: Plugins}

\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}
