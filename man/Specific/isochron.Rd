\name{isochron}
\title{Rb-Sr and Sm-Nd isochrons}
\alias{isochron}

\section{Plugin}{SrNd.r}

\description{
Plots a Rb-Sr or Sm-Nd isochron diagram and calculates a simple linear fit to 
the selected data.
    \if{html}{\figure{isochron.png}} 
    \if{latex}{\figure{isochron.png}{options: width=5.5in}}
}

\usage{
isochron()
}

\arguments{
  None.
}

\details{
The samples can be selected based on combination of three searching mechanisms 
(by sample name/label, range or a Boolean condition) - see \code{\link{selectSamples}} 
for details. 

If empty list is given, all the samples for which the required isotopic data are 
available are plotted and the user can choose their subset interactively. Then 
the isochron diagram is redrawn only with those samples.

The data are fitted by simple least-squares linear fit, from which the age and 
initial ratio are calculated.
}

\value{
Returns a numeric vector with the calculated age and initial ratio.
}

\keyword{hplot}
\concept{Plot}
\concept{Calculation}
\concept{Isotopic}
\concept{Menu: Plugins}

\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}
