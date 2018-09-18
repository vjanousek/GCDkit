\name{showColours}
\title{Show available colours}
\alias{showColours}
\alias{showColours2}

\description{
Display colours available for plotting.
}

\usage{
showColours(n=49)
showColours2(n=64)
}

\arguments{
\item{n}{numeric: number of colours to display}
}

\details{
The function '\code{showColours}' displays a palette of plotting colours 
which can be specified by their numeric codes (1-49). On the other hand, 
'\code{showColours2}' demonstrates the colours which can be given by their 
English names (there are some 657 of them).
    \if{html}{\figure{showColours.png}} 
    \if{latex}{\figure{showColours.png}{options: width=4in}}
}

\keyword{aplot}
\keyword{color}
\concept{Plot}
\concept{Menu: Data handling}

\seealso{
  '\code{\link{colours}}'
}

\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}
