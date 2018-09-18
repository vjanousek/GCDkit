\name{fousy}
\alias{fousy}

\title{Plotting error bars}

\description{
Using the function \code{fousy} can be added error bars for the x, and or, y
coordinate(s) of the binary plot.
}

\usage{
fousy(x, y, sy, sx = 0, col = rep("black", length(x)))
}

\arguments{
  \item{x, y}{coordinates}
  \item{sx, sy}{one sigma errors}
  \item{col}{colour}
}
\details{
The function plots error bars plus/minus 1 sigma.
}

\author{
    Vojtech Janousek, \email{vojtech.janousek@geology.cz}
}

\keyword{internal}
\keyword{aplot}
