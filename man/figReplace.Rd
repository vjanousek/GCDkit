\name{figReplace}
\alias{figReplace}
\title{Setting Figaro parameters directly}

\description{
    An auxiliary function that serves for replacement of the values for the given 
    parameter in the Figaro-compatible templates.
}

\usage{
    figReplace(what = "lines", param = "col", value = 1, 
    where = "sheet$demo$template", exact=TRUE)
}

\arguments{
  \item{what}{character; name of the parameter}
  \item{param}{character; name of its attribute}
  \item{value}{the new value of the attribute}
  \item{where}{template key}
  \item{exact}{logical, should be returned only exact or also partial matches?}
}

\details{
    This function is used only internally by other functions, 
    such as '\code{\link{figCol}}'
    and '\code{\link{figUser}}'.
}

\value{
    Modifies the Figaro template.
}

\author{
    Colin M. Farrow, \email{colinfarrow537@gmail.com} 

    and Vojtech Janousek, \email{vojtech.janousek@geology.cz}
}

\seealso{
    \link{figRemove} 
    \link{figaro} 
}

\keyword{internal}
\keyword{aplot}
\concept{Plot}
\concept{Figaro}
\concept{Menu: Plot editing}
