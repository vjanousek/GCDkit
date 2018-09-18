\name{menu}
\alias{menuOn}
\alias{menuOff}

\title{Controlling the Windows menus}

\description{
Functions controlling that render some options of the menus on or off, as 
appropriate.
}

\usage{
menuOn(which=winMenuNames())

menuOff(which=winMenuNames())
}

\arguments{
  \item{which}{character; menu names to be treated}
}

\details{
The function '\code{menuOff}' is called at the start up, when no data are
present in the memory and most of the actions defined by the menus would be 
irrelevant.
}

\value{
None.
}

\keyword{internal}
\keyword{manip}
\concept{Edit}

\author{
    Vojtech Janousek, \email{vojtech.janousek@geology.cz}
}
