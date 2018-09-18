\name{sourceDir}
\alias{sourceDir}
\title{Source all R files in the given directory}

\description{
Internal function designed to source all '\code{*.r} files in a given directory
and (optionally) all its subdirectories. 
}

\usage{
sourceDir(dir="Norms",text="external file",recursive=FALSE)
}

\arguments{
  \item{dir}{directory specification}
  \item{text}{text echoed on the screen when the file is being loaded}
  \item{recursive}{logical, shall be the subdirectories included?}
}

\details{
This function is used at system start up (to load the normative algorithms and
diagrams) as well as after a new data file is loaded (to source all plugins). 
}

\value{
None.
}

\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}

\keyword{internal}
