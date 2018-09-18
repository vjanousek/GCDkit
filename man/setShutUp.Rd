\name{setShutUp}
\title{Quiet mode?}
\alias{setShutUp}

\description{
Determines whether extensive textual output is to be printed.
}

\usage{
setShutUp()
}


\arguments{
None.
}

\details{
The control option is \code{shut.up}, whose default is \code{FALSE}, meaning that 
detailed information is to be printed. This, however, may become not viable on 
slower systems and/or for extensive data sets.

This can be set from the menu \code{'GCDkit|Options'} 
by setting the checkbox \code{'Minimize output on screen?'} or directly, from the 
command line (see Examples). 
}

\examples{
getOption("shut.up")    # query the current value of the given option
options("shut.up"=TRUE) # reduce the printed output to a minimum
}

\keyword{print}
\concept{Print}
\concept{Parameter}
\concept{Menu: GCDkit}

\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}

\seealso{
\code{'\link{gcdOptions}'} \code{'\link{options}'}
}

