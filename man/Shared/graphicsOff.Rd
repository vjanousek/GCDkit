\name{graphicsOff}
\title{Close all graphic windows}
\alias{graphicsOff}

\description{
Closes all graphic windows.
}

\usage{
graphicsOff()
}


\arguments{
None.
}


\details{
Under Windows 95/98/ME, the R system may become install, failing to redraw 
graphical windows if too many of them are being open. It is always a good idea 
to close the unnecessary ones, for instance using this function. 
}

\seealso{
'\code{\link{dev.off}}'
}

\keyword{hplot}
\concept{Menu: GCDkit}
\concept{Plot}
