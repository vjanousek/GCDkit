\name{pdfAll}
\title{Save all graphics to PDF}
\alias{pdfAll}

\description{
Saves all graphical windows to a single PDF file.
}

\usage{
pdfAll(filename=NULL)
}

\arguments{
    \item{filename}{a name of file for saving the output.}
}

\details{
The function prompts for filename under which it saves all graphical windows, 
each on a separate page. PDF is the most portable format, that should preserve 
practically the same layout on all platforms. 

Individual diagram can be saved from a menu that appears after clicking on the 
appropriate graphical window ('\code{File|Save as|PDF}').
}

\value{
None.
}

\seealso{
  '\code{\link{psAll}}' '\code{\link{pdf}}'
}

\keyword{device}
\keyword{file}
\concept{Plot}
\concept{File}
\concept{Menu: GCDkit}

\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}
