\name{psAll}
\title{Save all graphics to PS}
\alias{psAll}

\description{
Saves all graphical windows to Postscript files.
}

\usage{
    psAll(filename=NULL)
}

\arguments{
    \item{filename}{a name of file for saving the output.}
}

\details{
The function prompts for a common root of the filenames and then saves all 
graphical windows, each in a separate file, numbering them sequentially. 
Postscript is the best export format from R, preserving the necessary quality as 
well as the possibility to be imported by most graphical editors (such as Corel 
Draw!) for retouching. 

Otherwise individual diagram can be saved from a menu that appears after 
clicking on the appropriate graphical window ('\code{File|Save as|Postscript}').
}

\value{
None.
}

\seealso{
  '\code{\link{pdfAll}}' '\code{\link{postscript}}'
}

\keyword{device}
\keyword{file}
\concept{Plot}
\concept{File}
\concept{Menu: GCDkit}

\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}
