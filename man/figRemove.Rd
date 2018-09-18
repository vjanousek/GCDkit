\name{figRemove}
\title{Setting Figaro parameters directly}
\alias{figRemove}

\description{
    An auxiliary function that serves for permanent removal of entries of certain 
    type from the Figaro-compatible templates.
}

\usage{
    figRemove(what = "text", where = "sheet$demo$template")
}

\arguments{
  \item{what}{character; string to be sought in parameter names}
  \item{where}{template key}
}

\section{Warning}{
    All the corresponding entries are deleted permanently and without any prior 
    warning.
}

\details{
    The function is invoked from the menu in its default form, when it 
    switches off all texts on the current diagram. 

    This can be achieved also, prior to plotting, 
    from the menu \code{'GCDkit|Options'} 
    by setting the checkbox \code{'Annotate fields on discrimination plots?'},
    connected to the option \code{plot.text}. Alternatively, this option 
    can be set directly from the command line (see Examples). 
} 

\value{
    Modifies the Figaro template.
}

\author{
    Colin M. Farrow, \email{colinfarrow537@gmail.com} 

    and Vojtech Janousek, \email{vojtech.janousek@geology.cz}
}

\examples{
    getOption("plot.text")     # query the current value of the given option
    options("plot.text"=FALSE) # from now on no textual labels will be shown on 
                               #  classification plots
}

\seealso{
    \link{figReplace}
    \link{figaro} 
    \link{gcdOptions}
    \link{options}
}

\keyword{internal}
\keyword{aplot}
\concept{Plot}
\concept{Figaro}
\concept{Menu: Plot editing}
