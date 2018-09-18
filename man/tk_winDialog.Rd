\name{tk_winDialog}
\title{tk_winDialog}
\alias{tk_winDialog}

\description{
    Tcl/Tk replacement for the MS Windows-specific function '\code{winDialog}'.
}

\usage{
tk_winDialog(type="ok",message="")
}

\arguments{
  \item{type}{Character; the type of the dialogue box.}  
  \item{message}{Character. The information field of the dialogue box.}
}


\details{
    This is a platform-independent implementation of the MS Windows-specific function 
    '\code{\link{winDialog}}', written using the Tcl/Tk. 
    Possible types of the dialogue box are: 
    \code{ok}, \code{okcancel}, \code{yesno} and  \code{yesnocancel}. 
}

\value{
    A character string giving the name of the button pressed (in capitals). 
}

\seealso{
    \code{\link{winDialog}} \code{\link{tkmessageBox}} \code{\link{tk_winDialogString}} \code{\link{tcltk-package}}
}

\examples{
tk_winDialog(type="yesnocancel",message="Are you sure?")
}

\keyword{manip}
\concept{Edit}
\concept{TclTk}
\concept{GUI} 

\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}
