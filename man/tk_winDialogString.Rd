\name{tk_winDialogString}
\title{tk_winDialogString}
\alias{tk_winDialogString}

\description{
    Tcl/Tk replacement for the MS Windows-specific function '\code{winDialogString}'.
}

\usage{
tk_winDialogString(message="Enter variable",default="",returnValOnCancel=NULL)
}

\arguments{
  \item{message}{Character. The information field of the dialog box.}
  \item{default}{Character; the default string.}  
  \item{returnValOnCancel}{Character; a value to be returned when the dialogue is canceled.}
}


\details{
    This is a platform-independent implementation of the MS Windows-specific function 
    '\code{\link{winDialogString}}',
    written using the Tcl/Tk. 
}

\value{
    A character string giving the contents of the text box when \code{Ok} was pressed, 
    or value specified by '\code{returnValOnCancel}' if \code{Cancel} was pressed. 
}

\seealso{
    \code{\link{winDialogString}} \code{\link{tkentry}} \code{\link{tk_winDialog}} \code{\link{tcltk-package}}
}

\examples{
tk_winDialogString(message="Enter x value",default="15.7")
}

\keyword{manip}
\concept{Edit}
\concept{TclTk}
\concept{GUI} 

\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}

