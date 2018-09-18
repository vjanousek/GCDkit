\name{tkSelectVariable}
\title{TclTk GUI: Select a single variable}
\alias{tkSelectVariable}


\description{
    Function to select a single variable using the Tcl/Tk-based Graphical User Interface (GUI).
}

\usage{
tkSelectVariable(top.frame = NULL, where = colnames(WR), preselect = 2, 
    pack = FALSE, message = "Select a variable", background = "wheat", 
    variable = "x", on.leave = function() {}, row = 0, column = 0, height = 15, 
    width = 50, buttons = FALSE, state = "normal")
}

\arguments{
    \item{top.frame}{name of the parental frame}
    \item{where}{character; names of variables to be chosen from}
    \item{preselect}{numeric; which item is to be preselected}
    \item{pack}{logical; pack the frame?}
    \item{message}{character; textual prompt}
    \item{background}{colour for the frame background}
    \item{variable}{character; variable name with the output}
    \item{on.leave}{function to be invoked upon leave}
    \item{row, column}{coordinates within the parental frame}
    \item{height, width}{size of the frame}
    \item{buttons}{logical; should the frame have also buttons?}
    \item{state}{???}
}

\details{
    The buttons are: Reset, SortUp, SortDown, OK, Cancel.  
}


\keyword{manip}
\concept{Subsetting}
\concept{Edit}
\concept{TclTk}
\concept{GUI} 

\seealso{
    \code{\link{tcltk-package}}
}

\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}
