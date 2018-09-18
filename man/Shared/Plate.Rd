\name{Plate}
\title{Plotting plates of several diagrams}
\alias{Plate}
\alias{multiplePerPage}
\alias{plateRedraw}
\alias{platePS}
\alias{plateSave}
\alias{plateLoad}

\description{
    Functions to set up, save or load a so-called 'plate', i.e. a regular grid of slots
    to accommodate (any mixture of) binary or ternary plots, spiderplots 
    or such alike. For instance, Harker plots are implemented 
    using the plate concept.  
}

\usage{
multiplePerPage(which=NULL,nrow=NULL,ncol=NULL,title="Plate",
    dummy=FALSE)
    
Plate(scr=NULL) 
    
plateRedraw(device="windows",filename=NULL,colormodel="rgb")
    
platePS(colormodel="rgb")
    
plateSave()
    
plateLoad()  
}

\arguments{
  \item{which}{total number of slots to be occupied by individual diagrams.}
  \item{nrow}{number of rows in the plots' matrix.}
  \item{ncol}{number of columns in the plots' matrix.}
  \item{title}{title for the whole plate.}
  \item{dummy}{logical; if TRUE, dummy plots are shown. See Details.}
  \item{scr}{(optional) number of screen to be selected.}
  \item{device}{output device; either \code{'windows'} or \code{'postscript'}.}
  \item{filename}{name of file if output redirected to Postscript.}
  \item{colormodel}{color mode for Postscript; \code{'rgb'} or \code{'gray'}.}
}

\details{
    The function \code{'multiplePerPage'} serves to setting up a matrix of  slots, 
    each of which could be taken by a single Figaro-compatible diagram 
    (a binary plot, a ternary plot, a spiderplot,\dots).
    If \code{'which'} is NULL, the function asks for their number, 
    and then suggests number of rows (\code{'nrow'}) and columns (\code{'ncol'}) 
    for the matrix arrangement.

    If desired, the slots can be filled by the so-called 'dummy plots', i.e. gray 
    boxes showing the exact position and the size of each of them.    

    If \code{'which'} is an integer, specified number of slots is allocated. Alternatively,
    this argument may represent a vector containing any mixture of names of diagrams 
    that can be plotted by the function \code{\link{plotDiagram}} or even plotting commands 
    themselves used to fill the individual slots directly. See Examples.  

    Once set up, a single slot can be selected for further work using the function 
    \code{'Plate'}. The function can be called directly, with the number of the
    screen desired. If none is specified, a red box-like cursor appears in the graphical 
    window, which can be moved around using the cursor keys, \code{Spacebar} or by mouse. 
    The appropriate slot can be chosen by left mouse button or by pressing \code{Enter}. 
    Right-click anywhere on the plate invokes a context menu which enables several actions:

    \tabular{ll}{
        \bold{Menu item}\tab \bold{Function} \cr
        Introduce plot \tab Select a new Figaro-compatible diagram for this slot.\cr
        Plot editing \tab Modify the existing diagram (like the menu 
            \code{Plot editing} for stand alone plots).\cr
        Plate editing \tab Functions to modify the overall plate properties or all 
            its diagrams simultaneously.\cr
    }
    
    The function \code{'plateRedraw'} serves for replotting a 'clean! version of the 
    whole plate, eg. for saving/printing, For this purpose, its output can be redirected
    to Postscript, either in colour or as black and white. As a wrapper for the Postscript
    output serves the function \code{'platePS'} 
    
    The functions \code{'plateSave'} and \code{'plateLoad'} are designed to save and 
    retrieve definitions of plates (Figaro sheets and the relevant data) for later use. 
    The default suffix for the saved plates is '\code{mgr}'. Note that only the data 
    needed for the plotting ('\code{x.data}', '\code{y.data}') are stored in
    the 'mgr' files. Thus the data set currently in memory (e.g., variables '\code{WR}', 
    '\code{labels}', \ldots) is unaffected by the function '\code{plateLoad}'.

    Starting with GCDkit version 3, the plates concept is used by 
    some built-in functions, such as 'Multiple plots' (function \code{\link{multiple}}) or 
    'Multiple plots by groups' (function \code{\link{figMulti}}). 
}

\value{
    \item{plate}{list of Figaro definitions for individual diagrams}
    \item{plate.data}{list containing \code{'x.data'} and \code{'y.data'} for each of them}
}


\seealso{
    \link{Plate editing},  \link{plotPlate}, \link{multiple},  \link{figMulti},  \link{plot},  
    \link{binary},  \link{ternary},  \link{spider},  \link{figaro},  \link{figLoad},  \link{figSave}
}

\examples{
data<-loadData("sazava.data",sep="\t")
multiplePerPage(which=c("binary(\"K2O/Na2O\",
    \"Rb\",new=FALSE)","DebonPQ","AFM",
    "PeceTaylor","Shand"))
    
Plate()
    
Plate(3)
plotDiagram("LarochePlut",FALSE,FALSE)
}

\keyword{hplot}
\concept{Plot}
\concept{Plates}
\concept{Menu: Plots}

\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}

