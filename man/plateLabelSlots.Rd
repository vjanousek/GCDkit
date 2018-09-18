\name{plateLabelSlots}
\alias{plateLabelSlots}
\title{Annotate individual slots by letters or Roman numerals}

\description{
  Annotates individual slots in a plate by letters or Roman numerals. 
  For instance (a), (b), (c)... or (i), (ii), (iii), (iv), (v)...
}

\usage{
plateLabelSlots(text=letters,style="()",cex=1.5,pos="topright")
}

\arguments{
  \item{text}{desired type of labels; see Details.}
  \item{style}{optional character strings before and after label, typically brackets.}
  \item{cex}{relative size of the text compared to the current code{par("cex")}.} 
  \item{pos}{character; position of the label relative to the plot.} 
}

\details{
    The argument '\code{what}' may acquire one of following values:
    \preformatted{
        'letters' 'LETTERS' 'numbers' 'roman' 'ROMAN'  
    } or can be user-defined character string of longer or of the same length
    as is the number of slots to be annotated (see the last example).
    
    Possible positions (parameter \code{pos}) are:
    \preformatted{
        'bottomright' 'bottom' 'bottomleft' 'left' 
        'topleft' 'top' 'topright' 'right' 'center'}.  
}

\value{
    none
}

\section{Note}{
    This function uses the plates concept. The individual plots can be selected and their 
    properties/appearance changed as if they were stand alone Figaro-compatible plots. 
    See \code{\link{Plate}}, \code{\link{Plate editing}} and 
    \code{\link{figaro}} for details.
}

\seealso{
    \link{Plate},  \link{Plate editing},  \link{figaro}
}

\examples{
multipleMjr("SiO2")
   
plateLabelSlots("letters","",pos="bottomleft")
   
plateLabelSlots("ROMAN","{}")
   
my_labs<-c("1st","2nd","3rd","4th","5th","6th","7th","8th","9th") 
plateLabelSlots(my_labs)
}

\keyword{hplot}
\concept{Plot}
\concept{Plates}
\concept{Menu: Plots}

\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}
