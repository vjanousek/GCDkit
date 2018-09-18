\name{selectPalette}
\title{selectPalette}
\alias{selectPalette}

\description{
    Picks given number of colours from one of the available palettes.
}

\usage{
selectPalette(n,colour.palette=NULL,GUI=TRUE)
}

\arguments{
    \item{n}{desired number of colours}  
    \item{colour.palette}{one of the colour palette names, see Details}
    \item{GUI}{logical; is the function called from GUI?}
}

\details{
The desired number of colours has to be given in any case.

The possible palettes are: \code{'grays','reds','blues','greens','cyans','violets','yellows'},
\code{'cm.colors','heat.colors','terrain.colors','topo.colors','rainbow'} and \code{'jet.colors'}.

Also, user-defined palette functions are supported now. See Examples. 

If not specified upon function call, the colour palette can be picked from
list of available ones. Optionally (if \code{GUI = TRUE}) it plots a chart with their preview.



    \if{html}{\figure{selectPalette.png}} 
    \if{latex}{\figure{selectPalette.png}{options: width=5.5in}}
    
    
}

\value{
    Returns a matrix with a single row of hexadecimal codes. Its \code{rownames} represent the name 
    of the palette selected.
}

\note{
    Note that UK spelling of "colours" in names of palettes is fixed automatically to the US "colors".
}


\seealso{
    Colours by label can be assigned by \code{\link{assignColLab}}, 
    colours by variable using \code{\link{assignColVar}}. 
    Uniform colours are obtained by \code{\link{assign1col}}.
    Table of available plotting colours is obtained by \code{\link{showColours}}.
}

\examples{
ee<-selectPalette(5,"heat.colours")
ee<-selectPalette(5)
ee<-selectPalette(5,GUI=FALSE)
    
my.palette<-colorRampPalette(c("black", "darkgreen", "red"),space = "rgb")
ee<-selectPalette(5,"my.palette")
}

\keyword{color}
\concept{Edit}
\concept{Plot}
\concept{Menu: Data handling}
\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}
