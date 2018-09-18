\name{setTransparency}
\title{Setting transparency of plotting colours}
\alias{setTransparency}

\description{
    Sets transparency of plotting colours for selected samples. Alternatively, it just returns 
    the hexadecimal code(s) of specified colour(s) with the desired degree of transparency.
}

\usage{
    setTransparency(which.samples=NULL,transp=NULL,alpha=NULL,
    col.in="black",save=TRUE,GUI=FALSE)
}

\arguments{
    \item{which.samples}{list of samples; if NULL a dialogue is displayed}
    \item{transp}{numeric; transparency to be set}  
    \item{alpha}{character; alpha value to be set (opacity)}
    \item{col.in}{numeric or character vector; colour specification(s)} 
    \item{save}{logical; should be the result saved into labels$Colour?}
    \item{GUI}{logical; is the function called form within GUI?}
}

\details{
    The transparency value has to fall between 1 (completely transparent) to 0 (opaque).

    Alternatively, the so-called alpha channel can be specified, which can attain any
    hexadecimal number between 0 (completely transparent) to ff (opaque). 

    if \code{GUI = TRUE}, the samples can be selected based on combination of three searching mechanisms 
    (by sample name/label, range or a Boolean condition) - see \code{\link{selectSamples}} for details. 
}

\value{
    Returns (invisibly) hexadecimal codes of the colours with desired degree of transparency
    If '\code{save=TRUE}' it also assigns '\code{labels$Colour}' producing the new, partly 
    transparent colour.
}

\section{Warning}{
    As a side product, plotting colours are converted to hexadecimal values, 
    which are not easy to translate back to symbolic names.
}

\seealso{
    Colours by a single variable can be assigned by \code{\link{assignColLab}}, 
    symbols and colours by groups simultaneously by \code{\link{assignSymbGroup}}. 
    Uniform colours are obtained by \code{\link{assign1col}}.
    Table of available plotting colours is obtained by \code{\link{showColours}}.
}

\examples{
    # Affects the colour of plotting symbols in the system (save=TRUE by default)
    setTransparency(transp=0)
    setTransparency(transp=0.5)
    setTransparency(which.samples=c("Sa-1","Sa-2","Sa-3"),transp=0.5)
    setTransparency(which.samples=c("Sa-1","Sa-2","Sa-3"),alpha="6a")
    
    # No labels assigned
    setTransparency(col=2,transp=0.5,save=FALSE)
    setTransparency(col=c("blue","red"),transp=0.5,save=FALSE)
}

\keyword{color}
\concept{Edit}
\concept{Plot}
\concept{Menu: Data handling}
\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}
