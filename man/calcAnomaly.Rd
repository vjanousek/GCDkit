\name{calcAnomaly}
\title{Anomaly on a spiderplot}
\alias{calcAnomaly}

\description{
    Calculates a magnitude of an anomaly on a spiderplot, 
    based on concentrations of selected neighboring elements.
}

\usage{
calcAnomaly(which.elem="Eu",dataset=WR,ref="^REE Boynton",left="Sm",
    right="Gd")
}

\arguments{
    \item{which.elem}{character; which element is being examined?}
    \item{dataset}{character; name of variable holding the whole-rock data.}
    \item{ref}{character; a specification of the normalization scheme.}
    \item{left}{character; a name of element to the left, used for extrapolation.}
    \item{right}{character; a name of element to the right, used for extrapolation.}
}

\details{
    This is a general function that calculates a magnitude of an anomaly on a spiderplot. 
    For the given element it is a ratio of its normalized contents divided by an extrapolated value 
    (denoted by a star). The extrapolation is performed is from two neighboring elements, 
    one to the left and  one to the right, of the examined one. But these two elements used 
    for extrapolation do not need to be immediately adjacent.
    
    The best known and the most commonly used  is the Eu anomaly on 
    chondrite-normalized REE plots expressed as:
    
    \deqn{\frac{Eu}{Eu^*}=\frac{{Eu}_{N}}{\sqrt{S{{m}_{N}}G{{d}_{N}}}}}{%
          Eu/Eu* = EuN/SQRT(SmN*GdN)}
          
    But this principle can be generalized even for elements that are not immediately adjacent 
    to the anomaly, like on its figure:
    
    
    \if{html}{\figure{AnyAnomaly.png}}
    \if{latex}{\figure{AnyAnomaly.png}{options: width=4.5in}}  
    
    The spiderplot is selected using the parameter '\code{ref}' 
    which can contain a substring (or a regular expression) specifying the name of 
    the normalizing scheme stored in the file '\code{spider.data}' 
    of the main GCDkit directory. For details and examples, see \code{\link{selectNorm}}. 
}

\value{
    A numeric matrix with a single row, containing the calculated values. 
}

\examples{
calcAnomaly() # Eu anomaly on chondrite-normalized REE plot after Boynton (1984). 

# Nb anomaly, Nb/Nb*, based on immobile NMORB spiderplot after Pearce (1984)
NbNb<-calcAnomaly(which.elem="Nb",dataset=WR,
    ref="^NMORB immobile",left="Th",right="La")     
WR<-addOn("Nb/Nb*",as.vector(NbNb),where=WR) # Append to the current data set
}

\references{
    Boynton WV (1984) Cosmochemistry of the rare earth elements: meteorite studies. 
    In: Henderson P (eds) Rare Earth Element Geochemistry. Elsevier, Amsterdam, 
    pp 63-114

    Pearce JA (2014) Immobile element fingerprinting of ophiolites. Elements 10: 101-108 
    \href{http://dx.doi.org/10.2113/gselements.10.2.101}{doi: 10.2113/gselements.10.2.101}
}

\seealso{
    \link{selectNorm}
    \link{spider}
}

\keyword{dplot}
\concept{Menu: Plots}
\concept{Plot}
\concept{Spiderplot}

\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}
