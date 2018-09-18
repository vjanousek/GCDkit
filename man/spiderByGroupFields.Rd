\name{spiderByGroupFields}
\title{Spider plot(s) - by group fields}
\alias{spiderByGroupFields}

\description{
Plots a series of spiderplots, for each group one, outlining the overall 
distribution as a field.
}

\usage{
spiderByGroupFields(rock = WR, norm = NULL, 
    bw = FALSE, fill = FALSE, ymin = 0, ymax = 0,
    xrotate = FALSE, offset = TRUE, centered = FALSE)
}

\arguments{
    \item{rock}{a numeric matrix; the whole-rock data from which will be filtered out those to be normalized.}
    \item{norm}{a character string specifying the model.}
    \item{bw}{logical; should be the plot black and white?}
    \item{fill}{logical; should be the fields filled by solid colour (and not hatched)?}
    \item{ymin, ymax}{y range of the diagram.}
    \item{xrotate}{logical; shall be the element names on x axis rotated?}
    \item{offset}{logical; shall be the  names for odd and even elements shifted relative to each other?}
    \item{centered}{logical; shall be the element names on x axis plotted in between tick marks?}
}

\details{
The  parameter '\emph{norm}' is an optional search pattern to 
query  the available normalizing model names. It can contain a 
substring or even a regular expression. For choosing the correct 
normalization values serves the auxiliary function 
\code{\link{selectNorm}}.The function fails 
if no matches are found or the search is ambiguous. 
See \code{\link{selectNorm}}
for details.

A series of spiderplots is plotted, for each group one, in 
which  the whole variation range is outlined as filled/cross-hatched 
fields.
}

\value{
None.
}

\keyword{hplot}
\concept{Plot}
\concept{Spiderplot}
\concept{Grouping}
\concept{Menu: Plots}

\seealso{
For the syntax of the setup file with normalizing values and adding new 
normalization schemes see \code{\link{selectNorm}}. This function is based on 
\code{\link{spider}}.
}

\examples{
\dontrun{
data<-loadData("sazava.data",sep="\t")
groupsByLabel("Intrusion")
spiderByGroupFields(norm="Boynton",ymin=1,ymax=1000)
    
spiderByGroupFields(norm="Boynton",bw=TRUE,ymin=1,ymax=1000,xrotate=TRUE,offset=FALSE)
    
spiderByGroupFields(norm="Boynton",fill=TRUE,ymin=1,ymax=1000)
}
}
   
\author{
Vojtech Janousek, \email{vojtech.janousek@geology.cz};
 
Vojtech Erban, \email{vojtech.erban@geology.cz}, 
contributed the algorithm hatching closed polygons
}
