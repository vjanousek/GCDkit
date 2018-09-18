\name{spiderBoxplot}
\title{Spider plot(s): Selected samples - summary boxplot}
\alias{spiderBoxplot}

\description{
Normalization of geochemical data by the given standard (optionally also one of 
the samples) and spiderplot plotting. No individual patterns are drawn; instead, 
the statistical distribution of each element is portrayed by a boxplot.
}

\usage{
spiderBoxplot(norm = NULL, which = rep(TRUE,nrow(WR)),
    doublenorm = FALSE, norm2 = "",
    ymin = NULL, ymax = NULL, bpplot = TRUE, 
    col = "lightgray", log = TRUE)
}

\arguments{
\item{norm}{a character string specifying the model.}
\item{which}{specification of the samples to be plotted.}
\item{doublenorm}{logical; should be the normalization employed? See details.}
\item{norm2}{name of the variable for the second normalization.}
\item{ymin, ymax}{y range of the diagram.}
\item{bpplot}{logical; if FALSE, boxplot box (instead of box and percentile plot) is shown.}
\item{col}{fill colour.}
\item{log}{logical; should be the y axis scaled logarithmically?}
}

\details{
The  parameter '\emph{norm}' is an optional search pattern to 
query  the available normalizing model names. It can contain a 
substring or even a regular expression. The function fails 
if no matches are found or the search is ambiguous. See \code{\link{selectNorm}}
for details.

The samples to be plotted can be selected based on combination of three 
searching mechanisms (by sample name/label, range or a Boolean condition) - see 
\code{\link{selectSamples}} for details. 

For choosing the correct normalization values serves the auxiliary function 
\code{\link{selectNorm}}, which is the same as in ordinary spiderplots.
If the user desires so, the data can be  normalized by a sample 
present in the dataset. Then the elements to be plotted and their order is to 
be specified, as well.

Optionally, double normalization can be used. Trace-element data are first 
normalized by the given standard, then by the normalized content of the selected 
element in each analysis to eliminate effects of fractional crystallization 
\emph{(Thompson et al. 2003, Pearce et al. 2005, Pearce and Stern 2006)}.
See \code{\link{spider2norm}} for details. 

Distributions of individual normalized elements are plotted in the form of 
\link{boxplot} or box and percentile plot \emph{(Esty and Banfield 2003)}.

\if{html}{\figure{spiderbpp.png}} 
\if{latex}{\figure{spiderbpp.png}{options: width=4.5in}}  
 
In both cases the box denotes 50\% of the population (both quartiles), 
the horizontal line in the middle is a median and the whiskers denote the overall 
range. For boxplot this is without outliers. 
See manual entry for '\code{boxplot}' and '\code{bpplot.my}' for further details. 

Printed are number of observations, missing values, mean,
standard deviation, minimum, 25\% quartile, median (=50\% quartile), 75\% 
quartile and maximum.
}

\section{Warning}{
This function IS NOT Figaro-compatible. It means that the set of diagrams cannot 
be further edited in GCDkit (e.g. tools in "Plot editing" menu are inactive).
}

\value{
\item{results}{numeric matrix with statistical data for individual elements.}
}

\keyword{hplot}
\concept{Plot}
\concept{Spiderplot}
\concept{Menu: Plots}

\examples{
 spiderBoxplot("Boynton",col="yellow",bpplot=FALSE)
 spiderBoxplot("Primordial Wood",doublenorm=TRUE,norm2="Y",
    col="khaki",ymin=0.05,ymax=1000,bpplot=TRUE)
}

\seealso{
For the syntax of the setup file with normalizing values and adding new 
normalization schemes \cr
see \code{\link{selectNorm}}; for further applications of 
'\code{spider}' see \code{\link{spiderByGroupPatterns}}, \cr
\code{\link{spider2norm}} and \code{\link{spiderByGroupFields}}.
}

\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}

\references{
Esty, W. W. & Banfield, J. D. (2003). The Box-Percentile Plot. 
Journal of Statistical Software 8 (17)

Pearce J A, Stern R J (2006) Origin of back-arc basin magmas: 
Trace element and isotope perspectives. Back-Arc Spreading Systems: 
Geological, Biological, Chemical, and Physical Interactions. 
Geophysical Monograph Series 166. American Geophysical Union, pp 63-86

Pearce J A, Stern R J, Bloomer S H, Fryer P (2005) 
Geochemical mapping of the Mariana arc-basin system: 
implications for the nature and distribution of subduction components. 
Geochem Geophys Geosyst 6: doi: 10.1029/2004GC000895

Thompson R N, Morrison M A, Dickin A P, Hendry G L (1983) 
Continental flood basalts... Arachnids rule OK? 
In: Hawkesworth C J, Norry M J (eds) Continental Basalts and Mantle Xenoliths. 
Shiva, Nantwich, pp 158-185
}
