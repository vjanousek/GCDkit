\name{summaryByGroup}
\title{Statistics: Statistical summaries by groups}
\alias{Statistics: Majors summaryByGroups}
\alias{Statistics: Trace summaryByGroups}
\alias{summaryByGroupTrc}
\alias{summaryByGroupMjr}
\alias{summaryByGroup}
\alias{summaryRangesByGroup}

\description{
The function  '\code{summaryByGroup}' prints a statistical summary for selected 
list of elements (majors as a default) and the whole dataset or its selection, 
respecting the current grouping. Functions '\code{summaryByGroupMjr}' and 
'\code{summaryByGroupTrc}' are entry points supplying the default lists for 
major- and trace elements. The function '\code{summaryByGroupTrc}'
returns only ranges of the given parameter(s).
}

\usage{
summaryByGroup(elems = major, where = NULL, show.boxplot = FALSE,
    show.hist = FALSE, silent = TRUE)

summaryByGroupMjr()

summaryByGroupTrc()

summaryRangesByGroup(elems=major, where=NULL, silent=TRUE)
}

\arguments{
    \item{elems}{list of desired elements}
    \item{where}{list of desired samples to be processed}
    \item{show.boxplot}{logical, should be plotted the boxplots?}
    \item{show.hist}{logical, should be plotted the histograms?}
    \item{silent}{logical, should be the above chosen by the appropriate dialogues?}
}

\details{
The statistical summary involves number of observations, missing values, mean,
standard deviation, minimum, 25\% quartile, median (= 50\% quartile), 75\% quartile 
and maximum. The function also plots a summary boxplots and histograms, if 
desired so.

The samples can be selected based on combination of three searching mechanisms 
(by sample name/label, range or a Boolean condition) - see \code{\link{selectSamples}} 
for details. 

The defaults are lists of major \code{(SiO2, TiO2, Al2O3, FeOt, MnO, MgO, 
CaO, Na2O,}\cr 
\code{K2O)} or trace \code{(Rb, Sr, Ba, Cr, Ni, La, Eu, Y, Zr)} 
elements, respectively.  

The desired variables are selected using the function 
'\code{\link{selectColumnsLabels}}'. 

In the specification of the variable can be used also 
arithmetic expressions, see \code{\link{calcCore}} for the correct syntax.
}

\value{
\item{results}{a list with the results for individual groups}
}

\examples{
summaryByGroup(LILE)
summaryByGroupTrc()
summaryByGroupMjr()
summaryRangesByGroup(elems="Rb/Sr,Na2O+K2O")

# user-defined list
my.elems<-c("Rb","Sr","Ba/Sr")
summaryByGroup(my.elems)
}

\keyword{univar}
\keyword{hplot}
\concept{Calculation}
\concept{Plot}
\concept{Menu: Calculations: Statistics}
\concept{Statistics}
\concept{Grouping}

\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}
