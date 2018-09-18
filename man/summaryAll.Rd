\name{summaryAll}
\title{Statistics: Statistical summaries for the whole data set or its subset}
\alias{Statistics: Majors summaryAll/selection}
\alias{Statistics: Trace summaryAll/selection}
\alias{summaryMajor}
\alias{summaryTrace}
\alias{summaryAll}

\description{
The function '\code{summaryAll}' prints statistical summary for selected list 
of elements (majors as a default) and the current dataset (or its part). 
Functions '\code{summaryMajor}' and '\code{summaryTrace}' are entry points 
supplying the default lists for major- and trace elements. 
}

\usage{
summaryAll(elems = major, where = NULL, show.boxplot = FALSE, 
    show.hist = FALSE, silent=TRUE)
summaryMajor()
summaryTrace()
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
and maximum. The function also plots summary boxplots and histograms, if 
desired so.

The samples can be selected based on combination of three searching mechanisms 
(by sample name/label, range or a Boolean condition) - see \code{\link{selectSamples}} 
for details. 

Even though as a default are assumed majors 
(\code{SiO2, TiO2, Al2O3, 
FeOt, MnO, MgO, CaO, Na2O, K2O} for 
'\code{summaryMajor}') or selected trace 
(\code{Rb, Sr, Ba, Cr, Ni, La, Eu, Y, Zr} 
for '\code{summaryTrace}')
elements, the variable(s) to be displayed can be 
modified/specified in all cases.
To this purpose serves the function 
'\code{\link{selectColumnsLabels}}'. 

In the specification of the variable can be used also 
arithmetic expressions, see \code{\link{calcCore}} for the correct syntax.
}

\value{
\item{results}{numeric matrix with the results}
}

\examples{
summaryAll(LILE)

summaryAll(LILE,show.hist=TRUE)

summaryAll(LILE,show.boxplot=TRUE)

# user-defined list
my.elems<-c("Rb","Sr","Ba")
summaryAll(my.elems)

\dontrun{
    summaryMajor()
    summaryTrace()
}
}

\seealso{
\code{\link{statistics}}
\code{\link{summarySingle}}
\code{\link{summarySingleByGroup}}
\code{\link{summaryByGroup}}
}


\keyword{univar}
\keyword{hplot}
\concept{Calculation}
\concept{Plot}
\concept{Menu: Calculations: Statistics}
\concept{Statistics}

\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}
