\name{pairsCorr}
\title{Statistics: Correlation}
\alias{Statistics: Correlation: majors}
\alias{Statistics: Correlation: traces}
\alias{pairsMjr}
\alias{pairsTrc}
\alias{pairsCorr}
\alias{panel.cor}
\alias{panel.cov}
\alias{panel.hist}
\alias{panel.smooth}

\usage{
pairsCorr(elems = major)

pairsMjr()

pairsTrc()
}
\description{
Plots a matrix of scatterplots in the lower panel and one of other pre-defined 
panel functions in the upper.
}

\arguments{
\item{elems}{list of desired elements}
}

\examples{
pairsCorr(LILE)

pairsMjr()

pairsTrc()

# user-defined list
my.elems<-c("Rb","Sr","Ba")
pairsCorr(my.elems)
}

\details{
The samples can be selected based on combination of three searching mechanisms 
(by sample name/label, range or a Boolean condition) - see 
\code{\link{selectSamples}} for details. 

Even though a list of major elements is assumed as a default, different 
variables can be specified by the function '\code{\link{selectColumnsLabels}}'. 

\if{html}{\figure{pairsCorr.png}} 
\if{latex}{\figure{pairsCorr.png}{options: width=5.5in}}

The upper panels to choose from are:
    
    \tabular{ll}{
    '\code{panel.corr}' \tab Prints correlations, with size proportional to the correlations; \cr 
    '\code{panel.cov}' \tab Prints covariances; \cr
    '\code{panel.smooth}' \tab Fits smooth trendlines; \cr
    '\code{panel.hist}' \tab Plots frequency histograms. \cr
    }
}

\value{
None.
}

\section{Warning}{
Names of existing numeric data columns and not formulae involving these can be 
handled at this stage.
}

\keyword{hplot}
\keyword{multivariate}
\concept{Statistics}
\concept{Plot}
\concept{Menu: Calculations: Statistics}

\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}
