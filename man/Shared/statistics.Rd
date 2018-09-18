\name{statistics}
\alias{statistics}
\title{Basic descriptive statistics}

\description{
Calculates simple descriptive statistics respecting grouping. Optionally can be
plotted boxplots and/or histograms.   
}

\usage{
statistics(where = WR, elems, groups = rep("", nrow(where)), 
which = "", boxplots = TRUE, histograms = TRUE, 
print.table = TRUE, sub = "")
}

\arguments{
  \item{where}{name of the data matrix}
  \item{elems}{list of elements/oxides}
  \item{groups}{grouping information, default=no grouping}
  \item{which}{which of the groups are to be shown}
  \item{boxplots}{logical, plot boxplots?}
  \item{histograms}{logical, plot histograms?}
  \item{print.table}{logical, print the resulting table?}
  \item{sub}{subtitle for the histograms/boxplots}
}

\details{
The function returns a list containing the calculated statistical parameters 
respecting the desired grouping. The statistical summary involves number of 
observations, missing values, mean, standard deviation, minimum, 25\% quartile, 
median (= 50\% quartile), 75\% quartile and maximum.
}

\value{
\item{results}{a list with the results for individual groups}
}

\seealso{
\code{\link{summarySingle}}
\code{\link{summarySingleByGroup}}
\code{\link{summaryAll}}
\code{\link{summaryByGroup}}
}

\examples{
    i<-statistics(elems=c("SiO2","TiO2","Al2O3","Na2O","K2O"),
    boxplots=FALSE,histograms=FALSE)
}

\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}

\keyword{internal}
\keyword{univar}
\concept{Calculation}
\concept{Grouping}
\concept{Menu: Calculations: Statistics}
\concept{Statistics}
