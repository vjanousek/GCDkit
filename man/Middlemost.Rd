\name{Middlemost}
\title{Middlemost's diagram (1985)}
\alias{MiddlemostPlut}
\alias{Middlemost}

\description{
Assigns data for Middlemost's diagram into Figaro template (list '\code{sheet}') and appropriate values into '\code{x.data}' and '\code{y.data}'.
}

\usage{
MiddlemostPlut()
}

\details{
    Classification diagram, as proposed by \emph{Middlemost (1985)} for plutonic rocks.

    \if{html}{\figure{MiddlemostPlut.png}} 
    \if{latex}{\figure{MiddlemostPlut.png}{options: width=4.5in}}
}

\value{
  \item{sheet}{list with Figaro Style Sheet data}
  \item{x.data}{SiO2 weight percent}
  \item{y.data}{Na2O+K2O weight percent}
  \item{results}{matrix with classification results}
  \item{groups}{vector with classification results}
  \item{grouping}{set to -1}
}

\references{
  Middlemost E A K (1985) Magmas and Magmatic Rocks. Longman, London
}

\author{
  Vojtech Erban, \email{vojtech.erban@geology.cz}\cr 
  & Vojtech Janousek, \email{vojtech.janousek@geology.cz}
}

\seealso{
\link{classify}
\link{figaro}
\link{plotDiagram} 
}

\examples{
#Within GCDkit, the plot is called using following auxiliary functions:
#To classify data stored in WR (Groups by diagram)
classify("MiddlemostPlut")

#To plot data stored in WR or its subset (menu Classification)
plotDiagram("MiddlemostPlut", FALSE)
}

\keyword{hplot}
\concept{Plot}
\concept{Grouping}
\concept{Edit}
\concept{Menu: Data handling}
\concept{Menu: Classification}
