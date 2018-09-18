\name{Jensen}
\title{Jensen cation plot (1976)}
\alias{Jensen}

\description{
Assigns data for Jensen's cation plot into Figaro template (list '\code{sheet}') 
and appropriate values into '\code{x.data}' and '\code{y.data}'.  
}

\usage{
Jensen()
}

\details{
Jensen's cation plot, proposed by \emph{Jensen (1976)} and modified by 
\emph{Jensen & Pyke (1982)}. The triangular diagram is defined on the basis of 
millications as follows:

left apex: Al

upper apex: \eqn{Fe^T+Ti}{FeT+Ti}

right apex: Mg

\if{html}{\figure{Jensen.png}} 
\if{latex}{\figure{Jensen.png}{options: width=4.5in}}

The diagram defines following rock series and names:

\tabular{ll}{
\emph{Komatiite series (KOMATIITE)} \tab \emph{Komatiite}\cr
 \tab \emph{Komatiitic basalt}\cr
\emph{Tholeiite series (TH)} \tab \emph{Rhyolite}\cr
 \tab \emph{Dacite}\cr
 \tab \emph{Andesite}\cr
 \tab \emph{High-Fe tholeiite basalt}\cr
 \tab \emph{High-Mg tholeiite basalt}\cr
\emph{Calc-alkaline series (CA)} \tab \emph{Rhyolite}\cr
 \tab \emph{Dacite}\cr
 \tab \emph{Andesite}\cr
 \tab \emph{Basalt}\cr
}
}

\value{
  \item{x.data, y.data}{Values for the three apices transformed into 2D space}
  \item{sheet}{list with Figaro Style Sheet data}
}

\references{
Grunsky E C (1981) An algorithm for the classification of subalkalic volcanic 
rocks using the Jensen cation plot. In: Wood J, White O L, Barlow R B, 
Colvine A C (eds). Ontario Geological Survey, Misc Pap 100, pp 61-65

Jensen L S (1976) A new cation plot for classifying subalkalic volcanic rocks. 
Ont Div Mines, Misc Pap  66, 1-21

Jensen L S & Pyke D R (1982) Komatiites in the Ontario portion of the Abitibi 
belt. In: Arndt N T & Nisbet E G (eds) Komatiites. Allen & Unwin, London
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
#plot the diagram
plotDiagram("Jensen", FALSE)
}

\keyword{hplot}
\concept{Plot}
\concept{Classification}
\concept{Menu: Classification}
\concept{Menu: Data handling}
