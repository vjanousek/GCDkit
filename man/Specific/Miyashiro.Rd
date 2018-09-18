\name{Miyashiro}
\alias{Miyashiro}
\title{SiO2-FeOt/MgO diagram (Miyashiro 1974)}

\description{
Assigns data for \eqn{SiO_{2}}{SiO2} vs. \eqn{FeO_{t}/MgO}{FeOt/MgO} diagram 
into Figaro template (list '\code{sheet}') and appropriate values into 
'\code{x.data}' and '\code{y.data}'
}

\usage{
Miyashiro()
}

\details{
Diagram in \eqn{SiO_{2}}{SiO2} vs. \eqn{FeO_{t}/MgO}{FeOt/MgO} space, proposed 
by \emph{Miyashiro (1974)}, defines the following fields:

\emph{Tholeiite Series}\cr
\emph{Calc-alkaline Series}

\if{html}{\figure{Miyashiro.png}} 
\if{latex}{\figure{Miyashiro.png}{options: width=4.5in}}

As the boundary was defined by Akiho Miyashiro as straight line passing through 
two specific points, no limits of diagram validity for ultrabasic and 
high-silica rocks were given. Thus, the boundary implemented in GCDkit script 
spreads from \eqn{FeO_{t}/MgO=0}{FeOt/MgO=0} to \eqn{SiO_{2}=100 \%}{SiO2=100 \%}.
}

\value{
  \item{sheet}{list with Figaro Style Sheet data}
  \item{x.data}{SiO2 weight percent}
  \item{y.data}{FeOt/MgO weight percent}
}


\references{
Miyashiro A (1974) Volcanic rock series in island arcs and active continental 
margins. Am J Sci 274, 321-355.
\href{http://dx.doi.org/10.2475/ajs.274.4.321}{doi: 10.2475/ajs.274.4.321} 
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
#To Classify data stored in WR (Groups by diagram)
classify("Miyashiro")

#To plot data stored in WR or its subset (menu Classification)
plotDiagram("Miyashiro", FALSE)
}


\keyword{hplot}
\concept{Plot}
\concept{Grouping}
\concept{Edit}
\concept{Menu: Data handling}
\concept{Menu: Classification}
