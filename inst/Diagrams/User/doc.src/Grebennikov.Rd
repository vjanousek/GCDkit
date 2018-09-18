\encoding{utf8}
\name{Grebennikov}
\alias{Grebennikov}
\title{Grebennikov (2014) 5Fe2O3t - Na2O+K2O - 5(CaO+MgO)}

\description{
    Assigns data for 
    \eqn{5Fe_{2}O_{3}t - Na_{2}O + K_{2}O - 5(CaO+MgO)}{5Fe2O3t - Na2O+K2O - 5(CaO+MgO)}  
    ternary diagram into Figaro template (list '\code{sheet}') 
    and appropriate values into '\code{x.data}' and '\code{y.data}'.
}

\usage{
Grebennikov()
}

\arguments{
None.
}

\details{
    The ternary plot 
    \eqn{5Fe_{2}O_{3}t - Na_{2}O + K_{2}O - 5(CaO+MgO)}{5Fe2O3t - Na2O+K2O - 5(CaO+MgO)}
    of \emph{Grebennikov (2014)} 
    serves for classification of A-type granites and
    related felsic volcanic rocks (\eqn{SiO_{2}}{SiO2} > 67 wt.\%). 
    
    \if{html}{\figure{Grebennikov.png}}
    \if{latex}{\figure{Grebennikov.png}{options: width=4.5in}}
}

\value{
  \item{sheet}{list with Figaro Style Sheet data}
  \item{x.data}{x coordinates}
  \item{y.data}{y coordinates}
}

\references{
    Grebennikov AV (2014) A-type granites and related rocks: petrogenesis and classification. 
    Russ Geol Geophys 55: 1353-1366
    \href{http://dx.doi.org/10.1016/j.rgg.2014.08.003}{doi: 10.1016/j.rgg.2014.08.003} 
}

\author{
    Vojtech Janousek, \email{vojtech.janousek@geology.cz}
}

\examples{
# plot the diagram
# assuming a dataset is loaded, of course!
\dontrun{ 
plotDiagram("Grebennikov",FALSE,TRUE)
}
}

\seealso{
\link{figaro} \link{plotDiagram}
}

\keyword{hplot} 
\concept{Geotectonic} 
\concept{Plot} 
\concept{Menu: Geotectonic}
