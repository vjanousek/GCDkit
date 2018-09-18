\name{YbN vs. LaN/YbN}
\title{YbN vs LaN/YbN (Martin 1986) TTG/adakite}
\alias{LaYb}

\description{
    Assigns data for the \eqn{Yb_{N}}{YbN}
    vs. \eqn{La_{N}/Yb_{N}}{LaN/YbN} diagram  for adakite/TTG
    discrimination into Figaro template (list '\code{sheet}') and
    appropriate values into '\code{x.data}' and '\code{y.data}'. 
}

\usage{
LaYb(ybrep=FALSE)
}

\arguments{
\item{ybrep}{logical, should be missing Yb values replaced by Y/2.4?}
}

\details{
    Diagram proposed by several authors for discriminating between adakitic (or TTG) 
    and "ordinary" calc-alkaline rocks. The version used here is from \emph{Martin (1986)}.

    \emph{Martin (1999)} suggested that Yb (ppm) could be replaced by 
    Y (ppm)/2.4. Notionally this could help with old data with missing values. 
    However if Yb is missing La is also likely to be absent (or unreliable), so by default this 
    replacement is not done; the user can access it by calling the function with \code{ybrep=TRUE} 
    (not available from GUI).

    Normalization values are La = 0.33 ppm, Yb = 0.22 ppm (\emph{Nakamura 1974}).
}

\value{
  \item{sheet}{list with Figaro Style Sheet data}
  \item{x.data}{\eqn{La_{N}/Yb_{N}}{LaN/YbN} or \eqn{La/(Y/2.4)}{La/(Y/2.4)}}
  \item{y.data}{\eqn{Yb_{N}}{YbN}}
}

\references{
    Martin H (1986) Effect of steeper Archean geothermal gradient on geochemistry of subduction-zone magmas. 
    Geology 14: 753-756
	\href{http://dx.doi.org/10.1130/0091-7613(1986)14<753:EOSAGG>2.0.CO;2}{doi: 10.1130/0091-7613(1986)14<753:EOSAGG>2.0.CO;2}

    Martin H (1999) Adakitic magmas: modern analogues of Archaean granitoids. 
    Lithos 46: 411-429
    \href{http://dx.doi.org/10.1016/S0024-4937(98)00076-0}{doi: 10.1016/S0024-4937(98)00076-0}

    Nakamura N (1974) Determination of REE, Ba, Fe, Mg, Na and K in carbonaceous and ordinary chondrites. 
    Geochim Cosmochim Acta 38: 757-775
    \href{http://dx.doi.org/10.1016/0016-7037(74)90149-5}{doi: 10.1016/0016-7037(74)90149-5}
}

\author{Jean-Francois Moyen, \email{jfmoyen@gmail.com}}

\examples{
    # plot the diagram
    plotDiagram("LaYb",FALSE)
}

\seealso{
    \link{figaro}
    \link{plotDiagram}
}

\keyword{hplot}
\concept{Menu: Geotectonic}
\concept{Geotectonic}
\concept{Plot}
